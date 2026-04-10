######
# =========================================================
# setup_freezeframe.R
# Genskaber alle centrale datasæt til freeze-frame analysen
# =========================================================

#source("util.R")
load("fodbold_data.RData")

library(dplyr)
library(ggplot2)
library(scales)

# ---------------------------------------------------------
# 1. Hent kampe
# ---------------------------------------------------------
#AllMatches <- getAllMatches()

# Kvinder: UEFA Euro 2022
#MatchesWomen <- AllMatches %>%
#  filter(competition.competition_id == 53)

#MatchesWomen2022 <- MatchesWomen %>%
#  filter(match_date >= as.Date("2022-07-06"))

#Match_Id_Women <- MatchesWomen2022$match_id

# Mænd: UEFA Euro 2024
#MatchesMen <- AllMatches %>%
#  filter(competition.competition_id == 55)

#MatchesMen2024 <- MatchesMen %>%
#  filter(match_date >= as.Date("2024-06-14"))

#Match_Id_Men <- MatchesMen2024$match_id

# ---------------------------------------------------------
# 2. Hent eventdata
# ---------------------------------------------------------
#EventsWomen <- getAllEventsMultipleMatches(Match_Id_Women, gender = "f")
#EventsMen   <- getAllEventsMultipleMatches(Match_Id_Men,   gender = "m")

# ---------------------------------------------------------
# 3. Lav shot-datasæt
# ---------------------------------------------------------
ShotsMen <- EventsMen %>%
  filter(type.name == "Shot")

ShotsWomen <- EventsWomen %>%
  filter(type.name == "Shot")

# ---------------------------------------------------------
# 4. Geometri og hjælpefunktioner
# ---------------------------------------------------------
point_in_triangle <- function(px, py, ax, ay, bx, by, cx, cy) {
  denominator <- ((by - cy) * (ax - cx) + (cx - bx) * (ay - cy))
  
  if (denominator == 0) {
    return(FALSE)
  }
  
  a <- ((by - cy) * (px - cx) + (cx - bx) * (py - cy)) / denominator
  b <- ((cy - ay) * (px - cx) + (ax - cx) * (py - cy)) / denominator
  c <- 1 - a - b
  
  a >= 0 & b >= 0 & c >= 0
}

count_opponents_in_triangle <- function(ff, attack_x, attack_y,
                                        left_post = c(120, 36),
                                        right_post = c(120, 44)) {
  if (is.null(ff) || nrow(ff) == 0) {
    return(NA_integer_)
  }
  
  ff$x <- sapply(ff$location, `[`, 1)
  ff$y <- sapply(ff$location, `[`, 2)
  
  ff$in_triangle <- mapply(
    point_in_triangle,
    px = ff$x,
    py = ff$y,
    MoreArgs = list(
      ax = attack_x,
      ay = attack_y,
      bx = left_post[1],
      by = left_post[2],
      cx = right_post[1],
      cy = right_post[2]
    )
  )
  
  sum(ff$teammate == FALSE & ff$in_triangle == TRUE, na.rm = TRUE)
}

distance_to_goal_center <- function(x, y, goal_x = 120, goal_y = 40) {
  sqrt((goal_x - x)^2 + (goal_y - y)^2)
}

distance_between_players <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

get_second_last_defender_x <- function(ff) {
  if (is.null(ff) || nrow(ff) == 0) {
    return(NA_real_)
  }
  
  opponents <- ff[ff$teammate == FALSE, , drop = FALSE]
  
  if (nrow(opponents) < 2) {
    return(NA_real_)
  }
  
  opponents$x <- sapply(opponents$location, `[`, 1)
  opp_x_sorted <- sort(opponents$x, decreasing = TRUE)
  
  opp_x_sorted[2]
}

is_onside_approx <- function(mate_x, shooter_x, second_last_defender_x) {
  if (is.na(mate_x) || is.na(shooter_x)) {
    return(NA)
  }
  
  if (is.na(second_last_defender_x)) {
    return(mate_x <= shooter_x)
  }
  
  (mate_x <= shooter_x) || (mate_x <= second_last_defender_x)
}

evaluate_shot_realistic <- function(shot_row,
                                    max_extra_goal_distance = 8,
                                    max_pass_distance = 20) {
  ff <- shot_row$shot.freeze_frame[[1]]
  
  if (is.null(ff) || nrow(ff) == 0) {
    return(data.frame(
      shooter_blockers = NA_integer_,
      shooter_goal_distance = NA_real_,
      second_last_defender_x = NA_real_,
      best_teammate_name = NA_character_,
      best_teammate_blockers = NA_integer_,
      best_teammate_goal_distance = NA_real_,
      best_teammate_pass_distance = NA_real_,
      best_teammate_x = NA_real_,
      best_teammate_y = NA_real_,
      onside_approx = NA,
      realistic_candidate_exists = NA,
      better_placed_teammate_exists = NA
    ))
  }
  
  shooter_loc <- shot_row$location[[1]]
  shooter_x <- shooter_loc[1]
  shooter_y <- shooter_loc[2]
  
  shooter_blockers <- count_opponents_in_triangle(
    ff = ff,
    attack_x = shooter_x,
    attack_y = shooter_y
  )
  
  shooter_goal_distance <- distance_to_goal_center(shooter_x, shooter_y)
  second_last_defender_x <- get_second_last_defender_x(ff)
  
  teammates <- ff[ff$teammate == TRUE, , drop = FALSE]
  
  if (nrow(teammates) == 0) {
    return(data.frame(
      shooter_blockers = shooter_blockers,
      shooter_goal_distance = shooter_goal_distance,
      second_last_defender_x = second_last_defender_x,
      best_teammate_name = NA_character_,
      best_teammate_blockers = NA_integer_,
      best_teammate_goal_distance = NA_real_,
      best_teammate_pass_distance = NA_real_,
      best_teammate_x = NA_real_,
      best_teammate_y = NA_real_,
      onside_approx = NA,
      realistic_candidate_exists = FALSE,
      better_placed_teammate_exists = FALSE
    ))
  }
  
  teammates$x <- sapply(teammates$location, `[`, 1)
  teammates$y <- sapply(teammates$location, `[`, 2)
  
  teammates$blockers <- sapply(seq_len(nrow(teammates)), function(i) {
    count_opponents_in_triangle(
      ff = ff,
      attack_x = teammates$x[i],
      attack_y = teammates$y[i]
    )
  })
  
  teammates$onside_approx <- sapply(seq_len(nrow(teammates)), function(i) {
    is_onside_approx(
      mate_x = teammates$x[i],
      shooter_x = shooter_x,
      second_last_defender_x = second_last_defender_x
    )
  })
  
  teammates$goal_distance <- mapply(
    distance_to_goal_center,
    teammates$x,
    teammates$y
  )
  
  teammates$pass_distance <- mapply(
    distance_between_players,
    shooter_x,
    shooter_y,
    teammates$x,
    teammates$y
  )
  
  teammates$realistic_candidate <- (
    teammates$onside_approx == TRUE &
      teammates$blockers < shooter_blockers &
      teammates$goal_distance <= shooter_goal_distance + max_extra_goal_distance &
      teammates$pass_distance <= max_pass_distance
  )
  
  realistic_idx <- which(teammates$realistic_candidate)
  
  if (length(realistic_idx) == 0) {
    return(data.frame(
      shooter_blockers = shooter_blockers,
      shooter_goal_distance = shooter_goal_distance,
      second_last_defender_x = second_last_defender_x,
      best_teammate_name = NA_character_,
      best_teammate_blockers = NA_integer_,
      best_teammate_goal_distance = NA_real_,
      best_teammate_pass_distance = NA_real_,
      best_teammate_x = NA_real_,
      best_teammate_y = NA_real_,
      onside_approx = NA,
      realistic_candidate_exists = FALSE,
      better_placed_teammate_exists = FALSE
    ))
  }
  
  candidate_df <- teammates[realistic_idx, , drop = FALSE] %>%
    arrange(blockers, goal_distance)
  
  best_row <- candidate_df[1, , drop = FALSE]
  
  data.frame(
    shooter_blockers = shooter_blockers,
    shooter_goal_distance = shooter_goal_distance,
    second_last_defender_x = second_last_defender_x,
    best_teammate_name = best_row$player$name[1],
    best_teammate_blockers = best_row$blockers[1],
    best_teammate_goal_distance = best_row$goal_distance[1],
    best_teammate_pass_distance = best_row$pass_distance[1],
    best_teammate_x = best_row$x[1],
    best_teammate_y = best_row$y[1],
    onside_approx = best_row$onside_approx[1],
    realistic_candidate_exists = TRUE,
    better_placed_teammate_exists = TRUE
  )
}

# ---------------------------------------------------------
# 5. Hjælpefunktion til at bygge resultattabeller
# ---------------------------------------------------------
safe_under_pressure <- function(shot_row) {
  if ("under_pressure" %in% names(shot_row) && length(shot_row$under_pressure) > 0) {
    return(shot_row$under_pressure[1])
  }
  NA
}

# ---------------------------------------------------------
# 6. Kør modellen på mænd
# ---------------------------------------------------------
men_results_realistic <- bind_rows(
  lapply(seq_len(nrow(ShotsMen)), function(i) {
    shot_row <- ShotsMen[i, , drop = FALSE]
    
    shot_eval <- evaluate_shot_realistic(
      shot_row = shot_row,
      max_extra_goal_distance = 8,
      max_pass_distance = 20
    )
    
    data.frame(
      shot_id = shot_row$id[1],
      matchId = shot_row$matchId[1],
      shooter_name = shot_row$player.name[1],
      shot_outcome = shot_row$shot.outcome.name[1],
      shot_xg = shot_row$shot.statsbomb_xg[1],
      shooter_x = shot_row$location[[1]][1],
      shooter_y = shot_row$location[[1]][2],
      under_pressure = safe_under_pressure(shot_row)
    ) %>%
      bind_cols(shot_eval)
  })
)

# ---------------------------------------------------------
# 7. Kør modellen på kvinder
# ---------------------------------------------------------
women_results_realistic <- bind_rows(
  lapply(seq_len(nrow(ShotsWomen)), function(i) {
    shot_row <- ShotsWomen[i, , drop = FALSE]
    
    shot_eval <- evaluate_shot_realistic(
      shot_row = shot_row,
      max_extra_goal_distance = 8,
      max_pass_distance = 20
    )
    
    data.frame(
      shot_id = i,
      match_id = shot_row$match_id[1],
      shooter_name = shot_row$player.name[1],
      shot_outcome = shot_row$shot.outcome.name[1],
      shot_xg = shot_row$shot.statsbomb_xg[1],
      shooter_x = shot_row$location[[1]][1],
      shooter_y = shot_row$location[[1]][2],
      under_pressure = safe_under_pressure(shot_row)
    ) %>%
      bind_cols(shot_eval)
  })
)

# ---------------------------------------------------------
# 8. Data til overall-plot
# ---------------------------------------------------------
overall_plot_data <- data.frame(
  Gruppe = c("Kvinder", "Mænd"),
  Andel = c(
    mean(women_results_realistic$better_placed_teammate_exists, na.rm = TRUE),
    mean(men_results_realistic$better_placed_teammate_exists, na.rm = TRUE)
  )
)

# ---------------------------------------------------------
# 9. Hurtige checks
# ---------------------------------------------------------
cat("Mænd - andel:\n")
print(mean(men_results_realistic$better_placed_teammate_exists, na.rm = TRUE))

cat("Kvinder - andel:\n")
print(mean(women_results_realistic$better_placed_teammate_exists, na.rm = TRUE))

# ---------------------------------------------------------
# 10. Gem som RDS, så du ikke mister det igen
# ---------------------------------------------------------
saveRDS(EventsMen, "EventsMen.rds")
saveRDS(EventsWomen, "EventsWomen.rds")
saveRDS(ShotsMen, "ShotsMen.rds")
saveRDS(ShotsWomen, "ShotsWomen.rds")
saveRDS(men_results_realistic, "men_results_realistic.rds")
saveRDS(women_results_realistic, "women_results_realistic.rds")
saveRDS(overall_plot_data, "overall_plot_data.rds")

cat("Alle centrale objekter er nu genskabt og gemt som .rds\n")
