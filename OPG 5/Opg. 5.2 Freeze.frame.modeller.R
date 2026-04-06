# =========================================================
# Model for bedre placeret medspiller
# Mænd og kvinder
# =========================================================
# Inden man kører det her, så er der en dataframe/funktion der hedder ff_men_1
# Den bliver først kører igennem i "Opg. 5.2 Freeze Frames Cases. 4 styks.R"

source("util.R")

library(dplyr)
library(ggplot2)
library(scales)

# =========================================================
# DEL 1: MODEL FOR MÆND
# =========================================================

# ---------------------------------------------------------
# 1. Shot-datasæt
# ---------------------------------------------------------

ShotsMen <- EventsMen %>%
  filter(type.name == "Shot")


# ---------------------------------------------------------
# 2. Geometri
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

count_opponents_in_triangle <- function(
    ff, attack_x, attack_y,
    left_post = c(120, 36),
    right_post = c(120, 44)
) {
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


# ---------------------------------------------------------
# 3. Hjælpefunktioner
# ---------------------------------------------------------

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


# ---------------------------------------------------------
# 4. Model for ét skud
# ---------------------------------------------------------

evaluate_shot_realistic <- function(
    shot_row,
    max_extra_goal_distance = 8,
    max_pass_distance = 20
) {
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
  
  # Realistisk kandidat:
  # 1) onside
  # 2) færre modspillere i trekanten end skytten
  # 3) ikke meget længere fra mål end skytten
  # 4) ikke for langt væk fra skytten
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
  
  # Vælg bedste realistiske kandidat:
  # først færrest blockers, derefter kortest afstand til mål
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
# 5. Kør modellen på mændenes skud
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
      shot_id = shot_row$id,
      matchId = shot_row$matchId,
      shooter_name = shot_row$player.name,
      shot_outcome = shot_row$shot.outcome.name,
      shot_xg = shot_row$shot.statsbomb_xg,
      shooter_x = shot_row$location[[1]][1],
      shooter_y = shot_row$location[[1]][2],
      under_pressure = shot_row$under_pressure
    ) %>%
      bind_cols(shot_eval)
  })
)


# ---------------------------------------------------------
# 6. Tjek resultater for mænd
# ---------------------------------------------------------

head(men_results_realistic)

table(men_results_realistic$better_placed_teammate_exists, useNA = "ifany")

mean(men_results_realistic$better_placed_teammate_exists, na.rm = TRUE)
mean(men_results_realistic$better_placed_teammate_exists, na.rm = TRUE)


# =========================================================
# DEL 2: MODEL FOR KVINDER
# =========================================================

# ---------------------------------------------------------
# 1. Shot-datasæt
# ---------------------------------------------------------

ShotsWomen <- EventsWomen %>%
  filter(type.name == "Shot")


# ---------------------------------------------------------
# 2. Geometri
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

count_opponents_in_triangle <- function(
    ff, attack_x, attack_y,
    left_post = c(120, 36),
    right_post = c(120, 44)
) {
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


# ---------------------------------------------------------
# 3. Hjælpefunktioner
# ---------------------------------------------------------

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


# ---------------------------------------------------------
# 4. Model for ét skud
# ---------------------------------------------------------

evaluate_shot_realistic <- function(
    shot_row,
    max_extra_goal_distance = 8,
    max_pass_distance = 20
) {
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
# 5. Kør modellen på kvindernes skud
# ---------------------------------------------------------

women_results_realistic <- bind_rows(
  lapply(seq_len(nrow(ShotsWomen)), function(i) {
    shot_row <- ShotsWomen[i, , drop = FALSE]
    
    shot_eval <- evaluate_shot_realistic(
      shot_row = shot_row,
      max_extra_goal_distance = 8,
      max_pass_distance = 20
    )
    
    under_pressure_value <- if (
      "under_pressure" %in% names(shot_row) &&
      length(shot_row$under_pressure) > 0
    ) {
      shot_row$under_pressure[1]
    } else {
      NA
    }
    
    data.frame(
      shot_id = i,
      match_id = shot_row$match_id[1],
      shooter_name = shot_row$player.name[1],
      shot_outcome = shot_row$shot.outcome.name[1],
      shot_xg = shot_row$shot.statsbomb_xg[1],
      shooter_x = shot_row$location[[1]][1],
      shooter_y = shot_row$location[[1]][2],
      under_pressure = under_pressure_value
    ) %>%
      bind_cols(shot_eval)
  })
)


# ---------------------------------------------------------
# 6. Tjek resultater for kvinder
# ---------------------------------------------------------

head(women_results_realistic)

table(women_results_realistic$better_placed_teammate_exists, useNA = "ifany")

mean(women_results_realistic$better_placed_teammate_exists, na.rm = TRUE)
table(women_results_realistic$better_placed_teammate_exists, useNA = "ifany")
mean(women_results_realistic$better_placed_teammate_exists, na.rm = TRUE)


# =========================================================
# DEL 3: Plot af samlet forskel
# =========================================================

overall_plot_data <- data.frame(
  Gruppe = c("Kvinder", "Mænd"),
  Andel = c(
    mean(women_results_realistic$better_placed_teammate_exists, na.rm = TRUE),
    mean(men_results_realistic$better_placed_teammate_exists, na.rm = TRUE)
  )
)

ggplot(overall_plot_data, aes(x = Gruppe, y = Andel, fill = Gruppe)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(
    aes(label = percent(Andel, accuracy = 0.1)),
    vjust = -0.6,
    size = 6,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Kvinder" = "#EB6A5B",
      "Mænd" = "#17A2A6"
    )
  ) +
  scale_y_continuous(
    labels = percent,
    limits = c(0, 0.7),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Andel af skud hvor en bedre afleveringsmulighed eksisterer",
    subtitle = "Andel af skud hvor en bedre placeret medspiller eksisterer",
    y = NULL,
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )