### Shiny App, den færdige version
### Load data
load("fodbold_data.RData")


library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(countrycode)
library(ggiraph)
library(bslib)

# =========================================================
# FORUDSÆTNINGER
# =========================================================
# Følgende objekter skal allerede ligge i environment:
#
# ShotsMen
# ShotsWomen
# men_results_realistic
# women_results_realistic
# EventsMen
# EventsWomen
# =========================================================


# =========================================================
# 1. TEKST- OG LABEL-FUNKTIONER
# =========================================================

translate_shot_type <- function(x) {
  dplyr::recode(
    x,
    "Open Play" = "Åbent spil",
    "Penalty"   = "Straffespark",
    "Free Kick" = "Frispark",
    "Corner"    = "Hjørnespark",
    .default = x
  )
}

translate_play_pattern <- function(x) {
  dplyr::recode(
    x,
    "Regular Play"   = "Ordinært spil",
    "From Throw In"  = "Efter indkast",
    "From Free Kick" = "Efter frispark",
    "From Corner"    = "Efter hjørnespark",
    "From Counter"   = "Efter kontra",
    "From Keeper"    = "Efter målmandsaktion",
    "From Kick Off"  = "Efter kickoff",
    "From Goal Kick" = "Efter målspark",
    "Other"          = "Andet",
    .default = x
  )
}

translate_shot_outcome <- function(x) {
  dplyr::recode(
    x,
    "Goal" = "Mål",
    "Saved" = "Reddet",
    "Blocked" = "Blokeret",
    "Wayward" = "Forbi mål",
    "Off T" = "Uden for mål",
    "Off Target" = "Uden for mål",
    "Post" = "På stolpe",
    "Saved Off Target" = "Reddet, uden for mål",
    "Saved to Post" = "Reddet til stolpen",
    .default = x
  )
}

label_better_option <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "Ukendt",
    x ~ "Bedre afleveringsmulighed",
    TRUE ~ "Ingen bedre afleveringsmulighed"
  )
}

label_decision_outcome <- function(better_exists, shot_outcome) {
  dplyr::case_when(
    is.na(better_exists) | is.na(shot_outcome) ~ "Ukendt",
    better_exists & shot_outcome == "Goal" ~ "Bedre mulighed + mål",
    better_exists & shot_outcome != "Goal" ~ "Bedre mulighed + ikke mål",
    !better_exists & shot_outcome == "Goal" ~ "Ingen bedre mulighed + mål",
    !better_exists & shot_outcome != "Goal" ~ "Ingen bedre mulighed + ikke mål",
    TRUE ~ "Ukendt"
  )
}


# =========================================================
# 2. HOLDNAVNE, FLAG OG LABELS
# =========================================================

clean_team_name <- function(team_name) {
  dplyr::case_when(
    team_name == "Austria Women's" ~ "Austria",
    team_name == "England Women's" ~ "England",
    team_name == "Norway Women's" ~ "Norway",
    team_name == "Northern Ireland" ~ "Northern Ireland",
    team_name == "WNT Finland" ~ "Finland",
    team_name == "Spain Women's" ~ "Spain",
    team_name == "Germany Women's" ~ "Germany",
    team_name == "Denmark Women's" ~ "Denmark",
    team_name == "Switzerland Women's" ~ "Switzerland",
    team_name == "Portugal Women's" ~ "Portugal",
    team_name == "Sweden Women's" ~ "Sweden",
    team_name == "Netherlands Women's" ~ "Netherlands",
    team_name == "France Women's" ~ "France",
    team_name == "Italy Women's" ~ "Italy",
    team_name == "Belgium Women's" ~ "Belgium",
    team_name == "Iceland Women's" ~ "Iceland",
    TRUE ~ team_name
  )
}

team_to_iso2 <- function(team_name_clean) {
  dplyr::case_when(
    team_name_clean == "Germany" ~ "DE",
    team_name_clean == "Scotland" ~ "GB",
    team_name_clean == "Hungary" ~ "HU",
    team_name_clean == "Switzerland" ~ "CH",
    team_name_clean == "Spain" ~ "ES",
    team_name_clean == "Croatia" ~ "HR",
    team_name_clean == "Albania" ~ "AL",
    team_name_clean == "Italy" ~ "IT",
    team_name_clean == "Denmark" ~ "DK",
    team_name_clean == "Slovenia" ~ "SI",
    team_name_clean == "England" ~ "GB",
    team_name_clean == "Serbia" ~ "RS",
    team_name_clean == "Belgium" ~ "BE",
    team_name_clean == "Slovakia" ~ "SK",
    team_name_clean == "France" ~ "FR",
    team_name_clean == "Austria" ~ "AT",
    team_name_clean == "Czech Republic" ~ "CZ",
    team_name_clean == "Portugal" ~ "PT",
    team_name_clean == "Netherlands" ~ "NL",
    team_name_clean == "Turkey" ~ "TR",
    team_name_clean == "Romania" ~ "RO",
    team_name_clean == "Poland" ~ "PL",
    team_name_clean == "Ukraine" ~ "UA",
    team_name_clean == "Georgia" ~ "GE",
    team_name_clean == "Norway" ~ "NO",
    team_name_clean == "Northern Ireland" ~ "GB",
    team_name_clean == "Finland" ~ "FI",
    team_name_clean == "Sweden" ~ "SE",
    team_name_clean == "Iceland" ~ "IS",
    TRUE ~ NA_character_
  )
}

get_team_flag <- function(team_name) {
  clean_name <- clean_team_name(team_name)
  iso2 <- team_to_iso2(clean_name)
  
  flag <- countrycode::countrycode(
    iso2,
    origin = "iso2c",
    destination = "unicode.symbol"
  )
  
  ifelse(is.na(flag), "", flag)
}

get_team_label_with_flag <- function(team_name) {
  clean_name <- clean_team_name(team_name)
  flag <- get_team_flag(team_name)
  
  ifelse(flag == "", clean_name, paste(flag, clean_name))
}


# =========================================================
# 3. UI-HJÆLPERE
# =========================================================

make_value_box <- function(title, value, color = "#ffffff") {
  div(
    class = "value-box",
    style = paste0("background:", color, ";"),
    tags$div(class = "value-box-title", title),
    tags$div(class = "value-box-value", value)
  )
}

make_stats_table <- function(df, title, value_col, digits = NULL) {
  div(
    class = "meta-card stats-card",
    tags$h4(title),
    if (nrow(df) == 0) {
      tags$p("Ingen data.")
    } else {
      tags$table(
        class = "table table-sm stats-table",
        tags$thead(
          tags$tr(
            tags$th("Spiller"),
            tags$th("Hold"),
            tags$th(value_col)
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            value <- df[[value_col]][i]
            if (!is.null(digits)) value <- round(value, digits)
            
            tags$tr(
              tags$td(df$player.name[i]),
              tags$td(get_team_label_with_flag(df$team.name[i])),
              tags$td(value)
            )
          })
        )
      )
    }
  )
}

make_ego_table <- function(df, title) {
  div(
    class = "meta-card stats-card",
    tags$h4(title),
    if (nrow(df) == 0) {
      tags$p("Ingen data.")
    } else {
      div(
        style = "overflow-x:auto;",
        tags$table(
          class = "table table-sm stats-table",
          tags$thead(
            tags$tr(
              tags$th("Spiller"),
              tags$th("Hold"),
              tags$th("Skud"),
              tags$th("Mål"),
              tags$th("xG"),
              tags$th("xG/skud"),
              tags$th("Bedre mulighed"),
              tags$th("EGO-skud"),
              tags$th("EGO-andel"),
              tags$th("Målrate")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(df)), function(i) {
              tags$tr(
                tags$td(df$shooter_name[i]),
                tags$td(get_team_label_with_flag(df$team.name[i])),
                tags$td(df$shots[i]),
                tags$td(df$goals[i]),
                tags$td(round(df$total_xg[i], 2)),
                tags$td(round(df$avg_xg_per_shot[i], 3)),
                tags$td(df$better_option_shots[i]),
                tags$td(df$ego_shots[i]),
                tags$td(percent(df$ego_share[i], accuracy = 0.1)),
                tags$td(percent(df$goal_rate[i], accuracy = 0.1))
              )
            })
          )
        )
      )
    }
  )
}

make_player_profile_card <- function(df, title) {
  div(
    class = "meta-card stats-card",
    tags$h4(title),
    if (nrow(df) == 0) {
      tags$p("Spilleren findes ikke i datasættet.")
    } else {
      tagList(
        fluidRow(
          column(4, make_value_box("Skud", df$shots[1], "#FFF4F4")),
          column(4, make_value_box("Mål", df$goals[1], "#FCE8E8")),
          column(4, make_value_box("Samlet xG", round(df$total_xg[1], 2), "#FFF8F8"))
        ),
        fluidRow(
          column(4, make_value_box("xG pr. skud", round(df$avg_xg_per_shot[1], 3), "#FFF8F8")),
          column(4, make_value_box("Bedre mulighed", df$better_option_shots[1], "#FDECEC")),
          column(4, make_value_box("EGO-skud", df$ego_shots[1], "#FDECEC"))
        ),
        fluidRow(
          column(6, make_value_box("EGO-andel", percent(df$ego_share[1], accuracy = 0.1), "#FFF4F4")),
          column(6, make_value_box("Målrate", percent(df$goal_rate[1], accuracy = 0.1), "#FCE8E8"))
        )
      )
    }
  )
}


# =========================================================
# 4. GENERELLE DATAFUNKTIONER
# =========================================================

prepare_shots <- function(df, league_label = c("Mænd", "Kvinder")) {
  league_label <- match.arg(league_label)
  
  if (league_label == "Mænd") {
    df %>%
      mutate(
        shot_id_std = as.character(id),
        match_id_std = as.character(matchId),
        shot_type_label = translate_shot_type(shot.type.name),
        play_pattern_label = translate_play_pattern(play_pattern.name),
        shot_outcome_label = translate_shot_outcome(shot.outcome.name),
        has_freeze = sapply(shot.freeze_frame, function(x) !is.null(x) && length(x) > 0),
        league = "Mænd"
      )
  } else {
    df %>%
      mutate(
        shot_row_index = row_number(),
        shot_id_std = as.character(shot_row_index),
        match_id_std = as.character(match_id),
        shot_type_label = translate_shot_type(shot.type.name),
        play_pattern_label = translate_play_pattern(play_pattern.name),
        shot_outcome_label = translate_shot_outcome(shot.outcome.name),
        has_freeze = sapply(shot.freeze_frame, function(x) !is.null(x) && length(x) > 0),
        league = "Kvinder"
      )
  }
}

prepare_results <- function(df, league_label = c("Mænd", "Kvinder")) {
  league_label <- match.arg(league_label)
  
  if (league_label == "Mænd") {
    df %>%
      mutate(
        shot_id = as.character(shot_id),
        shot_id_std = as.character(shot_id),
        match_id_std = as.character(matchId),
        shot_outcome_label = translate_shot_outcome(shot_outcome),
        league = "Mænd"
      )
  } else {
    df %>%
      mutate(
        shot_id = as.character(shot_id),
        shot_id_std = as.character(shot_id),
        match_id_std = as.character(match_id),
        shot_outcome_label = translate_shot_outcome(shot_outcome),
        league = "Kvinder"
      )
  }
}

build_joined_results <- function(results_df, shots_df, league_label = c("Mænd", "Kvinder")) {
  league_label <- match.arg(league_label)
  
  results_df %>%
    left_join(
      shots_df %>%
        select(
          shot_id_std, match_id_std, minute, second,
          team.name, player.name,
          shot.type.name, play_pattern.name,
          shot_type_label, play_pattern_label,
          has_freeze
        ),
      by = c("shot_id_std", "match_id_std")
    ) %>%
    mutate(
      league = league_label,
      team_label = get_team_label_with_flag(team.name),
      better_option_text = label_better_option(better_placed_teammate_exists),
      decision_outcome_text = label_decision_outcome(
        better_exists = better_placed_teammate_exists,
        shot_outcome = shot_outcome
      ),
      display = paste0(
        team_label, " | ",
        shooter_name, " | ",
        shot_outcome_label, " | ",
        shot_type_label, " | ",
        play_pattern_label, " | ",
        better_option_text
      )
    )
}

build_top_counts <- function(df, type = c("shots", "goals", "assists", "shot_assists"), n_top = 5) {
  type <- match.arg(type)
  
  if (type == "shots") {
    return(
      df %>%
        count(player.name, team.name, sort = TRUE) %>%
        slice_head(n = n_top)
    )
  }
  
  if (type == "goals") {
    return(
      df %>%
        filter(shot.outcome.name == "Goal") %>%
        count(player.name, team.name, sort = TRUE) %>%
        slice_head(n = n_top)
    )
  }
  
  if (type == "assists") {
    return(
      df %>%
        filter(pass.goal_assist %in% TRUE) %>%
        count(player.name, team.name, sort = TRUE) %>%
        slice_head(n = n_top)
    )
  }
  
  df %>%
    filter(pass.shot_assist %in% TRUE) %>%
    count(player.name, team.name, sort = TRUE) %>%
    slice_head(n = n_top)
}

build_top_xg <- function(shots_df, n_top = 5) {
  shots_df %>%
    group_by(player.name, team.name) %>%
    summarise(xG = sum(shot.statsbomb_xg, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(xG)) %>%
    slice_head(n = n_top)
}

build_player_stat_bundle <- function(shots_df, events_df) {
  list(
    top_scorers = build_top_counts(shots_df, "goals"),
    top_assists = build_top_counts(events_df, "assists"),
    top_shot_assists = build_top_counts(events_df, "shot_assists"),
    top_shots = build_top_counts(shots_df, "shots"),
    top_xg = build_top_xg(shots_df)
  )
}

build_dataset_summary <- function(shots_df, joined_df, match_col) {
  data.frame(
    matches = dplyr::n_distinct(shots_df[[match_col]]),
    shots = nrow(shots_df),
    goals = sum(shots_df$shot.outcome.name == "Goal", na.rm = TRUE),
    avg_xg = mean(shots_df$shot.statsbomb_xg, na.rm = TRUE),
    better_share = mean(joined_df$better_placed_teammate_exists, na.rm = TRUE)
  )
}

build_team_bundle <- function(shots_df, joined_df, events_df, team_name) {
  shots_team <- shots_df %>% filter(team.name == team_name)
  joined_team <- joined_df %>% filter(team.name == team_name)
  events_team <- events_df %>% filter(team.name == team_name)
  
  list(
    shots = shots_team,
    joined = joined_team,
    events = events_team,
    stats = build_player_stat_bundle(shots_team, events_team)
  )
}

build_ego_player_summary <- function(joined_df, min_shots = 5) {
  joined_df %>%
    mutate(
      ego_case = better_placed_teammate_exists == TRUE & shot_outcome != "Goal",
      is_goal = shot_outcome == "Goal",
      better_option_case = better_placed_teammate_exists == TRUE
    ) %>%
    group_by(shooter_name, team.name) %>%
    summarise(
      shots = n(),
      goals = sum(is_goal, na.rm = TRUE),
      total_xg = sum(shot_xg, na.rm = TRUE),
      avg_xg_per_shot = mean(shot_xg, na.rm = TRUE),
      better_option_shots = sum(better_option_case, na.rm = TRUE),
      better_option_share = mean(better_option_case, na.rm = TRUE),
      ego_shots = sum(ego_case, na.rm = TRUE),
      ego_share = mean(ego_case, na.rm = TRUE),
      goal_rate = mean(is_goal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(shots >= min_shots) %>%
    arrange(desc(ego_shots), desc(ego_share), desc(total_xg))
}


# =========================================================
# 5. FREEZE-FRAME HJÆLPERE
# =========================================================

extract_freeze_frame <- function(shot_row) {
  ff_raw <- shot_row$shot.freeze_frame[[1]]
  
  if (is.null(ff_raw) || length(ff_raw) == 0) return(NULL)
  if (is.null(ff_raw$location)) return(NULL)
  
  ff <- data.frame(
    teammate = ff_raw$teammate,
    stringsAsFactors = FALSE
  )
  
  ff$x <- sapply(ff_raw$location, function(z) if (length(z) >= 1) z[1] else NA_real_)
  ff$y <- sapply(ff_raw$location, function(z) if (length(z) >= 2) z[2] else NA_real_)
  
  if ("player.name" %in% names(ff_raw)) {
    ff$player_name <- ff_raw$player.name
  } else if ("player" %in% names(ff_raw) && is.data.frame(ff_raw$player)) {
    ff$player_name <- ff_raw$player$name
  } else {
    ff$player_name <- NA_character_
  }
  
  if ("position.name" %in% names(ff_raw)) {
    ff$position_name <- ff_raw$position.name
  } else if ("position" %in% names(ff_raw) && is.data.frame(ff_raw$position)) {
    ff$position_name <- ff_raw$position$name
  } else {
    ff$position_name <- NA_character_
  }
  
  ff <- ff %>% filter(!is.na(x), !is.na(y))
  if (nrow(ff) == 0) return(NULL)
  
  ff$label <- ff$player_name
  ff$is_goalkeeper <- grepl("Goalkeeper", ff$position_name)
  
  ff$role_label <- dplyr::case_when(
    ff$is_goalkeeper ~ "Målmand",
    ff$teammate ~ "Medspiller",
    TRUE ~ "Modspiller"
  )
  
  ff$data_id <- paste0("player_", seq_len(nrow(ff)))
  ff$tooltip <- paste0(
    "<b>", ff$player_name, "</b><br/>",
    ff$role_label,
    ifelse(!is.na(ff$position_name), paste0("<br/>Position: ", ff$position_name), "")
  )
  
  ff
}

build_case_plot <- function(shot_row, result_row, better_case = TRUE) {
  ff <- extract_freeze_frame(shot_row)
  req(!is.null(ff))
  
  shooter_loc <- shot_row$location[[1]]
  req(length(shooter_loc) >= 2)
  
  shooter_x <- shooter_loc[1]
  shooter_y <- shooter_loc[2]
  shooter_name <- shot_row$player.name[1]
  
  left_post <- c(120, 36)
  right_post <- c(120, 44)
  goal_center <- c(120, 40)
  
  # --- BANEELEMENTER ---
  
  penalty_box <- data.frame(xmin = 102, xmax = 120, ymin = 18, ymax = 62)
  six_yard_box <- data.frame(xmin = 114, xmax = 120, ymin = 30, ymax = 50)
  
  # Straffesparksbue
  penalty_arc <- data.frame(theta = seq(0, 2 * pi, length.out = 300)) %>%
    dplyr::mutate(
      x = 108 + 10 * cos(theta),
      y = 40 + 10 * sin(theta)
    ) %>%
    dplyr::filter(x <= 102)
  
  # --- LEGEND POSITION ---
  legend_x <- 120.5
  legend_y <- 58
  
  # --- LABEL DATA (VIGTIG FOR IKKE AT CRASHE) ---
  label_df <- ff %>%
    dplyr::filter(teammate == TRUE, !is_goalkeeper) %>%
    dplyr::mutate(label_y = y + 1.2)
  
  # --- BASE PLOT ---
  base_plot <- ggplot() +
    # Grøn bane
    geom_rect(aes(xmin = 0, xmax = 120, ymin = 0, ymax = 80),
              fill = "#2E8B57", color = NA) +
    
    # Straffesparksfelt
    geom_rect(data = penalty_box,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = NA, color = "white", linewidth = 0.9) +
    
    # Målfelt
    geom_rect(data = six_yard_box,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = NA, color = "white", linewidth = 0.9) +
    
    # Mål (bag linjen)
    geom_segment(aes(x = 120, y = 36, xend = 122, yend = 36),
                 color = "white", linewidth = 1.1) +
    geom_segment(aes(x = 120, y = 44, xend = 122, yend = 44),
                 color = "white", linewidth = 1.1) +
    geom_segment(aes(x = 122, y = 36, xend = 122, yend = 44),
                 color = "white", linewidth = 1.1) +
    
    # Straffesparksplet
    geom_point(aes(x = 108, y = 40), color = "white", size = 1.5) +
    
    # Straffesparksbue
    geom_path(data = penalty_arc,
              aes(x = x, y = y),
              color = "white",
              linewidth = 0.9) +
    
    # KILDE (nederst til venstre)
    annotate("text",
             x = 84,
             y = 22,
             label = "Data: StatsBomb Open Data",
             color = "white",
             size = 3,
             hjust = 0)
  
  player_layer <- geom_point_interactive(
    data = ff %>% filter(!is_goalkeeper),
    aes(
      x = x,
      y = y,
      fill = teammate,
      data_id = data_id,
      tooltip = tooltip
    ),
    shape = 21,
    color = "white",
    size = 4,
    stroke = 0.8,
    show.legend = FALSE
  )
  
  keeper_layer <- geom_point_interactive(
    data = ff %>% filter(is_goalkeeper),
    aes(
      x = x,
      y = y,
      data_id = data_id,
      tooltip = tooltip
    ),
    shape = 24,
    fill = "gold",
    color = "black",
    size = 5,
    stroke = 1
  )
  
  if (better_case && !is.na(result_row$best_teammate_name[1])) {
    best_x <- result_row$best_teammate_x[1]
    best_y <- result_row$best_teammate_y[1]
    best_name <- result_row$best_teammate_name[1]
    
    ff$label_clean <- ifelse(ff$player_name == best_name, "", ff$label)
    
    label_df <- ff %>%
      filter(teammate == TRUE, player_name != best_name, !is_goalkeeper) %>%
      mutate(label_y = y + 1.2)
    
    key_labels <- data.frame(
      label_x = c(shooter_x - 2.6, best_x - 1.8),
      label_y = c(shooter_y + 0.8, best_y + 3.0),
      label = c(shooter_name, best_name),
      stringsAsFactors = FALSE
    )
    
    p <- base_plot +
      geom_polygon(
        data = data.frame(
          x = c(shooter_x, left_post[1], right_post[1]),
          y = c(shooter_y, left_post[2], right_post[2])
        ),
        aes(x = x, y = y),
        fill = "#E30613", alpha = 0.20, color = "#E30613", linewidth = 0.8
      ) +
      geom_polygon(
        data = data.frame(
          x = c(best_x, left_post[1], right_post[1]),
          y = c(best_y, left_post[2], right_post[2])
        ),
        aes(x = x, y = y),
        fill = "#8B0000", alpha = 0.18, color = "#8B0000", linewidth = 0.8
      ) +
      geom_segment(
        aes(x = shooter_x, y = shooter_y, xend = goal_center[1], yend = goal_center[2]),
        color = "#E30613", linewidth = 2.0, alpha = 0.95
      ) +
      geom_segment(
        aes(x = best_x, y = best_y, xend = goal_center[1], yend = goal_center[2]),
        color = "#8B0000", linewidth = 2.0, alpha = 0.95
      ) +
      player_layer +
      keeper_layer +
      geom_text(
        data = ff %>% filter(is_goalkeeper),
        aes(x = x, y = y + 1.3, label = player_name),
        color = "black",
        size = 3
      ) +
      geom_point(aes(x = shooter_x, y = shooter_y), shape = 21, fill = "#E30613", color = "black", size = 6, stroke = 1.5) +
      geom_point(aes(x = best_x, y = best_y), shape = 21, fill = "#8B0000", color = "black", size = 6, stroke = 1.5) +
      geom_text(data = label_df, aes(x = x, y = label_y, label = label_clean), color = "black", size = 3, alpha = 0.85) +
      geom_label(
        data = key_labels,
        aes(x = label_x, y = label_y, label = label),
        fill = "white",
        color = "black",
        fontface = "bold",
        size = 3.8,
        linewidth = 0.25
      ) +
      geom_point(aes(x = legend_x, y = legend_y), shape = 21, fill = "#E30613", color = "black", size = 4) +
      geom_text(aes(x = legend_x + 0.6, y = legend_y), label = "Skytte", hjust = 0, size = 3.8) +
      geom_point(aes(x = legend_x, y = legend_y - 3), shape = 21, fill = "#8B0000", color = "black", size = 4) +
      geom_text(aes(x = legend_x + 0.6, y = legend_y - 3), label = "Bedre afleveringsmulighed", hjust = 0, size = 3.8) +
      geom_point(aes(x = legend_x, y = legend_y - 6), shape = 24, fill = "gold", color = "black", size = 4) +
      geom_text(aes(x = legend_x + 0.6, y = legend_y - 6), label = "Modstanderens målmand", hjust = 0, size = 3.8) +
      scale_fill_manual(values = c("TRUE" = "#E30613", "FALSE" = "#F28B82")) +
      coord_fixed(xlim = c(82, 135.5), ylim = c(20, 62), expand = FALSE) +
      labs(
        title = paste0("Case: ", shooter_name, " vs. ", best_name),
        subtitle = paste0(
          best_name, " har ", result_row$best_teammate_blockers[1],
          " modspillere i sin afslutningszone mod ",
          shooter_name, "s ", result_row$shooter_blockers[1], "."
        ),
        caption = "Rød = skytte. Mørkerød = bedre afleveringsmulighed. Gul = målmand."
      )
  } else {
    key_labels <- data.frame(
      label_x = shooter_x - 2.4,
      label_y = shooter_y + 1.0,
      label = shooter_name,
      stringsAsFactors = FALSE
    )
    
    p <- base_plot +
      geom_polygon(
        data = data.frame(
          x = c(shooter_x, left_post[1], right_post[1]),
          y = c(shooter_y, left_post[2], right_post[2])
        ),
        aes(x = x, y = y),
        fill = "#E30613", alpha = 0.20, color = "#E30613", linewidth = 0.8
      ) +
      geom_segment(
        aes(x = shooter_x, y = shooter_y, xend = goal_center[1], yend = goal_center[2]),
        color = "#E30613", linewidth = 2.0, alpha = 0.95
      ) +
      player_layer +
      keeper_layer +
      geom_text(
        data = ff %>% filter(is_goalkeeper),
        aes(x = x, y = y + 1.3, label = player_name),
        color = "black",
        size = 3
      ) +
      geom_point(aes(x = shooter_x, y = shooter_y), shape = 21, fill = "#E30613", color = "black", size = 6, stroke = 1.5) +
      geom_text(data = label_df, aes(x = x, y = label_y, label = label), color = "black", size = 3, alpha = 0.85) +
      geom_label(
        data = key_labels,
        aes(x = label_x, y = label_y, label = label),
        fill = "white",
        color = "black",
        fontface = "bold",
        size = 3.8,
        linewidth = 0.25
      ) +
      geom_point(aes(x = legend_x, y = legend_y), shape = 21, fill = "#E30613", color = "black", size = 4) +
      geom_text(aes(x = legend_x + 0.6, y = legend_y), label = "Skytte", hjust = 0, size = 3.8) +
      geom_point(aes(x = legend_x, y = legend_y - 3), shape = 24, fill = "gold", color = "black", size = 4) +
      geom_text(aes(x = legend_x + 0.6, y = legend_y - 3), label = "Modstanderens målmand", hjust = 0, size = 3.8) +
      scale_fill_manual(values = c("TRUE" = "#E30613", "FALSE" = "#F28B82")) +
      coord_fixed(xlim = c(82, 135.5), ylim = c(20, 62), expand = FALSE) +
      labs(
        title = paste0("Case: ", shooter_name),
        subtitle = "Ingen bedre afleveringsmulighed er identificeret i situationen.",
        caption = "Rød = skytte."
      )
  }
  
  p +
    theme_void(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#2E8B57", color = NA),
      plot.title = element_text(face = "bold", size = 18, color = "#1F1F1F"),
      plot.subtitle = element_text(size = 11, color = "#333333", margin = margin(b = 10)),
      plot.caption = element_text(size = 10, color = "#333333", hjust = 0),
      legend.position = "none",
      plot.margin = margin(15, 15, 15, 15)
    )
}


# =========================================================
# 6. FORBEREDELSE AF DATA
# =========================================================

shots_men_std <- prepare_shots(ShotsMen, "Mænd")
shots_women_std <- prepare_shots(ShotsWomen, "Kvinder")

results_men_std <- prepare_results(men_results_realistic, "Mænd")
results_women_std <- prepare_results(women_results_realistic, "Kvinder")

men_joined <- build_joined_results(results_men_std, shots_men_std, "Mænd")
women_joined <- build_joined_results(results_women_std, shots_women_std, "Kvinder")
all_results <- bind_rows(women_joined, men_joined)

men_stats <- build_player_stat_bundle(ShotsMen, EventsMen)
women_stats <- build_player_stat_bundle(ShotsWomen, EventsWomen)

denmark_men <- build_team_bundle(
  shots_df = ShotsMen,
  joined_df = men_joined,
  events_df = EventsMen,
  team_name = "Denmark"
)

denmark_women <- build_team_bundle(
  shots_df = ShotsWomen,
  joined_df = women_joined,
  events_df = EventsWomen,
  team_name = "Denmark Women's"
)

men_dataset_summary <- build_dataset_summary(ShotsMen, men_joined, "matchId")
women_dataset_summary <- build_dataset_summary(ShotsWomen, women_joined, "match_id")

ego_summary_men <- build_ego_player_summary(men_joined, min_shots = 5)
ego_summary_women <- build_ego_player_summary(women_joined, min_shots = 5)

ego_top5_men <- ego_summary_men %>% slice_head(n = 5)
ego_top5_women <- ego_summary_women %>% slice_head(n = 5)

ronaldo_profile <- ego_summary_men %>%
  filter(shooter_name == "Cristiano Ronaldo dos Santos Aveiro")

beth_mead_profile <- ego_summary_women %>%
  filter(shooter_name == "Bethany Mead")


# =========================================================
# 7. UI
# =========================================================

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#E30613",
    secondary = "#B80000",
    success = "#059669",
    info = "#E30613",
    warning = "#f59e0b",
    bg = "#F5F5F5",
    fg = "#1F1F1F",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background: #F5F5F5;
      }

      .app-header {
        background: linear-gradient(90deg, #E30613 0%, #B80000 100%);
        color: white;
        padding: 26px 32px;
        margin: -15px -15px 24px -15px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.12);
      }

      .app-title {
        font-size: 42px;
        font-weight: 800;
        line-height: 1.1;
        margin: 0;
      }

      .app-subtitle {
        font-size: 16px;
        margin-top: 8px;
        opacity: 0.95;
      }

      .sidebar-note {
        font-size: 12px;
        color: #555555;
        margin-top: 8px;
        line-height: 1.5;
      }

      .meta-card, .value-box {
        background: #FFFFFF;
        border: 1px solid #E5E5E5;
        border-radius: 14px;
        padding: 16px;
        margin-bottom: 16px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.06);
      }

      .value-box-title {
        font-size: 14px;
        color: #666666;
        margin-bottom: 8px;
      }

      .value-box-value {
        font-size: 30px;
        font-weight: 800;
        color: #1F1F1F;
      }

      .stats-card h4, .meta-card h3, .meta-card h4 {
        color: #B80000;
        font-weight: 800;
      }

      .section-title {
        font-weight: 800;
        font-size: 24px;
        color: #B80000;
        margin-top: 12px;
        margin-bottom: 14px;
      }

      .table.stats-table {
        margin-bottom: 0;
      }

      .table.stats-table th {
        color: #B80000;
        font-weight: 800;
        border-top: none;
        white-space: nowrap;
      }

      .table.stats-table td {
        vertical-align: middle;
      }

      .nav-tabs {
        border-bottom: 2px solid #D9D9D9;
      }

      .nav-tabs .nav-link {
        border-radius: 10px 10px 0 0;
        font-weight: 800;
        color: #B80000;
        background-color: transparent;
      }

      .nav-tabs .nav-link.active {
        background-color: #FFFFFF;
        color: #E30613;
        border-color: #D9D9D9 #D9D9D9 #FFFFFF;
      }

      .btn, .btn-default, .btn-primary {
        border-radius: 10px !important;
        font-weight: 700;
      }

      .form-control, .selectize-input {
        border-radius: 10px !important;
      }
    "))
  ),
  
  div(
    class = "app-header",
    tags$div(class = "app-title", "Freeze-frame analyse af afslutningssituationer"),
    tags$div(
      class = "app-subtitle",
      "Interaktiv analyse af skudbeslutninger i herre- og kvindefodbold baseret på StatsBomb data."
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      conditionalPanel(
        condition = "input.main_tabs == 'data_tab'",
        div(class = "sidebar-note", "Her kan du læse om datakilde, turneringer, hold og metode.")
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'overview_tab'",
        selectInput(
          "overview_league",
          "Vis data for",
          choices = c("Begge", "Mænd", "Kvinder"),
          selected = "Begge"
        ),
        div(class = "sidebar-note", "Overblikket viser nøgletal, skudtyper og sammenhængen mellem beslutning og udfald.")
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'denmark_tab'",
        div(class = "sidebar-note", "Danmark-fanen samler statistik for Danmark mænd og Danmark kvinder.")
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'case_tab'",
        radioButtons(
          "league",
          "Vælg turnering",
          choices = c("Mænd", "Kvinder"),
          inline = TRUE
        ),
        selectInput(
          "case_type",
          "Vælg case-type",
          choices = c("Alle", "Bedre afleveringsmulighed", "Ingen bedre afleveringsmulighed"),
          selected = "Alle"
        ),
        selectInput(
          "case_goal_filter",
          "Vælg udfald",
          choices = c("Alle", "Mål", "Ikke mål"),
          selected = "Alle"
        ),
        selectInput(
          "case_shot_type",
          "Vælg skudtype",
          choices = c("Alle", "Åbent spil", "Frispark", "Hjørnespark"),
          selected = "Alle"
        ),
        uiOutput("shot_selector"),
        div(class = "sidebar-note", "Straffespark er fjernet fra case-visningen. Hold musen over spillerne i plottet for at se navn og rolle.")
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'ego_tab'",
        div(class = "sidebar-note", "EGO-fanen kombinerer top 5-rangeringer med profilkort for Cristiano Ronaldo og Beth Mead.")
      )
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(
          title = "Datasæt",
          value = "data_tab",
          br(),
          uiOutput("dataset_info")
        ),
        
        tabPanel(
          title = "Overblik",
          value = "overview_tab",
          br(),
          fluidRow(
            column(3, uiOutput("box_matches")),
            column(3, uiOutput("box_shots")),
            column(3, uiOutput("box_goals")),
            column(3, uiOutput("box_xg"))
          ),
          fluidRow(
            column(4, uiOutput("box_better_pass")),
            column(4, uiOutput("box_better_goal")),
            column(4, uiOutput("box_better_not_goal"))
          ),
          fluidRow(
            column(4, uiOutput("box_goals_per_match"))
          ),
          br(),
          plotOutput("overall_plot", height = "360px"),
          br(),
          plotOutput("decision_outcome_plot", height = "420px"),
          br(),
          plotOutput("shot_type_plot", height = "380px"),
          br(),
          verbatimTextOutput("summary_text"),
          br(),
          tags$div(class = "section-title", "Spillerstatistik"),
          uiOutput("player_stats")
        ),
        
        tabPanel(
          title = "Danmark",
          value = "denmark_tab",
          br(),
          uiOutput("denmark_info")
        ),
        
        tabPanel(
          title = "Case-plot",
          value = "case_tab",
          br(),
          uiOutput("case_info"),
          girafeOutput("case_plot", height = "900px")
        ),
        
        tabPanel(
          title = "EGO-analyse",
          value = "ego_tab",
          br(),
          uiOutput("ego_info")
        )
      )
    )
  )
)


# =========================================================
# 8. SERVER
# =========================================================

server <- function(input, output, session) {
  
  # -------------------------------------------------------
  # 8.1 Reaktive datasæt til overblik
  # -------------------------------------------------------
  
  overview_results <- reactive({
    if (input$overview_league == "Mænd") return(men_joined)
    if (input$overview_league == "Kvinder") return(women_joined)
    all_results
  })
  
  overview_shots <- reactive({
    if (input$overview_league == "Mænd") return(shots_men_std)
    if (input$overview_league == "Kvinder") return(shots_women_std)
    bind_rows(shots_men_std, shots_women_std)
  })
  
  overview_stats <- reactive({
    res <- overview_results()
    
    better_cases <- res %>% filter(better_placed_teammate_exists == TRUE)
    
    matches <- n_distinct(res$match_id_std)
    goals <- sum(res$shot_outcome == "Goal", na.rm = TRUE)
    
    data.frame(
      matches = matches,
      shots = nrow(res),
      goals = goals,
      avg_xg = mean(res$shot_xg, na.rm = TRUE),
      better_share = mean(res$better_placed_teammate_exists, na.rm = TRUE),
      goals_per_match = ifelse(matches > 0, goals / matches, NA_real_),
      better_goal_share = ifelse(
        nrow(better_cases) > 0,
        mean(better_cases$shot_outcome == "Goal", na.rm = TRUE),
        NA_real_
      ),
      better_not_goal_share = ifelse(
        nrow(better_cases) > 0,
        mean(better_cases$shot_outcome != "Goal", na.rm = TRUE),
        NA_real_
      )
    )
  })
  
  
  # -------------------------------------------------------
  # 8.2 Value boxes
  # -------------------------------------------------------
  
  output$box_matches <- renderUI({
    s <- overview_stats()
    make_value_box("Antal kampe", comma(s$matches), "#FDECEC")
  })
  
  output$box_shots <- renderUI({
    s <- overview_stats()
    make_value_box("Antal skud", comma(s$shots), "#FFF4F4")
  })
  
  output$box_goals <- renderUI({
    s <- overview_stats()
    make_value_box("Antal mål", comma(s$goals), "#FCE8E8")
  })
  
  output$box_xg <- renderUI({
    s <- overview_stats()
    make_value_box("Gennemsnitlig xG", number(s$avg_xg, accuracy = 0.001), "#FFF8F8")
  })
  
  output$box_better_pass <- renderUI({
    s <- overview_stats()
    make_value_box(
      "Andel med bedre afleveringsmulighed",
      percent(s$better_share, accuracy = 0.1),
      "#FDECEC"
    )
  })
  
  output$box_goals_per_match <- renderUI({
    s <- overview_stats()
    make_value_box(
      "Mål pr. kamp",
      number(s$goals_per_match, accuracy = 0.01),
      "#FCE8E8"
    )
  })
  
  output$box_better_goal <- renderUI({
    s <- overview_stats()
    make_value_box(
      "Bedre mulighed → mål",
      percent(s$better_goal_share, accuracy = 0.1),
      "#FFF4F4"
    )
  })
  
  output$box_better_not_goal <- renderUI({
    s <- overview_stats()
    make_value_box(
      "Bedre mulighed → ikke mål",
      percent(s$better_not_goal_share, accuracy = 0.1),
      "#FDECEC"
    )
  })
  
  
  # -------------------------------------------------------
  # 8.3 Overbliksplots
  # -------------------------------------------------------
  
  output$overall_plot <- renderPlot({
    plot_df <- bind_rows(
      women_joined %>%
        summarise(
          Gruppe = "Kvinder",
          Andel = mean(better_placed_teammate_exists, na.rm = TRUE)
        ),
      men_joined %>%
        summarise(
          Gruppe = "Mænd",
          Andel = mean(better_placed_teammate_exists, na.rm = TRUE)
        )
    )
    
    if (input$overview_league != "Begge") {
      plot_df <- plot_df %>% filter(Gruppe == input$overview_league)
    }
    
    ggplot(plot_df, aes(x = Gruppe, y = Andel, fill = Gruppe)) +
      geom_col(width = 0.55, show.legend = FALSE) +
      geom_text(
        aes(label = percent(Andel, accuracy = 0.1)),
        vjust = -0.6, size = 6, fontface = "bold"
      ) +
      scale_fill_manual(values = c("Kvinder" = "#E30613", "Mænd" = "#6B7280")) +
      scale_y_continuous(
        labels = percent,
        limits = c(0, max(0.7, max(plot_df$Andel, na.rm = TRUE) + 0.08)),
        expand = expansion(mult = c(0, 0.05))
      ) +
      labs(
        title = "Andel af skud hvor en bedre afleveringsmulighed identificeres",
        subtitle = "Sammenligning mellem mænd og kvinder",
        x = NULL,
        y = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = "#1F1F1F"),
        plot.subtitle = element_text(size = 13, margin = margin(b = 10), color = "#555555"),
        axis.text.x = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
      )
  })
  
  output$decision_outcome_plot <- renderPlot({
    df <- overview_results() %>%
      filter(!is.na(decision_outcome_text), decision_outcome_text != "Ukendt")
    
    plot_df <- df %>%
      count(league, decision_outcome_text, name = "n") %>%
      group_by(league) %>%
      mutate(share = n / sum(n)) %>%
      ungroup()
    
    if (input$overview_league != "Begge") {
      plot_df <- plot_df %>% filter(league == input$overview_league)
    }
    
    ggplot(plot_df, aes(x = decision_outcome_text, y = share, fill = league)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = percent(share, accuracy = 0.1)),
        position = position_dodge(width = 0.9),
        vjust = -0.4,
        size = 4
      ) +
      scale_fill_manual(values = c("Kvinder" = "#E30613", "Mænd" = "#6B7280")) +
      scale_y_continuous(
        labels = percent,
        limits = c(0, max(0.45, max(plot_df$share, na.rm = TRUE) + 0.08)),
        expand = expansion(mult = c(0, 0.05))
      ) +
      labs(
        title = "Beslutning og udfald",
        subtitle = "Viser både om en bedre afleveringsmulighed fandtes, og om skuddet blev mål.",
        x = NULL,
        y = NULL,
        fill = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = "#1F1F1F"),
        plot.subtitle = element_text(size = 13, margin = margin(b = 10), color = "#555555"),
        axis.text.x = element_text(size = 11, face = "bold"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
      )
  })
  
  output$shot_type_plot <- renderPlot({
    shots_df <- overview_shots() %>%
      filter(shot_type_label != "Straffespark") %>%
      filter(!is.na(shot_type_label))
    
    plot_df <- shots_df %>%
      count(league, shot_type_label, name = "n")
    
    req(nrow(plot_df) > 0)
    
    ggplot(plot_df, aes(x = shot_type_label, y = n, fill = league)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Kvinder" = "#E30613", "Mænd" = "#6B7280")) +
      labs(
        title = "Fordeling af skudtyper",
        subtitle = "Straffespark er udeladt for at fokusere på mere sammenlignelige afslutningssituationer.",
        x = NULL,
        y = "Antal skud",
        fill = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = "#1F1F1F"),
        plot.subtitle = element_text(size = 13, margin = margin(b = 10), color = "#555555"),
        axis.text.x = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
      )
  })
  
  output$summary_text <- renderText({
    men_val <- percent(mean(men_joined$better_placed_teammate_exists, na.rm = TRUE), accuracy = 0.1)
    women_val <- percent(mean(women_joined$better_placed_teammate_exists, na.rm = TRUE), accuracy = 0.1)
    
    paste0(
      "Mænd: ", men_val, "\n",
      "Kvinder: ", women_val, "\n\n",
      "I analysen skelnes der mellem beslutning og udfald.\n",
      "Et skud kan godt ende i mål, selvom modellen identificerer en bedre afleveringsmulighed.\n",
      "Derfor beholdes mål i datasættet i stedet for at blive fjernet."
    )
  })
  
  
  # -------------------------------------------------------
  # 8.4 Spillerstatistik
  # -------------------------------------------------------
  
  output$player_stats <- renderUI({
    fluidRow(
      column(
        6,
        tags$h3("Mænd"),
        make_stats_table(men_stats$top_scorers, "Topscorere", "n"),
        make_stats_table(men_stats$top_assists, "Top assists", "n"),
        make_stats_table(men_stats$top_shot_assists, "Shot assists", "n"),
        make_stats_table(men_stats$top_shots, "Flest skud", "n"),
        make_stats_table(men_stats$top_xg, "Samlet xG", "xG", digits = 2)
      ),
      column(
        6,
        tags$h3("Kvinder"),
        make_stats_table(women_stats$top_scorers, "Topscorere", "n"),
        make_stats_table(women_stats$top_assists, "Top assists", "n"),
        make_stats_table(women_stats$top_shot_assists, "Shot assists", "n"),
        make_stats_table(women_stats$top_shots, "Flest skud", "n"),
        make_stats_table(women_stats$top_xg, "Samlet xG", "xG", digits = 2)
      )
    )
  })
  
  
  # -------------------------------------------------------
  # 8.5 Danmark-fane
  # -------------------------------------------------------
  
  output$denmark_info <- renderUI({
    den_men_matches <- dplyr::n_distinct(denmark_men$shots$matchId)
    den_women_matches <- dplyr::n_distinct(denmark_women$shots$match_id)
    
    den_men_shots_n <- nrow(denmark_men$shots)
    den_women_shots_n <- nrow(denmark_women$shots)
    
    den_men_goals <- sum(denmark_men$shots$shot.outcome.name == "Goal", na.rm = TRUE)
    den_women_goals <- sum(denmark_women$shots$shot.outcome.name == "Goal", na.rm = TRUE)
    
    den_men_xg <- mean(denmark_men$shots$shot.statsbomb_xg, na.rm = TRUE)
    den_women_xg <- mean(denmark_women$shots$shot.statsbomb_xg, na.rm = TRUE)
    
    den_men_better <- mean(denmark_men$joined$better_placed_teammate_exists, na.rm = TRUE)
    den_women_better <- mean(denmark_women$joined$better_placed_teammate_exists, na.rm = TRUE)
    
    div(
      tags$div(class = "section-title", "Danmark i turneringerne"),
      
      fluidRow(
        column(3, make_value_box("Kampe (Danmark mænd)", den_men_matches, "#FDECEC")),
        column(3, make_value_box("Kampe (Danmark kvinder)", den_women_matches, "#FDECEC")),
        column(3, make_value_box("Skud (Danmark mænd)", den_men_shots_n, "#FFF4F4")),
        column(3, make_value_box("Skud (Danmark kvinder)", den_women_shots_n, "#FFF4F4"))
      ),
      
      fluidRow(
        column(3, make_value_box("Mål (Danmark mænd)", den_men_goals, "#FCE8E8")),
        column(3, make_value_box("Mål (Danmark kvinder)", den_women_goals, "#FCE8E8")),
        column(3, make_value_box("Gns. xG (Danmark mænd)", number(den_men_xg, accuracy = 0.001), "#FFF8F8")),
        column(3, make_value_box("Gns. xG (Danmark kvinder)", number(den_women_xg, accuracy = 0.001), "#FFF8F8"))
      ),
      
      fluidRow(
        column(6, make_value_box("Bedre afleveringsmulighed (Danmark mænd)", percent(den_men_better, accuracy = 0.1), "#FDECEC")),
        column(6, make_value_box("Bedre afleveringsmulighed (Danmark kvinder)", percent(den_women_better, accuracy = 0.1), "#FDECEC"))
      ),
      
      br(),
      
      fluidRow(
        column(
          6,
          tags$h3("🇩🇰 Danmark mænd"),
          make_stats_table(denmark_men$stats$top_scorers, "Topscorere", "n"),
          make_stats_table(denmark_men$stats$top_assists, "Top assists", "n"),
          make_stats_table(denmark_men$stats$top_shot_assists, "Shot assists", "n"),
          make_stats_table(denmark_men$stats$top_shots, "Flest skud", "n"),
          make_stats_table(denmark_men$stats$top_xg, "Samlet xG", "xG", digits = 2)
        ),
        column(
          6,
          tags$h3("🇩🇰 Danmark kvinder"),
          make_stats_table(denmark_women$stats$top_scorers, "Topscorere", "n"),
          make_stats_table(denmark_women$stats$top_assists, "Top assists", "n"),
          make_stats_table(denmark_women$stats$top_shot_assists, "Shot assists", "n"),
          make_stats_table(denmark_women$stats$top_shots, "Flest skud", "n"),
          make_stats_table(denmark_women$stats$top_xg, "Samlet xG", "xG", digits = 2)
        )
      )
    )
  })
  
  
  # -------------------------------------------------------
  # 8.6 Case-datasæt
  # -------------------------------------------------------
  
  current_results <- reactive({
    if (input$league == "Mænd") men_joined else women_joined
  })
  
  current_shots <- reactive({
    if (input$league == "Mænd") shots_men_std else shots_women_std
  })
  
  
  # -------------------------------------------------------
  # 8.7 Filtrering af cases
  # -------------------------------------------------------
  
  filtered_results <- reactive({
    df <- current_results()
    
    df <- df %>%
      filter(shot_type_label != "Straffespark") %>%
      filter(has_freeze %in% TRUE)
    
    if (input$case_type == "Bedre afleveringsmulighed") {
      df <- df %>% filter(better_placed_teammate_exists == TRUE)
    } else if (input$case_type == "Ingen bedre afleveringsmulighed") {
      df <- df %>% filter(better_placed_teammate_exists == FALSE)
    }
    
    if (input$case_goal_filter == "Mål") {
      df <- df %>% filter(shot_outcome == "Goal")
    } else if (input$case_goal_filter == "Ikke mål") {
      df <- df %>% filter(shot_outcome != "Goal")
    }
    
    if (input$case_shot_type != "Alle") {
      df <- df %>% filter(shot_type_label == input$case_shot_type)
    }
    
    df %>%
      arrange(desc(shot_xg), desc(minute), desc(second))
  })
  
  
  # -------------------------------------------------------
  # 8.8 Dropdown til case-valg
  # -------------------------------------------------------
  
  output$shot_selector <- renderUI({
    df <- filtered_results()
    
    if (nrow(df) == 0) {
      return(tags$p("Ingen cases matcher de valgte filtre."))
    }
    
    choice_values <- paste(df$match_id_std, df$shot_id_std, sep = "__")
    
    selectInput(
      "selected_shot",
      "Vælg skud",
      choices = stats::setNames(as.list(choice_values), df$display)
    )
  })
  
  selected_keys <- reactive({
    req(input$selected_shot)
    parts <- strsplit(input$selected_shot, "__")[[1]]
    req(length(parts) == 2)
    
    list(
      match_id_std = parts[1],
      shot_id_std = parts[2]
    )
  })
  
  selected_result <- reactive({
    keys <- selected_keys()
    
    filtered_results() %>%
      filter(
        match_id_std == keys$match_id_std,
        shot_id_std == keys$shot_id_std
      )
  })
  
  selected_shot_row <- reactive({
    keys <- selected_keys()
    
    current_shots() %>%
      filter(
        match_id_std == keys$match_id_std,
        shot_id_std == keys$shot_id_std
      )
  })
  
  
  # -------------------------------------------------------
  # 8.9 Metadata til case
  # -------------------------------------------------------
  
  output$case_info <- renderUI({
    row <- selected_result()
    req(nrow(row) == 1)
    
    shot_type <- ifelse(is.na(row$shot_type_label[1]), "Ukendt", row$shot_type_label[1])
    play_pattern <- ifelse(is.na(row$play_pattern_label[1]), "Ukendt", row$play_pattern_label[1])
    minute_txt <- ifelse(
      is.na(row$minute[1]),
      "Ukendt",
      paste0(row$minute[1], ":", sprintf("%02d", row$second[1]))
    )
    team_txt <- ifelse(
      is.na(row$team.name[1]),
      "Ukendt",
      get_team_label_with_flag(row$team.name[1])
    )
    better_txt <- dplyr::case_when(
      is.na(row$better_placed_teammate_exists[1]) ~ "Ukendt",
      row$better_placed_teammate_exists[1] ~ "Ja",
      TRUE ~ "Nej"
    )
    best_name <- ifelse(is.na(row$best_teammate_name[1]), "Ingen", row$best_teammate_name[1])
    outcome_txt <- ifelse(is.na(row$shot_outcome_label[1]), "Ukendt", row$shot_outcome_label[1])
    
    div(
      class = "meta-card",
      fluidRow(
        column(
          4,
          tags$b("Skytte: "), tags$span(row$shooter_name[1]), br(),
          tags$b("Hold: "), tags$span(team_txt), br(),
          tags$b("Tidspunkt: "), tags$span(minute_txt)
        ),
        column(
          4,
          tags$b("Udfald: "), tags$span(outcome_txt), br(),
          tags$b("xG: "), tags$span(number(row$shot_xg[1], accuracy = 0.001)), br(),
          tags$b("Under pres: "), tags$span(ifelse(isTRUE(row$under_pressure[1]), "Ja", "Nej"))
        ),
        column(
          4,
          tags$b("Skudtype: "), tags$span(shot_type), br(),
          tags$b("Spilmønster: "), tags$span(play_pattern), br(),
          tags$b("Bedre afleveringsmulighed findes: "), tags$span(better_txt)
        )
      ),
      tags$hr(),
      fluidRow(
        column(
          6,
          tags$b("Skytte - modspillere i trekant: "),
          tags$span(ifelse(is.na(row$shooter_blockers[1]), "Ukendt", row$shooter_blockers[1])), br(),
          tags$b("Skytte - afstand til mål: "),
          tags$span(ifelse(is.na(row$shooter_goal_distance[1]), "Ukendt", number(row$shooter_goal_distance[1], accuracy = 0.1)))
        ),
        column(
          6,
          tags$b("Bedste medspiller: "), tags$span(best_name), br(),
          tags$b("Medspiller - modspillere i trekant: "),
          tags$span(ifelse(is.na(row$best_teammate_blockers[1]), "Ukendt", row$best_teammate_blockers[1]))
        )
      )
    )
  })
  
  
  # -------------------------------------------------------
  # 8.10 Selve case-plottet
  # -------------------------------------------------------
  
  output$case_plot <- renderGirafe({
    row <- selected_result()
    shot <- selected_shot_row()
    req(nrow(row) == 1, nrow(shot) == 1)
    
    p <- build_case_plot(
      shot_row = shot,
      result_row = row,
      better_case = isTRUE(row$better_placed_teammate_exists[1])
    )
    
    girafe(
  ggobj = p,
  width_svg = 14,
  height_svg = 7,
      options = list(
        opts_tooltip(
          css = "background:rgba(31,31,31,0.96);color:white;padding:8px 10px;border-radius:10px;font-size:12px;"
        ),
        opts_hover(css = "stroke:black;stroke-width:2;"),
        opts_sizing(rescale = TRUE)
      )
    )
  })
  
  
  # -------------------------------------------------------
  # 8.11 EGO-fanen
  # -------------------------------------------------------
  
  output$ego_info <- renderUI({
    div(
      div(
        class = "meta-card",
        tags$h3("Sådan læses EGO-analysen"),
        tags$ul(
          tags$li("Bedre afleveringsmulighed betyder, at modellen vurderer, at en medspiller stod bedre placeret end skytten."),
          tags$li("Et EGO-lignende skud er her defineret som: bedre afleveringsmulighed + skuddet bliver ikke mål."),
          tags$li("Top 5-rangeringen nedenfor er baseret på antal EGO-skud og viser derfor volumen, ikke nødvendigvis relativ tendens alene.")
        )
      ),
      
      make_ego_table(ego_top5_men, "Top 5 mænd – flest EGO-skud (antal)"),
      make_ego_table(ego_top5_women, "Top 5 kvinder – flest EGO-skud (antal)"),
      
      fluidRow(
        column(
          6,
          make_player_profile_card(ronaldo_profile, "Spillerprofil – Cristiano Ronaldo")
        ),
        column(
          6,
          make_player_profile_card(beth_mead_profile, "Spillerprofil – Beth Mead")
        )
      )
    )
  })
  
  
  # -------------------------------------------------------
  # 8.12 Datasæt-fanen
  # -------------------------------------------------------
  
  output$dataset_info <- renderUI({
    men_teams <- sort(unique(ShotsMen$team.name))
    women_teams <- sort(unique(ShotsWomen$team.name))
    
    men_teams_flag <- sapply(men_teams, get_team_label_with_flag)
    women_teams_flag <- sapply(women_teams, get_team_label_with_flag)
    
    men_goals_per_match <- men_dataset_summary$goals / men_dataset_summary$matches
    women_goals_per_match <- women_dataset_summary$goals / women_dataset_summary$matches
    
    div(
      div(
        class = "meta-card",
        tags$h3("Om datasættet"),
        tags$p("Denne analyse er baseret på eventdata fra StatsBomb og dækker kampe fra UEFA Euro 2024 (mænd) og UEFA Women’s Euro 2022 (kvinder)."),
        tags$p("Formålet er at identificere forskelle i spillestil mellem mænd og kvinder med fokus på afslutninger, beslutningstagning og spilstruktur."),
        tags$p("Analysen søger ikke at rangere kvalitet eller niveau, men at belyse hvordan spillet strukturelt kan udfolde sig forskelligt.")
      ),
      
      div(
        class = "meta-card",
        tags$h4("Sådan læses appen"),
        tags$ul(
          tags$li("Bedre afleveringsmulighed: modellen vurderer, at en medspiller stod bedre placeret end skytten."),
          tags$li("Beslutning og udfald holdes adskilt: et skud kan godt blive mål, selvom en bedre aflevering fandtes."),
          tags$li("EGO-lignende skud bruges som arbejdsdefinition for skud, hvor en bedre afleveringsmulighed fandtes, og skuddet ikke blev til mål.")
        )
      ),
      
      fluidRow(
        column(3, make_value_box("Kampe (mænd)", men_dataset_summary$matches, "#FDECEC")),
        column(3, make_value_box("Kampe (kvinder)", women_dataset_summary$matches, "#FDECEC")),
        column(3, make_value_box("Skud (mænd)", men_dataset_summary$shots, "#FFF4F4")),
        column(3, make_value_box("Skud (kvinder)", women_dataset_summary$shots, "#FFF4F4"))
      ),
      
      fluidRow(
        column(3, make_value_box("Mål (mænd)", men_dataset_summary$goals, "#FCE8E8")),
        column(3, make_value_box("Mål (kvinder)", women_dataset_summary$goals, "#FCE8E8")),
        column(3, make_value_box("Gns. xG (mænd)", number(men_dataset_summary$avg_xg, accuracy = 0.001), "#FFF8F8")),
        column(3, make_value_box("Gns. xG (kvinder)", number(women_dataset_summary$avg_xg, accuracy = 0.001), "#FFF8F8"))
      ),
      
      fluidRow(
        column(6, make_value_box("Mål pr. kamp (mænd)", number(men_goals_per_match, accuracy = 0.01), "#FDECEC")),
        column(6, make_value_box("Mål pr. kamp (kvinder)", number(women_goals_per_match, accuracy = 0.01), "#FDECEC"))
      ),
      
      br(),
      
      fluidRow(
        column(
          6,
          div(
            class = "meta-card",
            tags$h4("UEFA Euro 2024"),
            tags$p(paste0("Antal hold: ", length(men_teams))),
            tags$div(
              style = "font-size:13px; line-height:1.7;",
              paste(men_teams_flag, collapse = ", ")
            )
          )
        ),
        column(
          6,
          div(
            class = "meta-card",
            tags$h4("UEFA Women's Euro 2022"),
            tags$p(paste0("Antal hold: ", length(women_teams))),
            tags$div(
              style = "font-size:13px; line-height:1.7;",
              paste(women_teams_flag, collapse = ", ")
            )
          )
        )
      ),
      
      br(),
      
      div(
        class = "meta-card",
        tags$h4("Metode (Freeze-frame analyse)"),
        tags$p("Analysen anvender freeze-frame data fra StatsBomb, som viser spillernes positioner i det øjeblik et skud bliver taget."),
        tags$p("For hver afslutning konstrueres en trekant mellem skytte og målstolperne. Antallet af modspillere i denne zone bruges som mål for, hvor presset afslutningen er."),
        tags$p("Denne vurdering sammenlignes med medspillere i bedre positioner, hvilket gør det muligt at identificere situationer, hvor en aflevering kunne have været et stærkere alternativ end et skud."),
        tags$p("Et vigtigt metodisk valg er, at mål ikke fjernes fra analysen. Formålet er at skelne mellem beslutning og udfald: et skud kan godt ende i mål, selvom en bedre afleveringsmulighed fandtes.")
      ),
      
      div(
        class = "meta-card",
        tags$h4("Datastruktur"),
        tags$p("Datasættet indeholder blandt andet følgende information:"),
        tags$ul(
          tags$li("Position (x, y) for skud"),
          tags$li("xG (forventet målsandsynlighed)"),
          tags$li("Spillernes positioner i freeze-frame"),
          tags$li("Skudtype (åbent spil, frispark, hjørnespark, straffespark)"),
          tags$li("Spilmønster (kontra, dødbold, indkast osv.)"),
          tags$li("Udfald af skud (mål, reddet, blokeret, forbi osv.)")
        )
      )
    )
  })
}

shinyApp(ui, server)



