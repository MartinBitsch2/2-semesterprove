library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(ggplot2)

safe_read_rds <- function(path) if (file.exists(path)) readRDS(path) else NULL

clean_teamname <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x <- gsub("\\s+", " ", x)
  x <- gsub("\\.", "", x)
  x <- gsub("-", " ", x)
  x <- gsub("^fc ", "", x)
  x <- gsub(" fc$| bk$| if$", "", x)
  trimws(x)
}

result_label_vec <- function(goals_for, goals_against) {
  case_when(
    is.na(goals_for) | is.na(goals_against) ~ "Ukendt",
    goals_for > goals_against ~ "Vundet",
    goals_for < goals_against ~ "Tabt",
    TRUE ~ "Uafgjort"
  )
}

vurdering_xg_vec <- function(goals_for, goals_against, xg_for, xg_against, tol = 0.05) {
  actual_result <- case_when(
    is.na(goals_for) | is.na(goals_against) ~ NA_integer_,
    goals_for > goals_against ~ 1,
    goals_for < goals_against ~ -1,
    TRUE ~ 0
  )
  
  expected_result <- case_when(
    is.na(xg_for) | is.na(xg_against) ~ NA_integer_,
    xg_for - xg_against > tol ~ 1,
    xg_against - xg_for > tol ~ -1,
    TRUE ~ 0
  )
  
  case_when(
    is.na(actual_result) | is.na(expected_result) ~ "Ikke beregnet",
    actual_result > expected_result ~ "Overpræsteret",
    actual_result < expected_result ~ "Underpræsteret",
    TRUE ~ "Som forventet"
  )
}

valid_shot_for_model <- function(df) {
  df %>%
    filter(
      selvmaal == 0,
      PRIMARYTYPE != "penalty",
      PRIMARYTYPE != "free_kick",
      PRIMARYTYPE != "own_goal"
    )
}

get_player_name_col <- function(df) {
  if ("SHORTNAME" %in% names(df)) return("SHORTNAME")
  if ("PLAYERNAME" %in% names(df)) return("PLAYERNAME")
  if ("PLAYER_WYID" %in% names(df)) return("PLAYER_WYID")
  names(df)[1]
}

make_dt <- function(df, pageLength = 10, escape = TRUE) {
  datatable(
    df,
    rownames = FALSE,
    escape = escape,
    options = list(pageLength = pageLength, dom = "tip", scrollX = TRUE)
  )
}

rename_if_exists <- function(df, mapping) {
  old_names <- names(mapping)
  present <- old_names[old_names %in% names(df)]
  if (!length(present)) return(df)
  dplyr::rename(df, !!!stats::setNames(as.list(present), unname(mapping[present])))
}

drop_if_exists <- function(df, cols) {
  df %>% select(-any_of(cols))
}

metric_box <- function(label, value, sub = NULL) {
  div(
    class = "metric-box",
    div(class = "metric-label", label),
    div(class = "metric-value", value),
    if (!is.null(sub)) div(class = "metric-sub", sub)
  )
}

pitch_length <- 105
pitch_width <- 68
goal_width <- 7.32
goal_center_x <- 105
goal_center_y <- 34
halfway_line_x <- pitch_length / 2
left_post_y <- goal_center_y + goal_width / 2
right_post_y <- goal_center_y - goal_width / 2

compute_distance <- function(x, y) sqrt((goal_center_x - x)^2 + (goal_center_y - y)^2)

compute_angle_deg <- function(x, y) {
  d1 <- sqrt((goal_center_x - x)^2 + (left_post_y - y)^2)
  d2 <- sqrt((goal_center_x - x)^2 + (right_post_y - y)^2)
  cos_v <- (d1^2 + d2^2 - goal_width^2) / (2 * d1 * d2)
  acos(pmin(pmax(cos_v, -1), 1)) * 180 / pi
}

compute_x_from_direct_distance <- function(distance_to_goal, width_pos) {
  lateral_offset <- width_pos - goal_center_y
  horizontal_component <- sqrt(pmax(distance_to_goal^2 - lateral_offset^2, 0))
  goal_center_x - horizontal_component
}

predict_xg <- function(model, x, y, kontra = FALSE, corner = FALSE) {
  if (is.null(model)) return(NA_real_)
  newdata <- data.frame(
    afstand_til_mål = compute_distance(x, y),
    vinkel_mellem_stolper = compute_angle_deg(x, y),
    Kontraangreb = as.integer(kontra),
    corner = as.integer(corner)
  )
  tryCatch(
    as.numeric(predict(model, newdata = newdata, type = "response"))[1],
    error = function(e) NA_real_
  )
}

get_xg_explanation <- function(distance, angle, xg, kontra, corner) {
  distance_txt <- case_when(
    distance <= 10 ~ "Afslutningen er meget tæt på mål",
    distance <= 16 ~ "Afslutningen er i en fornuftig afstand til mål",
    TRUE ~ "Afslutningen er relativt langt fra mål"
  )
  
  angle_txt <- case_when(
    angle >= 35 ~ "vinklen mod målet er god",
    angle >= 20 ~ "vinklen mod målet er middel",
    TRUE ~ "vinklen mod målet er smal"
  )
  
  xg_txt <- case_when(
    is.na(xg) ~ "Modellen kunne ikke beregne en xG-værdi.",
    xg >= 0.30 ~ "Det er en stor chance.",
    xg >= 0.15 ~ "Det er en rimelig chance.",
    TRUE ~ "Det er en forholdsvis lille chance."
  )
  
  situation_txt <- c()
  if (kontra) situation_txt <- c(situation_txt, "Skuddet er markeret som kontraangreb")
  if (corner) situation_txt <- c(situation_txt, "Skuddet er markeret som hjørnesparksfase")
  if (!length(situation_txt)) situation_txt <- "Skuddet er behandlet som almindeligt åbent spil"
  
  paste0(distance_txt, ", og ", angle_txt, ". ", xg_txt, " ", paste(situation_txt, collapse = ". "), ".")
}

shot_zone_presets <- list(
  "Centralt tæt på mål" = c(x = 99, y = 34),
  "Centralt i feltet" = c(x = 92, y = 34),
  "Venstre side i feltet" = c(x = 92, y = 24),
  "Højre side i feltet" = c(x = 92, y = 44),
  "Venstre halv-space" = c(x = 86, y = 26),
  "Højre halv-space" = c(x = 86, y = 42),
  "Langskud centralt" = c(x = 78, y = 34),
  "Smal vinkel venstre" = c(x = 101, y = 12),
  "Smal vinkel højre" = c(x = 101, y = 56)
)

make_player_xg_table <- function(df, xg_col = "xG_pred") {
  needed <- c(xg_col, "selvmaal", "PRIMARYTYPE", "maal")
  if (is.null(df) || !all(needed %in% names(df))) return(NULL)
  
  player_col <- get_player_name_col(df)
  
  df %>%
    valid_shot_for_model() %>%
    filter(!is.na(.data[[xg_col]])) %>%
    transmute(
      Spiller = as.character(.data[[player_col]]),
      mål = maal,
      xg_value = .data[[xg_col]]
    ) %>%
    group_by(Spiller) %>%
    summarise(
      skud = n(),
      mål = sum(mål, na.rm = TRUE),
      xG = sum(xg_value, na.rm = TRUE),
      mål_minus_xG = mål - xG,
      .groups = "drop"
    ) %>%
    filter(skud >= 10) %>%
    mutate(
      xG = round(xG, 2),
      mål_minus_xG = round(mål_minus_xG, 2)
    ) %>%
    arrange(desc(mål_minus_xG))
}

ensure_gameweek <- function(df, season_label = NULL) {
  if (is.null(df)) return(NULL)
  if ("Gameweek" %in% names(df)) return(df)
  
  match_order <- df %>%
    distinct(MATCH_WYID) %>%
    arrange(MATCH_WYID) %>%
    mutate(kamp_nr = row_number())
  
  match_order <- if (!is.null(season_label) && season_label == "2024/25") {
    match_order %>%
      mutate(Gameweek = ifelse(kamp_nr <= 132, as.character(ceiling(kamp_nr / 6)), "slutspil"))
  } else {
    match_order %>% mutate(Gameweek = as.character(ceiling(kamp_nr / 6)))
  }
  
  left_join(df, match_order %>% select(MATCH_WYID, Gameweek), by = "MATCH_WYID")
}

simulate_match_xp_from_lambda <- function(lambda_home, lambda_away, n_sim = 5000, seed = 123) {
  set.seed(seed)
  lambda_home <- pmax(lambda_home, 0.01)
  lambda_away <- pmax(lambda_away, 0.01)
  
  goals_home <- rpois(n_sim, lambda_home)
  goals_away <- rpois(n_sim, lambda_away)
  
  tibble(
    xP_home = 3 * mean(goals_home > goals_away) + mean(goals_home == goals_away),
    xP_away = 3 * mean(goals_home < goals_away) + mean(goals_home == goals_away)
  )
}

build_brondby_matches_from_shots <- function(shots_df, season_label) {
  needed <- c("MATCH_WYID", "TEAMNAME", "SIDE", "maal", "selvmaal", "PRIMARYTYPE")
  if (is.null(shots_df) || !all(needed %in% names(shots_df))) return(NULL)
  
  shots_df <- ensure_gameweek(shots_df, season_label)
  brondby_clean <- clean_teamname("Brøndby IF")
  
  match_teams <- shots_df %>% distinct(MATCH_WYID, TEAMNAME, SIDE)
  
  brondby_matches <- match_teams %>%
    mutate(TEAMNAME_clean = clean_teamname(TEAMNAME)) %>%
    group_by(MATCH_WYID) %>%
    filter(any(TEAMNAME_clean == brondby_clean)) %>%
    ungroup()
  
  if (!nrow(brondby_matches)) return(NULL)
  
  regular_goals <- shots_df %>%
    filter(maal == 1, selvmaal == 0) %>%
    count(MATCH_WYID, TEAMNAME, name = "goals")
  
  own_goals <- shots_df %>%
    filter(selvmaal == 1) %>%
    select(MATCH_WYID, TEAMNAME) %>%
    left_join(match_teams %>% rename(scoring_team = TEAMNAME), by = "MATCH_WYID") %>%
    filter(scoring_team != TEAMNAME) %>%
    count(MATCH_WYID, scoring_team, name = "goals") %>%
    rename(TEAMNAME = scoring_team)
  
  all_goals <- bind_rows(regular_goals, own_goals) %>%
    group_by(MATCH_WYID, TEAMNAME) %>%
    summarise(goals = sum(goals), .groups = "drop")
  
  xg_by_team_match <- shots_df %>%
    valid_shot_for_model() %>%
    filter(!is.na(xG_pred)) %>%
    group_by(MATCH_WYID, TEAMNAME) %>%
    summarise(
      xG_for = sum(xG_pred, na.rm = TRUE),
      .groups = "drop"
    )
  
  match_wide <- brondby_matches %>%
    left_join(all_goals, by = c("MATCH_WYID", "TEAMNAME")) %>%
    left_join(xg_by_team_match, by = c("MATCH_WYID", "TEAMNAME")) %>%
    left_join(shots_df %>% distinct(MATCH_WYID, Gameweek), by = "MATCH_WYID") %>%
    mutate(
      goals = coalesce(goals, 0),
      xG_for = coalesce(xG_for, 0),
      Gameweek = as.character(Gameweek)
    ) %>%
    select(MATCH_WYID, TEAMNAME, SIDE, goals, xG_for, Gameweek) %>%
    pivot_wider(
      names_from = SIDE,
      values_from = c(TEAMNAME, goals, xG_for),
      names_sep = "_"
    )
  
  xP_match <- match_wide %>%
    rowwise() %>%
    mutate(
      sim = list(simulate_match_xp_from_lambda(coalesce(xG_for_home, 0), coalesce(xG_for_away, 0)))
    ) %>%
    mutate(
      xP_home = sim$xP_home,
      xP_away = sim$xP_away
    ) %>%
    ungroup() %>%
    select(MATCH_WYID, xP_home, xP_away)
  
  match_wide %>%
    left_join(xP_match, by = "MATCH_WYID") %>%
    mutate(
      brondby_is_home = clean_teamname(TEAMNAME_home) == brondby_clean,
      TEAMNAME = ifelse(brondby_is_home, TEAMNAME_home, TEAMNAME_away),
      opponent = ifelse(brondby_is_home, TEAMNAME_away, TEAMNAME_home),
      SIDE = ifelse(brondby_is_home, "home", "away"),
      goals_for = ifelse(brondby_is_home, goals_home, goals_away),
      goals_against = ifelse(brondby_is_home, goals_away, goals_home),
      xG_for = ifelse(brondby_is_home, xG_for_home, xG_for_away),
      xG_against = ifelse(brondby_is_home, xG_for_away, xG_for_home),
      xP = ifelse(brondby_is_home, xP_home, xP_away),
      result = result_label_vec(goals_for, goals_against),
      actual_points = case_when(
        goals_for > goals_against ~ 3,
        goals_for == goals_against ~ 1,
        goals_for < goals_against ~ 0,
        TRUE ~ NA_real_
      ),
      season = season_label,
      vurdering = vurdering_xg_vec(goals_for, goals_against, xG_for, xG_against)
    ) %>%
    filter(clean_teamname(TEAMNAME) == brondby_clean) %>%
    transmute(
      MATCH_WYID,
      Gameweek = as.character(Gameweek),
      TEAMNAME,
      opponent,
      SIDE,
      goals_for,
      goals_against,
      xG_for = round(xG_for, 2),
      xG_against = round(xG_against, 2),
      xP = round(xP, 2),
      result,
      actual_points,
      season,
      vurdering
    ) %>%
    arrange(suppressWarnings(as.numeric(Gameweek)), MATCH_WYID)
}

make_half_pitch_plot <- function(shots_df, team_label = "Hold") {
  if (is.null(shots_df) || nrow(shots_df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Ingen afslutninger fundet", size = 6) +
        theme_void()
    )
  }
  
  df <- shots_df %>%
    mutate(is_goal = maal == 1)
  
  ggplot() +
    annotate(
      "rect", xmin = 52.5, xmax = 105, ymin = 0, ymax = 68,
      fill = "#0f5f2e", colour = "white", linewidth = 0.8
    ) +
    annotate(
      "segment", x = 52.5, xend = 52.5, y = 0, yend = 68,
      colour = "white", linewidth = 0.7
    ) +
    annotate(
      "rect", xmin = 88.5, xmax = 105, ymin = 13.84, ymax = 54.16,
      fill = NA, colour = "white", linewidth = 0.7
    ) +
    annotate(
      "rect", xmin = 99.5, xmax = 105, ymin = 24.84, ymax = 43.16,
      fill = NA, colour = "white", linewidth = 0.7
    ) +
    annotate(
      "segment", x = 105, xend = 105, y = right_post_y, yend = left_post_y,
      colour = "white", linewidth = 1.1
    ) +
    annotate("point", x = 94, y = 34, colour = "white", size = 1.8) +
    geom_bin2d(
      data = df,
      aes(x = x_meter, y = y_meter),
      bins = 12,
      alpha = 0.55
    ) +
    geom_point(
      data = df,
      aes(x = x_meter, y = y_meter, size = xG_pred, shape = is_goal),
      colour = "#ffd400",
      alpha = 0.85,
      stroke = 1
    ) +
    scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16), guide = "none") +
    scale_size_continuous(range = c(2.5, 8), guide = "none") +
    coord_fixed() +
    labs(
      title = paste0("Shot map – ", team_label),
      subtitle = "Størrelse på punkt = xG | Fyldt punkt = mål",
      x = NULL,
      y = NULL
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

build_match_report_text <- function(match_row, brondby_shots, opp_shots, big_threshold = 0.20) {
  if (is.null(match_row) || nrow(match_row) == 0) return("Ingen kampdata fundet.")
  
  br_shots <- nrow(brondby_shots)
  op_shots <- nrow(opp_shots)
  
  br_big <- sum(brondby_shots$xG_pred >= big_threshold, na.rm = TRUE)
  op_big <- sum(opp_shots$xG_pred >= big_threshold, na.rm = TRUE)
  
  xg_balance <- match_row$xG_for - match_row$xG_against
  
  chance_text <- case_when(
    xg_balance > 0.30 ~ "Brøndby skabte samlet set flere og bedre chancer end modstanderen.",
    xg_balance < -0.30 ~ "Modstanderen skabte samlet set flere og bedre chancer end Brøndby.",
    TRUE ~ "Kampen var chance-mæssigt forholdsvis jævn."
  )
  
  volume_text <- case_when(
    br_shots > op_shots ~ "Brøndby havde flest afslutninger i kampen.",
    br_shots < op_shots ~ "Modstanderen havde flest afslutninger i kampen.",
    TRUE ~ "De to hold havde omtrent samme antal afslutninger."
  )
  
  big_text <- case_when(
    br_big > op_big ~ "Brøndby havde flest store chancer.",
    br_big < op_big ~ "Modstanderen havde flest store chancer.",
    TRUE ~ "Antallet af store chancer var nogenlunde lige."
  )
  
  result_text <- case_when(
    match_row$vurdering == "Overpræsteret" ~ "Resultatet var bedre end chancefordelingen tilsagde.",
    match_row$vurdering == "Underpræsteret" ~ "Resultatet var svagere end chancefordelingen tilsagde.",
    TRUE ~ "Resultatet lå tæt på det forventelige ud fra chancebilledet."
  )
  
  paste(chance_text, volume_text, big_text, result_text)
}

build_match_report_data <- function(match_row, shots_df) {
  if (is.null(match_row) || nrow(match_row) == 0 || is.null(shots_df)) return(NULL)
  
  match_id <- match_row$MATCH_WYID[1]
  brondby_name <- match_row$TEAMNAME[1]
  opponent_name <- match_row$opponent[1]
  
  match_shots <- shots_df %>%
    filter(MATCH_WYID == match_id)
  
  brondby_shots <- match_shots %>%
    filter(clean_teamname(TEAMNAME) == clean_teamname(brondby_name)) %>%
    valid_shot_for_model()
  
  opp_shots <- match_shots %>%
    filter(clean_teamname(TEAMNAME) == clean_teamname(opponent_name)) %>%
    valid_shot_for_model()
  
  player_col <- get_player_name_col(match_shots)
  
  scorers_tbl <- match_shots %>%
    filter(maal == 1, selvmaal == 0) %>%
    transmute(
      Minut = MINUTE,
      Hold = TEAMNAME,
      Spiller = as.character(.data[[player_col]]),
      xG = round(xG_pred, 3)
    ) %>%
    arrange(Minut)
  
  big_chances_tbl <- bind_rows(
    brondby_shots %>% mutate(Hold = brondby_name),
    opp_shots %>% mutate(Hold = opponent_name)
  ) %>%
    filter(!is.na(xG_pred), xG_pred >= 0.20) %>%
    transmute(
      Minut = MINUTE,
      Hold = TEAMNAME,
      Spiller = as.character(.data[[player_col]]),
      xG = round(xG_pred, 3),
      Mål = ifelse(maal == 1, "Ja", "Nej")
    ) %>%
    arrange(desc(xG), Minut)
  
  summary_tbl <- tibble(
    Nøgletal = c(
      "Resultat",
      "Brøndby xG",
      "Modstander xG",
      "Brøndby xP",
      "Brøndby afslutninger",
      "Modstander afslutninger",
      "Brøndby store chancer (xG >= 0.20)",
      "Modstander store chancer (xG >= 0.20)",
      "Vurdering"
    ),
    Værdi = c(
      paste0(match_row$goals_for[1], "-", match_row$goals_against[1]),
      sprintf("%.2f", match_row$xG_for[1]),
      sprintf("%.2f", match_row$xG_against[1]),
      sprintf("%.2f", match_row$xP[1]),
      nrow(brondby_shots),
      nrow(opp_shots),
      sum(brondby_shots$xG_pred >= 0.20, na.rm = TRUE),
      sum(opp_shots$xG_pred >= 0.20, na.rm = TRUE),
      match_row$vurdering[1]
    )
  )
  
  list(
    match_row = match_row,
    match_shots = match_shots,
    brondby_shots = brondby_shots,
    opp_shots = opp_shots,
    scorers_tbl = scorers_tbl,
    big_chances_tbl = big_chances_tbl,
    summary_tbl = summary_tbl,
    report_text = build_match_report_text(match_row, brondby_shots, opp_shots)
  )
}

shots_data <- list(
  `2024/25` = safe_read_rds("shots_superliga_2425.rds"),
  `2025/26` = safe_read_rds("shots_superliga_2526.rds")
)

xp_data <- list(
  `2024/25` = list(
    own = list(
      full = safe_read_rds("xp_vs_real_own_2425.rds"),
      gw22 = safe_read_rds("xp_vs_real_22_own_2425.rds")
    ),
    wyscout = list(
      full = safe_read_rds("xp_vs_real_wyscout_2425.rds"),
      gw22 = safe_read_rds("xp_vs_real_22_wyscout_2425.rds")
    ),
    compare = safe_read_rds("xp_model_compare_2425.rds")
  ),
  `2025/26` = list(
    gw18 = safe_read_rds("xp_vs_real_2526_18.rds"),
    proj22 = safe_read_rds("xp_projection_2526_22.rds"),
    mesterskab = safe_read_rds("result_mesterskab_2526.rds"),
    kval = safe_read_rds("result_kval_2526.rds")
  )
)

glm_model <- safe_read_rds("glm_2526_unweighted.rds")
if (is.null(glm_model)) glm_model <- safe_read_rds("glm_model_2526_compatible.rds")

model_metrics <- safe_read_rds("model_metrics.rds")
roc_data <- safe_read_rds("roc_data.rds")
coef_data <- safe_read_rds("coef_data.rds")
cm_glm_unweighted <- safe_read_rds("cm_glm_unweighted.rds")

player_xg_data <- list(
  `2024/25` = make_player_xg_table(shots_data[["2024/25"]]),
  `2025/26` = make_player_xg_table(shots_data[["2025/26"]])
)

brondby_all <- bind_rows(
  build_brondby_matches_from_shots(shots_data[["2024/25"]], "2024/25"),
  build_brondby_matches_from_shots(shots_data[["2025/26"]], "2025/26")
)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#0b1f3a",
    fg = "#ffffff",
    primary = "#ffd400",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  tags$head(
    tags$style(HTML("
      body { background: #0b1f3a !important; }
      .topbar {
        background: linear-gradient(90deg, #0b1f3a 0%, #123c7c 100%);
        color: white; padding: 14px 22px; border-bottom: 4px solid #ffd400; margin-bottom: 20px;
      }
      .brand-title { font-size: 28px; font-weight: 800; margin: 0; color: #ffd400; }
      .brand-subtitle { font-size: 14px; margin: 0; color: white; }
      .card-box {
        background: #f4f4f4; color: #0b1f3a; border-radius: 18px; padding: 18px;
        box-shadow: 0 8px 24px rgba(0,0,0,0.18); margin-bottom: 18px;
      }
      .section-title { color: #123c7c; font-weight: 800; margin-bottom: 10px; }
      .value-card {
        background: #123c7c; color: white; border-radius: 18px; padding: 18px;
        margin-bottom: 16px; border: 2px solid #ffd400;
      }
      .value-card h4 { color: #ffd400; margin-top: 0; }
      .metric-grid { display: grid; grid-template-columns: 1fr; gap: 12px; }
      .metric-box {
        background: #123c7c; color: white; border-radius: 16px; padding: 16px; border: 2px solid #ffd400;
      }
      .metric-label { font-size: 13px; text-transform: uppercase; letter-spacing: 0.08em; opacity: 0.85; }
      .metric-value { font-size: 30px; font-weight: 800; color: #ffd400; line-height: 1.1; margin-top: 4px; }
      .metric-sub { font-size: 13px; margin-top: 6px; opacity: 0.9; }
      .nav-pills .nav-link.active { background-color: #123c7c !important; color: white !important; }
      .nav-pills .nav-link { color: white !important; font-weight: 700; }
      .card-box .nav-pills .nav-link {
        color: #123c7c !important;
        background: #e9eef7 !important;
        border-radius: 10px;
        margin-right: 4px;
      }
      .card-box .nav-pills .nav-link.active {
        background: #123c7c !important;
        color: white !important;
      }
      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
        background: white !important; color: #0b1f3a !important;
      }
      .irs--shiny .irs-bar,
      .irs--shiny .irs-single,
      .irs--shiny .irs-from,
      .irs--shiny .irs-to {
        background: #123c7c; border-color: #123c7c;
      }
      .irs--shiny .irs-handle { border-color: #ffd400; }
      .report-link {
        display: inline-block;
        font-weight: 700;
        color: #123c7c;
        text-decoration: none;
        background: #e9eef7;
        border-radius: 8px;
        padding: 4px 8px;
      }
      .report-link:hover {
        background: #123c7c;
        color: white;
      }
    "))
  ),
  
  div(
    class = "topbar",
    tags$h1(class = "brand-title", "Brøndby IF – xG og xP"),
    tags$p(class = "brand-subtitle", "Et værktøj til at forstå chancer, kampbillede, forventede point og modelperformance")
  ),
  
  navset_pill(
    id = "main_tabs",
    
    nav_panel(
      "Forside",
      fluidRow(
        column(
          7,
          div(
            class = "card-box",
            h3(class = "section-title", "Hvad viser appen?"),
            p("Appen samler projektet om xG- og xP-modellering for Superligaen med Brøndby IF som case."),
            tags$ul(
              tags$li(tags$b("xG"), " viser sandsynligheden for scoring på en afslutning i åbent spil."),
              tags$li(tags$b("xP"), " viser forventede point ud fra kampens chancebillede."),
              tags$li(tags$b("Model performance"), " viser hvordan modellerne performer.")
            )
          )
        ),
        column(
          5,
          div(
            class = "value-card",
            h4("Hurtigt overblik"),
            p(strong("24/25 skud:"), if (!is.null(shots_data[["2024/25"]])) nrow(shots_data[["2024/25"]]) else "Ingen data"),
            p(strong("25/26 skud:"), if (!is.null(shots_data[["2025/26"]])) nrow(shots_data[["2025/26"]]) else "Ingen data"),
            p(strong("Brøndby kampdata:"), if (!is.null(brondby_all)) nrow(brondby_all) else 0)
          )
        )
      )
    ),
    
    nav_panel(
      "Metode",
      div(
        class = "card-box",
        h3(class = "section-title", "Metodisk ramme"),
        tags$ol(
          tags$li("Afslutningsdata bruges til at estimere sandsynligheden for scoring på skudniveau."),
          tags$li("Tre klassifikationsmodeller er konstrueret og sammenlignet."),
          tags$li("Den bedst performende model er valgt som xG-model."),
          tags$li("Den uvægtede logistiske regression bruges som endelig xG-model, fordi xG skal fortolkes som en sandsynlighed."),
          tags$li("De samlede xG-værdier for hvert hold i en kamp bruges som input i en Poisson-baseret model til beregning af forventede point (xP).")
        ),
        h4("Centrale variable"),
        tags$ul(
          tags$li("Afstand til mål"),
          tags$li("Vinkel mod mål"),
          tags$li("Kontraangreb"),
          tags$li("Hjørnesparksfase")
        ),
        h4("Afgrænsning"),
        tags$ul(
          tags$li("Straffespark indgår ikke i modellen"),
          tags$li("Frispark indgår ikke i modellen"),
          tags$li("Selvmål og own goals indgår ikke i xG-beregningen"),
          tags$li("Skud fra 35 meter eller længere er også sorteret fra")
        )
      )
    ),
    
    nav_panel(
      "Model performance",
      fluidRow(
        column(
          4,
          div(
            class = "card-box",
            h3(class = "section-title", "Performance-metrics"),
            DTOutput("model_metrics_table")
          )
        ),
        column(
          8,
          div(
            class = "card-box",
            h3(class = "section-title", "xG Performance"),
            p("Her sammenlignes de testede xG-modeller på tværs af centrale performance-mål. Formålet er at vurdere, hvilken model der bedst estimerer sandsynligheden for scoring."),
            plotlyOutput("model_metrics_plot", height = "320px")
          )
        )
      ),
      fluidRow(
        column(
          6,
          div(class = "card-box", h3(class = "section-title", "ROC-kurve"), plotlyOutput("roc_plot", height = "350px"))
        ),
        column(
          6,
          div(class = "card-box", h3(class = "section-title", "Confusion matrix"), plotlyOutput("cm_plot", height = "350px"))
        )
      ),
      div(class = "card-box", h3(class = "section-title", "Koefficienter / feature-effekter"), plotlyOutput("coef_plot", height = "350px"))
    ),
    
    nav_panel(
      "xG",
      fluidRow(
        column(
          3,
          div(
            class = "card-box",
            h3(class = "section-title", "Vælg sæson"),
            selectInput("xg_season", NULL, choices = c("2024/25", "2025/26"), selected = "2024/25")
          ),
          div(
            class = "card-box",
            h3(class = "section-title", "Skud-scenarie"),
            selectInput("xg_zone", "Vælg en zone", choices = names(shot_zone_presets), selected = "Centralt i feltet"),
            sliderInput("xg_distance_to_goal", "Direkte afstand til mål", min = 0, max = 60, value = 13, step = 0.5),
            sliderInput("xg_width", "Bredde (0 = venstre, 34 = centrum, 68 = højre)", min = 0, max = pitch_width, value = 34, step = 0.5),
            checkboxInput("xg_kontra", "Skuddet kommer fra et kontraangreb", FALSE),
            checkboxInput("xg_corner", "Skuddet kommer efter et hjørnespark", FALSE)
          ),
          div(
            class = "card-box",
            h3(class = "section-title", "Skud scenarie"),
            tableOutput("xg_scenario_table")
          )
        ),
        column(
          5,
          div(
            class = "card-box",
            h3(class = "section-title", "Skudvisualisering"),
            plotOutput("pitch_plot", height = "520px")
          ),
          div(
            class = "card-box",
            h3(class = "section-title", "Model-forklaring"),
            textOutput("xg_explanation")
          )
        ),
        column(
          4,
          div(
            class = "card-box",
            h3(class = "section-title", "Nøgletal"),
            uiOutput("xg_metric_cards")
          ),
          div(
            class = "card-box",
            h3(class = "section-title", "xG Stats"),
            navset_pill(
              id = "xg_scope_tabs",
              
              nav_panel(
                "Brøndby IF",
                br(),
                navset_pill(
                  id = "xg_player_tabs_brondby",
                  nav_panel("Over xG", br(), DTOutput("table_overperformers_brondby")),
                  nav_panel("Under xG", br(), DTOutput("table_underperformers_brondby")),
                  nav_panel("Topscorere", br(), DTOutput("table_topscorers_brondby"))
                )
              ),
              
              nav_panel(
                "Hele ligaen",
                br(),
                navset_pill(
                  id = "xg_player_tabs_league",
                  nav_panel("Over xG", br(), DTOutput("table_overperformers_league")),
                  nav_panel("Under xG", br(), DTOutput("table_underperformers_league")),
                  nav_panel("Topscorere", br(), DTOutput("table_topscorers_league"))
                )
              )
            )
          )
        )
      )
    ),
    
    nav_panel(
      "xP",
      fluidRow(
        column(
          3,
          div(
            class = "card-box",
            h3(class = "section-title", "Vælg sæson"),
            selectInput("xp_season", NULL, choices = c("2024/25", "2025/26"), selected = "2024/25")
          ),
          conditionalPanel(
            condition = "input.xp_season == '2024/25'",
            div(
              class = "card-box",
              h3(class = "section-title", "Vælg model"),
              radioButtons(
                "xp_model_2425", NULL,
                choices = c("Vores egen model" = "own", "Wyscouts model" = "wyscout"),
                selected = "own"
              )
            )
          )
        ),
        column(
          9,
          conditionalPanel(
            condition = "input.xp_season == '2024/25'",
            div(class = "card-box", h3(class = "section-title", "xG Performance"), DTOutput("xp_model_compare_table")),
            div(class = "card-box", h3(class = "section-title", "Samlet sæsontabel"), DTOutput("xp_2425_table")),
            div(class = "card-box", h3(class = "section-title", "Efter 22 runder"), DTOutput("xp_2425_22_table"))
          ),
          conditionalPanel(
            condition = "input.xp_season == '2025/26'",
            div(class = "card-box", h3(class = "section-title", "Tabel efter 18 runder"), DTOutput("xp_2526_18_table")),
            div(class = "card-box", h3(class = "section-title", "Forudsigelse af grundspillet"), DTOutput("xp_2526_22_table")),
            fluidRow(
              column(6, div(class = "card-box", h3(class = "section-title", "Forudsigelse af Mesterskabsspil"), DTOutput("result_mesterskab_table"))),
              column(6, div(class = "card-box", h3(class = "section-title", "Forudsigelse af Kvalifikationsspil"), DTOutput("result_kval_table")))
            )
          )
        )
      )
    ),
    
    nav_panel(
      "Brøndby IF",
      fluidRow(
        column(
          3,
          div(
            class = "card-box",
            h3(class = "section-title", "Vælg sæson"),
            selectInput("brondby_season", NULL, choices = c("2024/25", "2025/26"), selected = "2024/25")
          ),
          div(
            class = "card-box",
            h3(class = "section-title", "Opsummering"),
            tableOutput("brondby_summary_table")
          )
        ),
        column(
          9,
          div(class = "card-box", h3(class = "section-title", "Brøndbys kampe"), DTOutput("brondby_matches_table")),
          div(class = "card-box", h3(class = "section-title", "Udvikling kamp for kamp"), plotlyOutput("brondby_plot", height = "350px")),
          div(class = "card-box", h3(class = "section-title", "xG-billede kamp for kamp"), plotlyOutput("brondby_xg_plot", height = "380px"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$xg_zone, {
    preset <- shot_zone_presets[[input$xg_zone]]
    updateSliderInput(session, "xg_distance_to_goal", value = round(compute_distance(preset["x"], preset["y"]), 1))
    updateSliderInput(session, "xg_width", value = unname(preset["y"]))
  }, ignoreInit = TRUE)
  
  observeEvent(input$xg_width, {
    req(input$xg_distance_to_goal)
    min_possible_distance <- abs(input$xg_width - goal_center_y)
    updateSliderInput(
      session, "xg_distance_to_goal",
      min = round(min_possible_distance, 1),
      max = 60,
      value = max(input$xg_distance_to_goal, min_possible_distance)
    )
  }, ignoreInit = TRUE)
  
  xg_player_data_league <- reactive({
    player_xg_data[[input$xg_season]]
  })
  
  xg_player_data_brondby <- reactive({
    req(shots_data[[input$xg_season]])
    
    brondby_name <- clean_teamname("Brøndby IF")
    
    shots_data[[input$xg_season]] %>%
      filter(clean_teamname(TEAMNAME) == brondby_name) %>%
      make_player_xg_table()
  })
  
  current_shot <- reactive({
    width_pos <- min(max(input$xg_width, 0), pitch_width)
    x_pos <- compute_x_from_direct_distance(input$xg_distance_to_goal, width_pos)
    
    list(
      x = x_pos,
      y = width_pos,
      horizontal_distance = goal_center_x - x_pos,
      direkte_afstand = compute_distance(x_pos, width_pos),
      width = width_pos,
      vinkel = compute_angle_deg(x_pos, width_pos),
      xg = predict_xg(glm_model, x_pos, width_pos, input$xg_kontra, input$xg_corner)
    )
  })
  
  output$pitch_plot <- renderPlot({
    shot <- current_shot()
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    
    par(mar = c(0.5, 0.5, 0.5, 0.5), xaxs = "i", yaxs = "i", bg = "#0f5f2e")
    plot.new()
    plot.window(xlim = c(0, 105), ylim = c(0, 68), asp = 1)
    
    rect(0, 0, 105, 68, col = "#0f5f2e", border = "white", lwd = 2)
    segments(halfway_line_x, 0, halfway_line_x, 68, col = "white", lwd = 2)
    
    theta_center <- seq(0, 2 * pi, length.out = 300)
    lines(
      halfway_line_x + 9.15 * cos(theta_center),
      goal_center_y + 9.15 * sin(theta_center),
      col = "white",
      lwd = 2
    )
    
    rect(88.5, 13.84, 105, 54.16, border = "white", lwd = 2)
    rect(99.5, 24.84, 105, 43.16, border = "white", lwd = 2)
    segments(105, right_post_y, 105, left_post_y, col = "white", lwd = 4)
    points(94, 34, pch = 16, col = "white", cex = 1.1)
    
    polygon(
      c(shot$x, 105, 105),
      c(shot$y, right_post_y, left_post_y),
      col = adjustcolor("#ffd400", alpha.f = 0.22),
      border = NA
    )
    segments(shot$x, shot$y, 105, left_post_y, col = "#ffd400", lwd = 2)
    segments(shot$x, shot$y, 105, right_post_y, col = "#ffd400", lwd = 2)
    segments(shot$x, shot$y, 105, 34, col = "#ffd400", lwd = 2, lty = 2)
    points(shot$x, shot$y, pch = 16, col = "#ffd400", cex = 1.7)
    
    text(
      x = max(2, min(103, ifelse(shot$x > 90, shot$x - 5, shot$x + 3))),
      y = max(2, min(66, ifelse(shot$y > 60, shot$y - 3, shot$y + 3))),
      labels = paste0("xG: ", ifelse(is.na(shot$xg), "NA", round(shot$xg, 3))),
      col = "white", cex = 0.95, font = 2
    )
  }, res = 96)
  
  output$xg_metric_cards <- renderUI({
    shot <- current_shot()
    div(
      class = "metric-grid",
      metric_box("Forventet målchance", ifelse(is.na(shot$xg), "NA", sprintf("%.3f", shot$xg))),
      metric_box("Direkte afstand til mål", sprintf("%.1f m", shot$direkte_afstand)),
      metric_box("Vandret afstand til mållinjen", sprintf("%.1f m", shot$horizontal_distance)),
      metric_box("Vinkel mod mål", sprintf("%.1f°", shot$vinkel))
    )
  })
  
  output$xg_scenario_table <- renderTable({
    shot <- current_shot()
    data.frame(
      Måling = c("Valgt zone", "Direkte afstand til mål", "Bredde", "Vandret afstand til mållinjen", "Vinkel mod mål", "Forventet målchance"),
      Værdi = c(
        input$xg_zone,
        paste0(round(shot$direkte_afstand, 1), " meter"),
        paste0(round(shot$width, 1), " meter"),
        paste0(round(shot$horizontal_distance, 1), " meter"),
        paste0(round(shot$vinkel, 1), " grader"),
        ifelse(is.na(shot$xg), "Kunne ikke beregnes", round(shot$xg, 3))
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "m")
  
  output$xg_explanation <- renderText({
    shot <- current_shot()
    get_xg_explanation(shot$direkte_afstand, shot$vinkel, shot$xg, input$xg_kontra, input$xg_corner)
  })
  
  output$table_overperformers_brondby <- renderDT({
    req(xg_player_data_brondby())
    make_dt(xg_player_data_brondby() %>% slice_max(mål_minus_xG, n = 10))
  })
  
  output$table_underperformers_brondby <- renderDT({
    req(xg_player_data_brondby())
    make_dt(xg_player_data_brondby() %>% slice_min(mål_minus_xG, n = 10))
  })
  
  output$table_topscorers_brondby <- renderDT({
    req(xg_player_data_brondby())
    make_dt(xg_player_data_brondby() %>% arrange(desc(mål), desc(xG)) %>% slice_head(n = 10))
  })
  
  output$table_overperformers_league <- renderDT({
    req(xg_player_data_league())
    make_dt(xg_player_data_league() %>% slice_max(mål_minus_xG, n = 10))
  })
  
  output$table_underperformers_league <- renderDT({
    req(xg_player_data_league())
    make_dt(xg_player_data_league() %>% slice_min(mål_minus_xG, n = 10))
  })
  
  output$table_topscorers_league <- renderDT({
    req(xg_player_data_league())
    make_dt(xg_player_data_league() %>% arrange(desc(mål), desc(xG)) %>% slice_head(n = 10))
  })
  
  xp_2425_data <- reactive(xp_data[["2024/25"]][[input$xp_model_2425]][["full"]])
  xp_2425_22_data <- reactive(xp_data[["2024/25"]][[input$xp_model_2425]][["gw22"]])
  
  output$xp_model_compare_table <- renderDT({
    req(xp_data[["2024/25"]][["compare"]])
    
    df <- xp_data[["2024/25"]][["compare"]] %>%
      mutate(
        mean_abs_point_diff = round(mean_abs_point_diff, 2),
        mean_abs_rank_diff = round(mean_abs_rank_diff, 2)
      ) %>%
      rename_if_exists(c(
        model = "Model",
        mean_abs_point_diff = "Gennemsnitlig pointafvigelse",
        mean_abs_rank_diff = "Gennemsnitlig placeringsafvigelse"
      ))
    
    make_dt(df, 5)
  })
  
  output$xp_2425_table <- renderDT({
    req(xp_2425_data())
    
    df <- xp_2425_data() %>%
      rename_if_exists(c(
        TEAMNAME = "Hold",
        model = "Model",
        xP_rank = "xP placering",
        xP_total = "xP Point",
        xP_pr_kamp = "xP Point pr. kamp",
        real_rank = "Placering",
        real_points = "Point",
        point_diff = "Point diff",
        rank_diff = "Placering diff",
        slutspil = "Slutspil"
      ))
    
    make_dt(df, 12)
  })
  
  output$xp_2425_22_table <- renderDT({
    req(xp_2425_22_data())
    
    df <- xp_2425_22_data() %>%
      rename_if_exists(c(
        TEAMNAME = "Hold",
        model = "Model",
        xP_rank_22 = "xP placering 22",
        xP_22 = "xP Point 22",
        xP_pr_kamp_22 = "xP Point pr. kamp 22",
        slutspil_xP = "Slutspil xP",
        slutspil = "Slutspil",
        korrekt_slutspil = "Korrekt slutspil"
      ))
    
    make_dt(df, 12)
  })
  
  output$xp_2526_18_table <- renderDT({
    req(xp_data[["2025/26"]][["gw18"]])
    
    df <- xp_data[["2025/26"]][["gw18"]] %>%
      drop_if_exists("kampe_spillet") %>%
      rename_if_exists(c(
        TEAMNAME = "Hold",
        xP_rank_18 = "xP placering 18",
        xP_18 = "xP Point 18",
        xP_pr_kamp_18 = "xP Point pr. kamp 18",
        real_rank_18 = "Placering 18",
        real_points_18 = "Point 18",
        diff_18 = "Point diff",
        rank_diff_18 = "Placering diff"
      ))
    
    make_dt(df, 12)
  })
  
  output$xp_2526_22_table <- renderDT({
    req(xp_data[["2025/26"]][["proj22"]])
    
    df <- xp_data[["2025/26"]][["proj22"]] %>%
      drop_if_exists(c("kampe_spillet", "future_matches", "korrekt_slutspil_22")) %>%
      rename_if_exists(c(
        TEAMNAME = "Hold",
        xP_rank_18 = "xP placering 18",
        xP_18 = "xP Point 18",
        xP_rank_22 = "xP placering 22",
        future_xP = "Point xP",
        xP_22 = "xP point 22",
        slutspil_xP_22 = "xP slutspil 22",
        real_rank_22 = "Placering 22",
        real_points_22 = "Point 22",
        real_slutspil_22 = "Slutspil 22",
        diff_22 = "Point diff",
        rank_diff_22 = "Placering diff"
      ))
    
    make_dt(df, 12)
  })
  
  output$result_mesterskab_table <- renderDT({
    req(xp_data[["2025/26"]][["mesterskab"]])
    
    df <- xp_data[["2025/26"]][["mesterskab"]] %>%
      drop_if_exists(c("future_matches", "future_xP", "real_rank_18")) %>%
      rename_if_exists(c(
        slutplacering_mesterskab = "xP Endelig placering",
        TEAMNAME = "Hold",
        gruppe = "Slutspil",
        real_points_18 = "Point 18",
        projected_points_22 = "xP point efter 22. runder",
        playoff_matches = "Kampe i slutspil",
        playoff_xP = "xP point i slutspil",
        projected_final_points = "xP endelig Point"
      ))
    
    make_dt(df, 6)
  })
  
  output$result_kval_table <- renderDT({
    req(xp_data[["2025/26"]][["kval"]])
    
    df <- xp_data[["2025/26"]][["kval"]] %>%
      drop_if_exists(c("future_matches", "future_xP", "real_rank_18")) %>%
      rename_if_exists(c(
        slutplacering_kval = "xP Endelig placering",
        TEAMNAME = "Hold",
        gruppe = "Slutspil",
        real_points_18 = "Point 18",
        projected_points_22 = "xP point efter 22. runder",
        playoff_matches = "Kampe i slutspil",
        playoff_xP = "xP point i slutspil",
        projected_final_points = "xP endelig Point"
      ))
    
    make_dt(df, 6)
  })
  
  brondby_data_current <- reactive({
    req(brondby_all)
    brondby_all %>%
      filter(season == input$brondby_season) %>%
      mutate(Gameweek_num = suppressWarnings(as.numeric(Gameweek))) %>%
      arrange(Gameweek_num, MATCH_WYID)
  })
  
  selected_report_match <- reactive({
    req(input$show_match_report)
    brondby_data_current() %>%
      filter(as.character(MATCH_WYID) == as.character(input$show_match_report)) %>%
      slice(1)
  })
  
  selected_match_report_data <- reactive({
    req(selected_report_match())
    build_match_report_data(selected_report_match(), shots_data[[input$brondby_season]])
  })
  
  observeEvent(input$show_match_report, {
    dat <- selected_match_report_data()
    req(dat)
    
    showModal(
      modalDialog(
        title = paste0(
          "Kamprapport: Brøndby IF vs ", dat$match_row$opponent[1],
          " | Runde ", dat$match_row$Gameweek[1]
        ),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Luk"),
        fluidRow(
          column(12, p(dat$report_text))
        ),
        fluidRow(
          column(
            4,
            h4("Nøgletal"),
            tableOutput("report_summary_table")
          ),
          column(
            8,
            h4("Målscorere"),
            DTOutput("report_scorers_table")
          )
        ),
        br(),
        fluidRow(
          column(6, plotOutput("report_brondby_plot", height = "320px")),
          column(6, plotOutput("report_opponent_plot", height = "320px"))
        ),
        br(),
        fluidRow(
          column(
            12,
            h4("Store chancer"),
            DTOutput("report_big_chances_table")
          )
        )
      )
    )
  })
  
  output$report_summary_table <- renderTable({
    dat <- selected_match_report_data()
    req(dat)
    dat$summary_tbl
  }, striped = TRUE, bordered = TRUE, spacing = "m")
  
  output$report_scorers_table <- renderDT({
    dat <- selected_match_report_data()
    req(dat)
    make_dt(dat$scorers_tbl, pageLength = 10)
  })
  
  output$report_big_chances_table <- renderDT({
    dat <- selected_match_report_data()
    req(dat)
    make_dt(dat$big_chances_tbl, pageLength = 10)
  })
  
  output$report_brondby_plot <- renderPlot({
    dat <- selected_match_report_data()
    req(dat)
    print(make_half_pitch_plot(dat$brondby_shots, "Brøndby"))
  })
  
  output$report_opponent_plot <- renderPlot({
    dat <- selected_match_report_data()
    req(dat)
    print(make_half_pitch_plot(dat$opp_shots, dat$match_row$opponent[1]))
  })
  
  output$brondby_summary_table <- renderTable({
    df <- brondby_data_current()
    data.frame(
      Mål = c("Antal kampe", "Samlet xP", "Samlet xG for", "Samlet xG imod", "Sejre"),
      Værdi = c(
        nrow(df),
        round(sum(df$xP, na.rm = TRUE), 2),
        round(sum(df$xG_for, na.rm = TRUE), 2),
        round(sum(df$xG_against, na.rm = TRUE), 2),
        sum(df$result == "Vundet", na.rm = TRUE)
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "m")
  
  output$brondby_matches_table <- renderDT({
    df <- brondby_data_current() %>%
      mutate(
        Kamprapport = paste0(
          '<a href="#" class="report-link" onclick="Shiny.setInputValue(',
          "'show_match_report', '", MATCH_WYID, "', {priority: 'event'})",
          '">👁 Vis</a>'
        )
      ) %>%
      transmute(
        Runde = Gameweek,
        Modstander = opponent,
        `Hjemme/Ude` = ifelse(SIDE == "home", "Hjemme", "Ude"),
        Resultat = result,
        `Mål for` = goals_for,
        `Mål imod` = goals_against,
        `Brøndby xG` = xG_for,
        `Modstander xG` = xG_against,
        xP = xP,
        Vurdering = vurdering,
        Kamprapport = Kamprapport
      )
    
    make_dt(df, pageLength = 12, escape = FALSE)
  })
  
  output$brondby_plot <- renderPlotly({
    df <- brondby_data_current()
    validate(need(nrow(df) > 0, "Ingen Brøndby-kampe fundet for den valgte sæson."))
    
    p <- ggplot(
      df,
      aes(
        x = Gameweek_num, y = xP, color = vurdering,
        text = paste(
          "Runde:", Gameweek,
          "<br>Modstander:", opponent,
          "<br>Resultat:", result,
          "<br>Brøndby xG:", xG_for,
          "<br>Modstander xG:", xG_against,
          "<br>xP:", xP,
          "<br>Vurdering:", vurdering
        )
      )
    ) +
      geom_line(color = "#123c7c", linewidth = 1) +
      geom_point(size = 3) +
      scale_color_manual(values = c(
        "Overpræsteret" = "#2ca02c",
        "Underpræsteret" = "#d62728",
        "Som forventet" = "#ffd400",
        "Ikke beregnet" = "#666666"
      )) +
      labs(x = "Runde", y = "Forventede point i kampen", color = "Vurdering") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$brondby_xg_plot <- renderPlotly({
    df <- brondby_data_current()
    validate(need(nrow(df) > 0, "Ingen Brøndby-kampe fundet for den valgte sæson."))
    
    df_long <- df %>%
      select(Gameweek, Gameweek_num, opponent, result, vurdering, xG_for, xG_against) %>%
      pivot_longer(c(xG_for, xG_against), names_to = "Serie", values_to = "xG") %>%
      mutate(Serie = ifelse(Serie == "xG_for", "Brøndby xG", "Modstander xG"))
    
    p <- ggplot(
      df_long,
      aes(
        x = Gameweek_num, y = xG, color = Serie, group = Serie,
        text = paste(
          "Runde:", Gameweek,
          "<br>Modstander:", opponent,
          "<br>Serie:", Serie,
          "<br>xG:", round(xG, 2),
          "<br>Resultat:", result,
          "<br>Vurdering:", vurdering
        )
      )
    ) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 3) +
      scale_color_manual(values = c("Brøndby xG" = "#123c7c", "Modstander xG" = "#d62728")) +
      labs(x = "Runde", y = "Samlet xG i kampen", color = NULL) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$model_metrics_table <- renderDT({
    validate(need(!is.null(model_metrics), "Filen 'model_metrics.rds' blev ikke fundet."))
    df <- model_metrics
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    if (length(numeric_cols)) df[numeric_cols] <- lapply(df[numeric_cols], round, 3)
    make_dt(df)
  })
  
  output$model_metrics_plot <- renderPlotly({
    validate(need(!is.null(model_metrics), "Filen 'model_metrics.rds' blev ikke fundet."))
    validate(need(all(c("model", "metric", "value") %in% names(model_metrics)), "model_metrics.rds skal indeholde kolonnerne: model, metric, value"))
    
    p <- ggplot(
      model_metrics,
      aes(
        x = metric, y = value, fill = model,
        text = paste("Model:", model, "<br>Metric:", metric, "<br>Værdi:", round(value, 3))
      )
    ) +
      geom_col(position = "dodge") +
      labs(x = NULL, y = "Værdi", fill = "Model") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$roc_plot <- renderPlotly({
    validate(need(!is.null(roc_data), "Filen 'roc_data.rds' blev ikke fundet."))
    validate(need(all(c("fpr", "tpr", "model") %in% names(roc_data)), "roc_data.rds skal indeholde kolonnerne: fpr, tpr, model"))
    
    df <- roc_data %>%
      mutate(fpr = as.numeric(fpr), tpr = as.numeric(tpr), model = as.character(model)) %>%
      filter(is.finite(fpr), is.finite(tpr), !is.na(model)) %>%
      group_by(model) %>%
      arrange(fpr, tpr, .by_group = TRUE) %>%
      ungroup()
    
    p <- ggplot(
      df,
      aes(
        x = fpr, y = tpr, color = model, group = model,
        text = paste("Model:", model, "<br>FPR:", round(fpr, 3), "<br>TPR:", round(tpr, 3))
      )
    ) +
      geom_path(linewidth = 1.1) +
      geom_abline(slope = 1, intercept = 0, linetype = 2, alpha = 0.6) +
      labs(x = "False Positive Rate", y = "True Positive Rate", color = "Model") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$cm_plot <- renderPlotly({
    validate(need(!is.null(cm_glm_unweighted), "Filen 'cm_glm_unweighted.rds' blev ikke fundet."))
    
    cm_df <- as.data.frame.matrix(cm_glm_unweighted$table)
    validate(need(nrow(cm_df) > 0 && ncol(cm_df) > 0, "Confusion matrix er tom."))
    
    cm_df$Forudsagt <- rownames(cm_df)
    
    cm_long <- cm_df %>%
      pivot_longer(
        cols = -Forudsagt,
        names_to = "Faktisk",
        values_to = "Antal"
      ) %>%
      mutate(
        Forudsagt = as.character(Forudsagt),
        Faktisk = as.character(Faktisk),
        Antal = as.numeric(Antal)
      )
    
    p <- ggplot(
      cm_long,
      aes(
        x = Faktisk,
        y = Forudsagt,
        fill = Antal,
        text = paste(
          "Faktisk klasse:", Faktisk,
          "<br>Forudsagt klasse:", Forudsagt,
          "<br>Antal:", Antal
        )
      )
    ) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = Antal), size = 6, fontface = "bold") +
      labs(
        x = "Faktisk klasse",
        y = "Forudsagt klasse"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid = element_blank(),
        axis.title = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$coef_plot <- renderPlotly({
    validate(need(!is.null(coef_data), "Filen 'coef_data.rds' blev ikke fundet."))
    validate(need(all(c("term", "estimate") %in% names(coef_data)), "coef_data.rds skal indeholde kolonnerne: term, estimate"))
    
    df <- coef_data %>%
      arrange(estimate) %>%
      mutate(term = factor(term, levels = term))
    
    p <- ggplot(
      df,
      aes(x = term, y = estimate, text = paste("Variabel:", term, "<br>Effekt:", round(estimate, 3)))
    ) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Koefficient / effekt") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)