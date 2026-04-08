# Opg. 1 Eksamens-case – xP-model 25/26 + fremskrivning til grundspil og slutspil
##########################################################################################################################################

# Pakker
##########################################################################################################################################
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

##########################################################################################################################################
# Indlæs data
##########################################################################################################################################

shots_superliga2526 <- readRDS("shots_superliga_2526.rds")

team_rankings2526 <- read.csv(
  "Team rankings2526.csv",
  sep = ";",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(team_rankings2526) <- trimws(names(team_rankings2526))

set.seed(123)
n_sim <- 10000

##########################################################################################################################################
# 0. Hjælpefunktioner
##########################################################################################################################################

clean_teamname <- function(x) {
  x <- trimws(x)
  x <- tolower(x)
  x <- gsub("\\s+", " ", x)
  x <- gsub("\\.", "", x)
  x <- gsub("-", " ", x)
  x <- gsub("^fc ", "", x)
  x <- gsub(" fc$", "", x)
  x <- gsub(" bk$", "", x)
  x <- gsub(" if$", "", x)
  trimws(x)
}

safe_rank <- function(x) {
  rank(-x, ties.method = "min", na.last = "keep")
}

simulate_played_match <- function(df_match, n_sim = 10000) {
  team_info <- df_match %>%
    distinct(TEAMNAME, SIDE)
  
  if (nrow(team_info) != 2) return(NULL)
  if (!all(c("home", "away") %in% team_info$SIDE)) return(NULL)
  
  home_team <- team_info %>% filter(SIDE == "home") %>% pull(TEAMNAME)
  away_team <- team_info %>% filter(SIDE == "away") %>% pull(TEAMNAME)
  
  if (length(home_team) != 1 || length(away_team) != 1) return(NULL)
  
  shots_home <- df_match %>% filter(TEAMNAME == home_team)
  shots_away <- df_match %>% filter(TEAMNAME == away_team)
  
  goals_home <- replicate(n_sim, {
    sum(rbinom(nrow(shots_home), 1, shots_home$xG_xp))
  })
  
  goals_away <- replicate(n_sim, {
    sum(rbinom(nrow(shots_away), 1, shots_away$xG_xp))
  })
  
  home_win <- mean(goals_home > goals_away)
  draw     <- mean(goals_home == goals_away)
  away_win <- mean(goals_away > goals_home)
  
  expected_home_goals <- mean(goals_home)
  expected_away_goals <- mean(goals_away)
  
  most_likely_outcome <- case_when(
    home_win >= draw & home_win >= away_win ~ "Hjemmesejr",
    away_win >= draw & away_win >= home_win ~ "Udebanesejr",
    TRUE ~ "Uafgjort"
  )
  
  list(
    team_level = tibble(
      MATCH_WYID = unique(df_match$MATCH_WYID)[1],
      TEAMNAME = c(home_team, away_team),
      SIDE = c("home", "away"),
      Gameweek = unique(df_match$Gameweek)[1],
      xP = c(3 * home_win + draw, 3 * away_win + draw),
      win_prob = c(home_win, away_win),
      draw_prob = c(draw, draw),
      loss_prob = c(away_win, home_win),
      xG_for = c(sum(shots_home$xG_xp, na.rm = TRUE), sum(shots_away$xG_xp, na.rm = TRUE)),
      shots_for = c(nrow(shots_home), nrow(shots_away)),
      match_type = "Spillet kamp"
    ),
    match_level = tibble(
      MATCH_WYID = unique(df_match$MATCH_WYID)[1],
      Gameweek = unique(df_match$Gameweek)[1],
      home_team = home_team,
      away_team = away_team,
      home_win_prob = home_win,
      draw_prob = draw,
      away_win_prob = away_win,
      expected_home_goals = expected_home_goals,
      expected_away_goals = expected_away_goals,
      expected_score = paste0(round(expected_home_goals, 2), " - ", round(expected_away_goals, 2)),
      most_likely_outcome = most_likely_outcome,
      match_type = "Spillet kamp"
    )
  )
}

simulate_future_match <- function(home_team, away_team, strength_tbl, home_advantage, away_adjustment, gameweek, stage_label, n_sim = 10000) {
  home_row <- strength_tbl %>% filter(TEAMNAME == home_team)
  away_row <- strength_tbl %>% filter(TEAMNAME == away_team)
  
  if (nrow(home_row) == 0 || nrow(away_row) == 0) return(NULL)
  
  lambda_home <- ((home_row$xG_for_pg + away_row$xG_against_pg) / 2) * home_advantage
  lambda_away <- ((away_row$xG_for_pg + home_row$xG_against_pg) / 2) * away_adjustment
  
  lambda_home <- pmax(lambda_home, 0.05)
  lambda_away <- pmax(lambda_away, 0.05)
  
  goals_home <- rpois(n_sim, lambda_home)
  goals_away <- rpois(n_sim, lambda_away)
  
  home_win <- mean(goals_home > goals_away)
  draw     <- mean(goals_home == goals_away)
  away_win <- mean(goals_away > goals_home)
  
  expected_home_goals <- mean(goals_home)
  expected_away_goals <- mean(goals_away)
  
  most_likely_outcome <- case_when(
    home_win >= draw & home_win >= away_win ~ "Hjemmesejr",
    away_win >= draw & away_win >= home_win ~ "Udebanesejr",
    TRUE ~ "Uafgjort"
  )
  
  list(
    team_level = tibble(
      MATCH_WYID = NA_real_,
      TEAMNAME = c(home_team, away_team),
      SIDE = c("home", "away"),
      Gameweek = gameweek,
      xP = c(3 * home_win + draw, 3 * away_win + draw),
      win_prob = c(home_win, away_win),
      draw_prob = c(draw, draw),
      loss_prob = c(away_win, home_win),
      xG_for = c(expected_home_goals, expected_away_goals),
      shots_for = NA_real_,
      match_type = stage_label
    ),
    match_level = tibble(
      MATCH_WYID = NA_real_,
      Gameweek = gameweek,
      home_team = home_team,
      away_team = away_team,
      home_win_prob = home_win,
      draw_prob = draw,
      away_win_prob = away_win,
      expected_home_goals = expected_home_goals,
      expected_away_goals = expected_away_goals,
      expected_score = paste0(round(expected_home_goals, 2), " - ", round(expected_away_goals, 2)),
      most_likely_outcome = most_likely_outcome,
      match_type = stage_label
    )
  )
}

##########################################################################################################################################
# 1. Lås 25/26-data fast
##########################################################################################################################################

cat("SEASON_WYID i filen:\n")
print(sort(unique(shots_superliga2526$SEASON_WYID)))

target_season_wyid <- 191611

shots_2526_base <- shots_superliga2526 %>%
  filter(SEASON_WYID == target_season_wyid)

if (nrow(shots_2526_base) == 0) {
  stop(
    paste0(
      "Ingen rækker fundet for SEASON_WYID == ", target_season_wyid,
      ". Tilgængelige værdier er: ",
      paste(sort(unique(shots_superliga2526$SEASON_WYID)), collapse = ", ")
    )
  )
}

cat("Sæson i 25/26-base:", paste(unique(shots_2526_base$SEASON_WYID), collapse = ", "), "\n")
cat("Antal rækker i 25/26-base:", nrow(shots_2526_base), "\n")
cat("Antal hold i 25/26-base:", n_distinct(shots_2526_base$TEAMNAME), "\n")

##########################################################################################################################################
# 2. Fjern ugyldige kampe
##########################################################################################################################################

valid_matches <- shots_2526_base %>%
  distinct(MATCH_WYID, TEAMNAME, SIDE) %>%
  group_by(MATCH_WYID) %>%
  summarise(
    n_teams = n_distinct(TEAMNAME),
    n_home = sum(SIDE == "home", na.rm = TRUE),
    n_away = sum(SIDE == "away", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_teams == 2, n_home == 1, n_away == 1)

shots_2526_base <- shots_2526_base %>%
  semi_join(valid_matches, by = "MATCH_WYID")

cat("Antal gyldige kampe efter strukturfilter:", n_distinct(shots_2526_base$MATCH_WYID), "\n")

##########################################################################################################################################
# 3. Konstruér Gameweek for 2025/2026
##########################################################################################################################################

match_order_2526 <- shots_2526_base %>%
  distinct(MATCH_WYID) %>%
  arrange(MATCH_WYID) %>%
  mutate(
    kamp_nr = row_number(),
    Gameweek = ceiling(kamp_nr / 6)
  )

shots_2526_xp_base <- shots_2526_base %>%
  select(-any_of("Gameweek")) %>%
  left_join(
    match_order_2526 %>% select(MATCH_WYID, Gameweek),
    by = "MATCH_WYID"
  )

n_matches_played <- n_distinct(shots_2526_xp_base$MATCH_WYID)
n_rounds_played <- n_matches_played / 6

cat("Spillede kampe i 2025/2026:", n_matches_played, "\n")
cat("Spillede runder i 2025/2026:", n_rounds_played, "\n")

##########################################################################################################################################
# 4. Brug kun runde 1-18 til observeret del
##########################################################################################################################################

shots_2526_matches_18 <- shots_2526_xp_base %>%
  filter(!is.na(Gameweek), Gameweek <= 18)

cat("Kampe til og med runde 18:", n_distinct(shots_2526_matches_18$MATCH_WYID), "\n")

##########################################################################################################################################
# 5. Definér modelrelevante skud til xP
# Samme shot-univers som xG-modellen: ingen selvmål, ingen straffespark, ingen frispark, ingen own_goal
##########################################################################################################################################

shots_2526_xp_18 <- shots_2526_matches_18 %>%
  filter(
    selvmaal == 0,
    PRIMARYTYPE != "penalty",
    PRIMARYTYPE != "free_kick",
    PRIMARYTYPE != "own_goal",
    !is.na(xG_pred),
    SIDE %in% c("home", "away")
  )

##########################################################################################################################################
# 6. Justér xG for hjemme/ude i 2025/2026
##########################################################################################################################################

home_away_summary <- shots_2526_xp_18 %>%
  group_by(SIDE) %>%
  summarise(
    mål = sum(maal, na.rm = TRUE),
    xG = sum(xG_pred, na.rm = TRUE),
    mål_pr_xG = ifelse(xG > 0, mål / xG, NA_real_),
    .groups = "drop"
  )

home_conv <- home_away_summary %>%
  filter(SIDE == "home") %>%
  pull(mål_pr_xG)

away_conv <- home_away_summary %>%
  filter(SIDE == "away") %>%
  pull(mål_pr_xG)

if (length(home_conv) == 0 || is.na(home_conv)) home_conv <- 1
if (length(away_conv) == 0 || is.na(away_conv)) away_conv <- 1

home_advantage <- sqrt(home_conv / away_conv)
away_adjustment <- 1 / home_advantage

cat("Databaseret hjemmebanefordel (home multiplier):", round(home_advantage, 4), "\n")
cat("Databaseret udebanetilpasning (away multiplier):", round(away_adjustment, 4), "\n")

shots_2526_xp_18 <- shots_2526_xp_18 %>%
  mutate(
    xG_xp = case_when(
      SIDE == "home" ~ pmin(xG_pred * home_advantage, 0.99),
      SIDE == "away" ~ pmin(xG_pred * away_adjustment, 0.99),
      TRUE ~ xG_pred
    )
  )

##########################################################################################################################################
# 7. Simuler alle spillede kampe til og med runde 18
##########################################################################################################################################

played_match_list <- shots_2526_xp_18 %>%
  group_by(MATCH_WYID) %>%
  group_split()

played_simulations <- played_match_list %>%
  map(simulate_played_match, n_sim = n_sim) %>%
  compact()

xp_results_2526_played <- played_simulations %>%
  map("team_level") %>%
  bind_rows() %>%
  distinct(MATCH_WYID, TEAMNAME, .keep_all = TRUE)

played_match_outcomes <- played_simulations %>%
  map("match_level") %>%
  bind_rows() %>%
  distinct(MATCH_WYID, .keep_all = TRUE) %>%
  arrange(Gameweek, MATCH_WYID)

##########################################################################################################################################
# 8. Kampniveau til beskrivende output (reelle mål inkl. selvmål)
##########################################################################################################################################

match_teams_18 <- shots_2526_matches_18 %>%
  distinct(MATCH_WYID, TEAMNAME, SIDE, Gameweek)

regular_goals_18 <- shots_2526_matches_18 %>%
  filter(maal == 1, selvmaal == 0) %>%
  group_by(MATCH_WYID, TEAMNAME) %>%
  summarise(goals = n(), .groups = "drop")

own_goals_18 <- shots_2526_matches_18 %>%
  filter(selvmaal == 1) %>%
  select(MATCH_WYID, TEAMNAME) %>%
  left_join(
    match_teams_18 %>% select(MATCH_WYID, TEAMNAME) %>% rename(scoring_team = TEAMNAME),
    by = "MATCH_WYID"
  ) %>%
  filter(scoring_team != TEAMNAME) %>%
  group_by(MATCH_WYID, scoring_team) %>%
  summarise(goals = n(), .groups = "drop") %>%
  rename(TEAMNAME = scoring_team)

all_goals_18 <- bind_rows(regular_goals_18, own_goals_18) %>%
  group_by(MATCH_WYID, TEAMNAME) %>%
  summarise(goals = sum(goals), .groups = "drop")

match_team_actual_18 <- match_teams_18 %>%
  left_join(all_goals_18, by = c("MATCH_WYID", "TEAMNAME")) %>%
  mutate(goals = coalesce(goals, 0L))

match_team_actual_18_wide <- match_team_actual_18 %>%
  select(MATCH_WYID, TEAMNAME, SIDE, Gameweek, goals) %>%
  pivot_wider(
    names_from = SIDE,
    values_from = c(TEAMNAME, goals),
    names_sep = "_"
  )

actual_match_long_18 <- bind_rows(
  match_team_actual_18_wide %>%
    transmute(
      MATCH_WYID,
      Gameweek,
      TEAMNAME = TEAMNAME_home,
      SIDE = "home",
      goals_for = goals_home,
      goals_against = goals_away,
      opponent = TEAMNAME_away
    ),
  match_team_actual_18_wide %>%
    transmute(
      MATCH_WYID,
      Gameweek,
      TEAMNAME = TEAMNAME_away,
      SIDE = "away",
      goals_for = goals_away,
      goals_against = goals_home,
      opponent = TEAMNAME_home
    )
)

xp_match_level_2526_18 <- xp_results_2526_played %>%
  left_join(
    actual_match_long_18,
    by = c("MATCH_WYID", "TEAMNAME", "SIDE", "Gameweek")
  )

##########################################################################################################################################
# 9. xP-tabel efter 18 runder
##########################################################################################################################################

xp_table_2526_18 <- xp_match_level_2526_18 %>%
  group_by(TEAMNAME) %>%
  summarise(
    kampe_spillet = n_distinct(MATCH_WYID),
    xP_18 = sum(xP, na.rm = TRUE),
    xP_pr_kamp_18 = mean(xP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(xP_18)) %>%
  mutate(
    xP_18 = round(xP_18, 2),
    xP_pr_kamp_18 = round(xP_pr_kamp_18, 3),
    xP_rank_18 = row_number()
  )

##########################################################################################################################################
# 10. Find de kampe der allerede er spillet i grundspillet
##########################################################################################################################################

played_fixtures <- shots_2526_matches_18 %>%
  distinct(MATCH_WYID, TEAMNAME, SIDE) %>%
  group_by(MATCH_WYID) %>%
  summarise(
    home_team = TEAMNAME[SIDE == "home"][1],
    away_team = TEAMNAME[SIDE == "away"][1],
    .groups = "drop"
  ) %>%
  filter(!is.na(home_team), !is.na(away_team)) %>%
  distinct(home_team, away_team)

##########################################################################################################################################
# 11. Byg fuldt grundspilsprogram
##########################################################################################################################################

all_teams <- shots_2526_matches_18 %>%
  distinct(TEAMNAME) %>%
  mutate(TEAMNAME_clean = clean_teamname(TEAMNAME)) %>%
  distinct(TEAMNAME_clean, .keep_all = TRUE) %>%
  pull(TEAMNAME) %>%
  sort()

full_fixture <- expand.grid(
  home_team = all_teams,
  away_team = all_teams,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  filter(home_team != away_team)

##########################################################################################################################################
# 12. Find de manglende kampe frem til runde 22
##########################################################################################################################################

missing_fixtures <- full_fixture %>%
  anti_join(
    played_fixtures,
    by = c("home_team", "away_team")
  ) %>%
  arrange(home_team, away_team)

cat("Antal manglende kampe fundet:", nrow(missing_fixtures), "\n")

##########################################################################################################################################
# 13. Tildel runder 19-22 til de manglende kampe
##########################################################################################################################################

if (nrow(missing_fixtures) != 24) {
  warning(
    paste0(
      "Der blev fundet ", nrow(missing_fixtures),
      " manglende kampe. Ved 18 runder burde der normalt mangle 24 kampe frem til runde 22."
    )
  )
}

future_rounds <- rep(19:22, each = 6, length.out = nrow(missing_fixtures))

missing_fixtures <- missing_fixtures %>%
  mutate(Gameweek = future_rounds)

##########################################################################################################################################
# 14. Estimér holdstyrker ud fra de spillede 18 runder
##########################################################################################################################################

team_match_xg <- shots_2526_xp_18 %>%
  group_by(MATCH_WYID, TEAMNAME, SIDE) %>%
  summarise(
    xG_for = sum(xG_xp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  distinct(MATCH_WYID, TEAMNAME, .keep_all = TRUE)

match_totals <- team_match_xg %>%
  group_by(MATCH_WYID) %>%
  summarise(
    match_xG = sum(xG_for, na.rm = TRUE),
    .groups = "drop"
  )

team_match_xg <- team_match_xg %>%
  left_join(match_totals, by = "MATCH_WYID") %>%
  mutate(
    xG_against = match_xG - xG_for
  )

team_strength <- team_match_xg %>%
  group_by(TEAMNAME) %>%
  summarise(
    kampe_spillet = n_distinct(MATCH_WYID),
    xG_for_pg = mean(xG_for, na.rm = TRUE),
    xG_against_pg = mean(xG_against, na.rm = TRUE),
    .groups = "drop"
  )

##########################################################################################################################################
# 15. Simuler de manglende kampe i grundspillet
##########################################################################################################################################

future_simulations <- pmap(
  list(
    home_team = missing_fixtures$home_team,
    away_team = missing_fixtures$away_team,
    gameweek = missing_fixtures$Gameweek
  ),
  function(home_team, away_team, gameweek) {
    simulate_future_match(
      home_team = home_team,
      away_team = away_team,
      strength_tbl = team_strength,
      home_advantage = home_advantage,
      away_adjustment = away_adjustment,
      gameweek = gameweek,
      stage_label = "Fremskrevet grundspil",
      n_sim = n_sim
    )
  }
) %>%
  compact()

future_team_level <- future_simulations %>%
  map("team_level") %>%
  bind_rows()

future_match_outcomes <- future_simulations %>%
  map("match_level") %>%
  bind_rows() %>%
  arrange(Gameweek, home_team, away_team)

##########################################################################################################################################
# 16. Fremskrevet xP-tabel efter 22 runder
##########################################################################################################################################

future_xp_table <- future_team_level %>%
  group_by(TEAMNAME) %>%
  summarise(
    future_xP = sum(xP, na.rm = TRUE),
    future_matches = n(),
    .groups = "drop"
  )

xp_table_2526_22 <- xp_table_2526_18 %>%
  left_join(future_xp_table, by = "TEAMNAME") %>%
  mutate(
    future_xP = replace_na(future_xP, 0),
    future_matches = replace_na(future_matches, 0L),
    xP_22 = xP_18 + future_xP
  ) %>%
  arrange(desc(xP_22)) %>%
  mutate(
    future_xP = round(future_xP, 2),
    xP_22 = round(xP_22, 2),
    xP_rank_22 = row_number(),
    slutspil_xP_22 = ifelse(xP_rank_22 <= 6, "Mesterskabsspillet", "Kvalifikationsspillet")
  )

##########################################################################################################################################
# 17. Find kolonner robust i Team rankings2526.csv
##########################################################################################################################################

point18_col <- names(team_rankings2526)[names(team_rankings2526) %in% c(
  "Point efter 18 runder", "Points18", "Point_18", "Points_18",
  "Points after 18", "Point after 18", "Point efter 18", "Points efter 18"
)][1]

point22_col <- names(team_rankings2526)[names(team_rankings2526) %in% c(
  "Point efter 22 runder", "Points22", "Point_22", "Points_22",
  "Points after 22", "Point after 22", "Point efter 22", "Points efter 22"
)][1]

slutspil22_col <- names(team_rankings2526)[names(team_rankings2526) %in% c(
  "Slutspil efter 22", "Slutspil", "Slutspilstype", "Gruppe", "Playoff", "Fase"
)][1]

##########################################################################################################################################
# 18. Byg reel tabel KUN fra CSV
##########################################################################################################################################

season_teams_clean <- shots_2526_matches_18 %>%
  distinct(TEAMNAME) %>%
  mutate(TEAMNAME_clean = clean_teamname(TEAMNAME)) %>%
  distinct(TEAMNAME_clean, .keep_all = TRUE)

real_table_2526 <- team_rankings2526 %>%
  rename(TEAMNAME = Hold) %>%
  mutate(
    TEAMNAME = trimws(TEAMNAME),
    TEAMNAME_clean = clean_teamname(TEAMNAME)
  )

if (!is.na(point18_col)) {
  real_table_2526 <- real_table_2526 %>%
    mutate(real_points_18 = as.numeric(.data[[point18_col]]))
}

if (!is.na(point22_col)) {
  real_table_2526 <- real_table_2526 %>%
    mutate(real_points_22 = as.numeric(.data[[point22_col]]))
}

if (!is.na(slutspil22_col)) {
  real_table_2526 <- real_table_2526 %>%
    mutate(real_slutspil_22 = as.character(.data[[slutspil22_col]]))
}

real_table_2526 <- real_table_2526 %>%
  semi_join(season_teams_clean, by = "TEAMNAME_clean") %>%
  mutate(
    real_rank_18 = if ("real_points_18" %in% names(.)) safe_rank(real_points_18) else NA_real_,
    real_rank_22 = if ("real_points_22" %in% names(.)) safe_rank(real_points_22) else NA_real_
  )

##########################################################################################################################################
# 19. Sammenligningstabel efter 18 runder
##########################################################################################################################################

xp_vs_real_2526_18 <- xp_table_2526_18 %>%
  mutate(
    TEAMNAME_clean = clean_teamname(TEAMNAME)
  ) %>%
  left_join(
    real_table_2526 %>%
      select(any_of(c("TEAMNAME", "TEAMNAME_clean", "real_points_18", "real_rank_18"))),
    by = "TEAMNAME_clean",
    suffix = c("_xp", "_real")
  ) %>%
  mutate(
    TEAMNAME = coalesce(TEAMNAME_xp, TEAMNAME_real),
    diff_18 = if ("real_points_18" %in% names(.)) round(real_points_18 - xP_18, 2) else NA_real_,
    rank_diff_18 = real_rank_18 - xP_rank_18
  ) %>%
  select(
    TEAMNAME,
    kampe_spillet,
    xP_rank_18,
    xP_18,
    xP_pr_kamp_18,
    real_rank_18,
    real_points_18,
    diff_18,
    rank_diff_18
  ) %>%
  arrange(xP_rank_18)

##########################################################################################################################################
# 20. Sammenligningstabel efter fremskrivning til 22 runder
##########################################################################################################################################

xp_projection_2526_22 <- xp_table_2526_22 %>%
  mutate(
    TEAMNAME_clean = clean_teamname(TEAMNAME)
  ) %>%
  left_join(
    real_table_2526 %>%
      select(any_of(c("TEAMNAME", "TEAMNAME_clean", "real_points_22", "real_rank_22", "real_slutspil_22"))),
    by = "TEAMNAME_clean",
    suffix = c("_xp", "_real")
  ) %>%
  mutate(
    TEAMNAME = coalesce(TEAMNAME_xp, TEAMNAME_real),
    diff_22 = if ("real_points_22" %in% names(.)) round(real_points_22 - xP_22, 2) else NA_real_,
    rank_diff_22 = real_rank_22 - xP_rank_22,
    korrekt_slutspil_22 = if ("real_slutspil_22" %in% names(.)) slutspil_xP_22 == real_slutspil_22 else NA
  ) %>%
  select(
    TEAMNAME,
    kampe_spillet,
    future_matches,
    xP_rank_18,
    xP_18,
    xP_rank_22,
    future_xP,
    xP_22,
    slutspil_xP_22,
    real_rank_22,
    real_points_22,
    real_slutspil_22,
    diff_22,
    rank_diff_22,
    korrekt_slutspil_22
  ) %>%
  arrange(xP_rank_22)

##########################################################################################################################################
# 21. Samlet kamptabel: spillede + fremskrevne grundspilskampe
##########################################################################################################################################

all_match_outcomes_2526 <- bind_rows(
  played_match_outcomes,
  future_match_outcomes
) %>%
  mutate(
    home_win_prob = round(home_win_prob, 3),
    draw_prob = round(draw_prob, 3),
    away_win_prob = round(away_win_prob, 3),
    expected_home_goals = round(expected_home_goals, 2),
    expected_away_goals = round(expected_away_goals, 2)
  ) %>%
  arrange(match_type, Gameweek, MATCH_WYID, home_team, away_team)

##########################################################################################################################################
# 22. Simulér Mesterskabsspillet og Kvalifikationsspillet
# Udgangspunkt: real_points_18 fra CSV + future_xP frem til 22 runder
##########################################################################################################################################

mesterskab_hold <- c(
  "Brøndby",
  "Nordsjælland",
  "Midtjylland",
  "SønderjyskE",
  "AGF",
  "Viborg"
)

projection_base_2526 <- xp_table_2526_22 %>%
  mutate(
    TEAMNAME_clean = clean_teamname(TEAMNAME)
  ) %>%
  left_join(
    real_table_2526 %>%
      select(any_of(c("TEAMNAME", "TEAMNAME_clean", "real_points_18", "real_rank_18"))),
    by = "TEAMNAME_clean",
    suffix = c("_xp", "_real")
  ) %>%
  mutate(
    TEAMNAME = coalesce(TEAMNAME_xp, TEAMNAME_real),
    real_points_18 = as.numeric(real_points_18),
    future_xP = replace_na(future_xP, 0),
    projected_points_22 = real_points_18 + future_xP
  ) %>%
  select(
    TEAMNAME,
    TEAMNAME_clean,
    kampe_spillet,
    xP_18,
    xP_rank_18,
    real_points_18,
    real_rank_18,
    future_matches,
    future_xP,
    projected_points_22,
    xP_22
  )

build_playoff_fixture <- function(team_vector, round_start = 23) {
  fixture <- expand.grid(
    home_team = team_vector,
    away_team = team_vector,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    filter(home_team != away_team) %>%
    arrange(home_team, away_team)
  
  fixture %>%
    mutate(Gameweek = rep(round_start:(round_start + 9), length.out = n()))
}

fixture_mesterskab <- build_playoff_fixture(mesterskab_hold, round_start = 23)

sim_mesterskab <- pmap(
  list(
    home_team = fixture_mesterskab$home_team,
    away_team = fixture_mesterskab$away_team,
    gameweek = fixture_mesterskab$Gameweek
  ),
  function(home_team, away_team, gameweek) {
    simulate_future_match(
      home_team = home_team,
      away_team = away_team,
      strength_tbl = team_strength,
      home_advantage = home_advantage,
      away_adjustment = away_adjustment,
      gameweek = gameweek,
      stage_label = "Fremskrevet Mesterskabsspil",
      n_sim = n_sim
    )
  }
) %>%
  compact()

mesterskab_team_level <- sim_mesterskab %>%
  map("team_level") %>%
  bind_rows()

mesterskab_match_outcomes <- sim_mesterskab %>%
  map("match_level") %>%
  bind_rows() %>%
  arrange(Gameweek, home_team, away_team)

playoff_xp_mesterskab <- mesterskab_team_level %>%
  group_by(TEAMNAME) %>%
  summarise(
    playoff_xP = sum(xP, na.rm = TRUE),
    playoff_matches = n(),
    .groups = "drop"
  )

result_mesterskab <- projection_base_2526 %>%
  filter(TEAMNAME %in% mesterskab_hold) %>%
  left_join(playoff_xp_mesterskab, by = "TEAMNAME") %>%
  mutate(
    playoff_xP = replace_na(playoff_xP, 0),
    playoff_matches = replace_na(playoff_matches, 0L),
    projected_final_points = projected_points_22 + playoff_xP
  ) %>%
  arrange(desc(projected_final_points), desc(xP_22), TEAMNAME) %>%
  mutate(
    projected_points_22 = round(projected_points_22, 2),
    playoff_xP = round(playoff_xP, 2),
    projected_final_points = round(projected_final_points, 2),
    slutplacering_mesterskab = row_number(),
    gruppe = "Mesterskabsspillet"
  ) %>%
  select(
    slutplacering_mesterskab,
    TEAMNAME,
    gruppe,
    real_rank_18,
    real_points_18,
    future_matches,
    future_xP,
    projected_points_22,
    playoff_matches,
    playoff_xP,
    projected_final_points
  )

kval_hold <- setdiff(sort(unique(projection_base_2526$TEAMNAME)), mesterskab_hold)

fixture_kval <- build_playoff_fixture(kval_hold, round_start = 23)

sim_kval <- pmap(
  list(
    home_team = fixture_kval$home_team,
    away_team = fixture_kval$away_team,
    gameweek = fixture_kval$Gameweek
  ),
  function(home_team, away_team, gameweek) {
    simulate_future_match(
      home_team = home_team,
      away_team = away_team,
      strength_tbl = team_strength,
      home_advantage = home_advantage,
      away_adjustment = away_adjustment,
      gameweek = gameweek,
      stage_label = "Fremskrevet Kvalifikationsspil",
      n_sim = n_sim
    )
  }
) %>%
  compact()

kval_team_level <- sim_kval %>%
  map("team_level") %>%
  bind_rows()

kval_match_outcomes <- sim_kval %>%
  map("match_level") %>%
  bind_rows() %>%
  arrange(Gameweek, home_team, away_team)

playoff_xp_kval <- kval_team_level %>%
  group_by(TEAMNAME) %>%
  summarise(
    playoff_xP = sum(xP, na.rm = TRUE),
    playoff_matches = n(),
    .groups = "drop"
  )

result_kval <- projection_base_2526 %>%
  filter(TEAMNAME %in% kval_hold) %>%
  left_join(playoff_xp_kval, by = "TEAMNAME") %>%
  mutate(
    playoff_xP = replace_na(playoff_xP, 0),
    playoff_matches = replace_na(playoff_matches, 0L),
    projected_final_points = projected_points_22 + playoff_xP
  ) %>%
  arrange(desc(projected_final_points), desc(xP_22), TEAMNAME) %>%
  mutate(
    projected_points_22 = round(projected_points_22, 2),
    playoff_xP = round(playoff_xP, 2),
    projected_final_points = round(projected_final_points, 2),
    slutplacering_kval = row_number(),
    gruppe = "Kvalifikationsspillet"
  ) %>%
  select(
    slutplacering_kval,
    TEAMNAME,
    gruppe,
    real_rank_18,
    real_points_18,
    future_matches,
    future_xP,
    projected_points_22,
    playoff_matches,
    playoff_xP,
    projected_final_points
  )

##########################################################################################################################################
# 23. Brøndby-data til Shiny
##########################################################################################################################################

brondby_match_xp_2526 <- bind_rows(
  xp_match_level_2526_18 %>%
    mutate(
      over_under_label = case_when(
        is.na(goals_for) | is.na(goals_against) ~ "Ukendt",
        goals_for > goals_against & xP < 1.5 ~ "Overpræsteret",
        goals_for < goals_against & xP > 1.5 ~ "Underpræsteret",
        TRUE ~ "Som forventet"
      )
    ),
  future_team_level %>%
    mutate(
      goals_for = NA_real_,
      goals_against = NA_real_,
      opponent = NA_character_,
      over_under_label = "Fremskrevet grundspil"
    ),
  mesterskab_team_level %>%
    mutate(
      goals_for = NA_real_,
      goals_against = NA_real_,
      opponent = NA_character_,
      over_under_label = "Fremskrevet Mesterskabsspil"
    ),
  kval_team_level %>%
    mutate(
      goals_for = NA_real_,
      goals_against = NA_real_,
      opponent = NA_character_,
      over_under_label = "Fremskrevet Kvalifikationsspil"
    )
) %>%
  filter(TEAMNAME == "Brøndby IF") %>%
  mutate(
    xP = round(xP, 3),
    xG_for = round(xG_for, 3)
  ) %>%
  arrange(Gameweek, MATCH_WYID)

brondby_summary_2526 <- brondby_match_xp_2526 %>%
  summarise(
    kampe_spillede = sum(match_type == "Spillet kamp", na.rm = TRUE),
    kampe_grundspil_total = sum(match_type %in% c("Spillet kamp", "Fremskrevet grundspil"), na.rm = TRUE),
    kampe_total_inkl_slutspil = n(),
    xP_spillede = round(sum(xP[match_type == "Spillet kamp"], na.rm = TRUE), 2),
    xP_grundspil_total = round(sum(xP[match_type %in% c("Spillet kamp", "Fremskrevet grundspil")], na.rm = TRUE), 2),
    xP_total_inkl_slutspil = round(sum(xP, na.rm = TRUE), 2),
    xG_spillede = round(sum(xG_for[match_type == "Spillet kamp"], na.rm = TRUE), 2)
  )

##########################################################################################################################################
# 24. Plotdatasæt til Shiny
##########################################################################################################################################

xp_plot_data_2526_18 <- xp_vs_real_2526_18 %>%
  mutate(
    over_under = case_when(
      diff_18 > 0 ~ "Overpræsteret",
      diff_18 < 0 ~ "Underpræsteret",
      TRUE ~ "Som forventet"
    )
  )

xp_plot_data_2526_22 <- xp_projection_2526_22 %>%
  mutate(
    projected_group = slutspil_xP_22
  )

##########################################################################################################################################
# 25. Output
##########################################################################################################################################

print(xp_table_2526_18)
print(xp_vs_real_2526_18)
print(xp_table_2526_22)
print(xp_projection_2526_22)
print(result_mesterskab)
print(result_kval)
print(all_match_outcomes_2526)
print(brondby_match_xp_2526)
print(brondby_summary_2526)

##########################################################################################################################################
# 26. Hurtig kontrol
##########################################################################################################################################

cat("Sæson i arbejdsdata:", paste(unique(shots_2526_xp_base$SEASON_WYID), collapse = ", "), "\n")
cat("Antal hold:", n_distinct(shots_2526_matches_18$TEAMNAME), "\n")
cat("Antal spillede kampe:", n_distinct(shots_2526_matches_18$MATCH_WYID), "\n")
cat("Antal spillede runder:", max(shots_2526_matches_18$Gameweek, na.rm = TRUE), "\n")
cat("Antal manglende kampe til runde 22:", nrow(missing_fixtures), "\n")

cat("Kampe pr. hold efter 18 runder:\n")
print(
  xp_table_2526_18 %>%
    select(TEAMNAME, kampe_spillet) %>%
    arrange(desc(kampe_spillet), TEAMNAME)
)

if ("real_points_18" %in% names(xp_vs_real_2526_18)) {
  cat("Antal hold matchet med point efter 18 runder:", sum(!is.na(xp_vs_real_2526_18$real_points_18)), "\n")
}

if ("real_points_22" %in% names(xp_projection_2526_22)) {
  cat("Antal hold matchet med point efter 22 runder:", sum(!is.na(xp_projection_2526_22$real_points_22)), "\n")
}

##########################################################################################################################################
# 27. Gem RDS-filer til Shiny
##########################################################################################################################################

saveRDS(match_order_2526, "match_order_2526.rds")
saveRDS(shots_2526_xp_18, "shots_superliga_xp_2526_18.rds")
saveRDS(xp_results_2526_played, "xp_results_2526_played.rds")
saveRDS(xp_match_level_2526_18, "xp_match_level_2526_18.rds")
saveRDS(xp_table_2526_18, "xp_table_2526_18.rds")
saveRDS(xp_vs_real_2526_18, "xp_vs_real_2526_18.rds")
saveRDS(future_match_outcomes, "future_match_outcomes_2526.rds")
saveRDS(future_team_level, "future_team_level_2526.rds")
saveRDS(xp_table_2526_22, "xp_table_2526_22.rds")
saveRDS(xp_projection_2526_22, "xp_projection_2526_22.rds")
saveRDS(all_match_outcomes_2526, "all_match_outcomes_2526.rds")
saveRDS(result_mesterskab, "result_mesterskab_2526.rds")
saveRDS(result_kval, "result_kval_2526.rds")
saveRDS(mesterskab_match_outcomes, "mesterskab_match_outcomes_2526.rds")
saveRDS(kval_match_outcomes, "kval_match_outcomes_2526.rds")
saveRDS(brondby_match_xp_2526, "brondby_match_xp_2526.rds")
saveRDS(brondby_summary_2526, "brondby_summary_2526.rds")
saveRDS(xp_plot_data_2526_18, "xp_plot_data_2526_18.rds")
saveRDS(xp_plot_data_2526_22, "xp_plot_data_2526_22.rds")