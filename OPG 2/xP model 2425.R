# Opg. 1 Eksamens-case – xP-model 24/25
# Sammenligning mellem egen xG-model og Wyscout xG
##########################################################################################################################################

# Pakker
##########################################################################################################################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)

##########################################################################################################################################
# Indlæs data
##########################################################################################################################################

shots_superliga <- readRDS("shots_superliga_2425.rds")

##########################################################################################################################################
# Hjælpefunktioner
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

simulate_match <- function(df_match, n_sim = 10000) {
  team_name <- unique(df_match$TEAMNAME)
  
  if (length(team_name) != 2) return(NULL)
  
  team1 <- team_name[1]
  team2 <- team_name[2]
  
  shots_team1 <- df_match %>% filter(TEAMNAME == team1)
  shots_team2 <- df_match %>% filter(TEAMNAME == team2)
  
  goals_team1 <- replicate(n_sim, {
    sum(rbinom(nrow(shots_team1), 1, shots_team1$xG_xp))
  })
  
  goals_team2 <- replicate(n_sim, {
    sum(rbinom(nrow(shots_team2), 1, shots_team2$xG_xp))
  })
  
  wins_team1 <- mean(goals_team1 > goals_team2)
  draws      <- mean(goals_team1 == goals_team2)
  wins_team2 <- mean(goals_team2 > goals_team1)
  
  xP_team1 <- 3 * wins_team1 + draws
  xP_team2 <- 3 * wins_team2 + draws
  
  tibble(
    MATCH_WYID = unique(df_match$MATCH_WYID)[1],
    TEAMNAME = c(team1, team2),
    SIDE = c(unique(shots_team1$SIDE)[1], unique(shots_team2$SIDE)[1]),
    Gameweek = c(unique(shots_team1$Gameweek)[1], unique(shots_team2$Gameweek)[1]),
    xP = c(xP_team1, xP_team2),
    win_prob = c(wins_team1, wins_team2),
    draw_prob = c(draws, draws),
    loss_prob = c(wins_team2, wins_team1)
  )
}

build_xp_model <- function(data, xg_col, model_name, n_sim = 10000) {
  
  ######################################################################################################################################
  # 1. Estimér hjemmebanefordel for den valgte xG-kolonne
  ######################################################################################################################################
  
  home_away_summary <- data %>%
    filter(
      selvmaal == 0,
      PRIMARYTYPE != "penalty",
      !is.na(.data[[xg_col]]),
      SIDE %in% c("home", "away")
    ) %>%
    group_by(SIDE) %>%
    summarise(
      mål = sum(maal, na.rm = TRUE),
      xG = sum(.data[[xg_col]], na.rm = TRUE),
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
  
  cat("\n---", model_name, "---\n")
  cat("Databaseret hjemmebanefordel (home multiplier):", round(home_advantage, 4), "\n")
  cat("Databaseret udebanetilpasning (away multiplier):", round(away_adjustment, 4), "\n")
  
  ######################################################################################################################################
  # 2. Byg xP-shotdatasæt
  # Samme metodik som i din xG-model: ingen selvmål og ingen straffespark
  ######################################################################################################################################
  
  shots_xp <- data %>%
    filter(
      selvmaal == 0,
      PRIMARYTYPE != "penalty",
      !is.na(.data[[xg_col]]),
      SIDE %in% c("home", "away")
    ) %>%
    mutate(
      xG_base = .data[[xg_col]],
      xG_xp = case_when(
        SIDE == "home" ~ pmin(xG_base * home_advantage, 0.99),
        SIDE == "away" ~ pmin(xG_base * away_adjustment, 0.99),
        TRUE ~ xG_base
      )
    )
  
  ######################################################################################################################################
  # 3. Simuler alle kampe
  ######################################################################################################################################
  
  xp_results <- shots_xp %>%
    filter(!is.na(xG_xp)) %>%
    group_by(MATCH_WYID) %>%
    group_split() %>%
    lapply(simulate_match, n_sim = n_sim) %>%
    bind_rows()
  
  ######################################################################################################################################
  # 4. Samlet xP-tabel for hele sæsonen
  ######################################################################################################################################
  
  xp_table <- xp_results %>%
    group_by(TEAMNAME) %>%
    summarise(
      kampe = n(),
      xP_total = sum(xP, na.rm = TRUE),
      xP_pr_kamp = mean(xP, na.rm = TRUE),
      win_prob_sum = sum(win_prob, na.rm = TRUE),
      draw_prob_sum = sum(draw_prob, na.rm = TRUE),
      loss_prob_sum = sum(loss_prob, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(xP_total)) %>%
    mutate(
      xP_rank = rank(-xP_total, ties.method = "min"),
      xP_total = round(xP_total, 2),
      xP_pr_kamp = round(xP_pr_kamp, 3)
    )
  
  ######################################################################################################################################
  # 5. xP-tabel efter 22 runder
  ######################################################################################################################################
  
  xp_table_22 <- xp_results %>%
    filter(Gameweek != "slutspil") %>%
    group_by(TEAMNAME) %>%
    summarise(
      kampe_22 = n(),
      xP_22 = sum(xP, na.rm = TRUE),
      xP_pr_kamp_22 = mean(xP, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(xP_22)) %>%
    mutate(
      xP_rank_22 = row_number(),
      xP_22 = round(xP_22, 2),
      xP_pr_kamp_22 = round(xP_pr_kamp_22, 3),
      slutspil_xP = ifelse(xP_rank_22 <= 6, "Mesterskabsspillet", "Kvalifikationsspillet")
    )
  
  list(
    model_name = model_name,
    xg_col = xg_col,
    home_advantage = home_advantage,
    away_adjustment = away_adjustment,
    shots_xp = shots_xp,
    xp_results = xp_results,
    xp_table = xp_table,
    xp_table_22 = xp_table_22
  )
}

##########################################################################################################################################
# A. Find de to hold i hver kamp
##########################################################################################################################################

match_teams <- shots_superliga %>%
  distinct(MATCH_WYID, TEAMNAME, SIDE)

##########################################################################################################################################
# B. Almindelige mål: tæller for eget hold
##########################################################################################################################################

regular_goals <- shots_superliga %>%
  filter(maal == 1, selvmaal == 0) %>%
  group_by(MATCH_WYID, TEAMNAME) %>%
  summarise(goals = n(), .groups = "drop")

##########################################################################################################################################
# C. Selvmål: tæller til modstanderholdet
##########################################################################################################################################

own_goals <- shots_superliga %>%
  filter(selvmaal == 1) %>%
  select(MATCH_WYID, TEAMNAME) %>%
  left_join(
    match_teams %>% rename(scoring_team = TEAMNAME),
    by = "MATCH_WYID"
  ) %>%
  filter(scoring_team != TEAMNAME) %>%
  group_by(MATCH_WYID, scoring_team) %>%
  summarise(goals = n(), .groups = "drop") %>%
  rename(TEAMNAME = scoring_team)

##########################################################################################################################################
# D. Saml alle mål
##########################################################################################################################################

all_goals <- bind_rows(regular_goals, own_goals) %>%
  group_by(MATCH_WYID, TEAMNAME) %>%
  summarise(goals = sum(goals), .groups = "drop")

##########################################################################################################################################
# E. Sørg for at hold med 0 mål også kommer med
##########################################################################################################################################

match_results <- match_teams %>%
  left_join(all_goals, by = c("MATCH_WYID", "TEAMNAME")) %>%
  mutate(goals = coalesce(goals, 0L)) %>%
  pivot_wider(
    names_from = SIDE,
    values_from = c(TEAMNAME, goals),
    names_sep = "_"
  ) %>%
  mutate(
    outcome = case_when(
      goals_home > goals_away ~ "Hjemmesejr",
      goals_home < goals_away ~ "Udebanesejr",
      TRUE ~ "Uafgjort"
    )
  )

home_away_wins <- match_results %>%
  filter(outcome != "Uafgjort") %>%
  count(outcome) %>%
  mutate(share = n / sum(n))

plot_home_away_wins <- ggplot(home_away_wins, aes(x = outcome, y = share, fill = outcome)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(share, accuracy = 0.1)), vjust = -0.4, size = 5) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(home_away_wins$share) + 0.05)
  ) +
  labs(
    title = "Sejren går til hjemmebaneholdet i 61% af tilfældene",
    subtitle = "Uafgjorte kampe er udeladt",
    x = NULL,
    y = "Andel af sejre",
    caption = "Kilde = Wyscout",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

plot_home_away_wins

##########################################################################################################################################
# Konstruér Gameweek ud fra MATCH_WYID
##########################################################################################################################################

match_order <- shots_superliga %>%
  distinct(MATCH_WYID) %>%
  arrange(MATCH_WYID) %>%
  mutate(
    kamp_nr = row_number(),
    Gameweek = case_when(
      kamp_nr <= 132 ~ as.character(ceiling(kamp_nr / 6)),
      TRUE ~ "slutspil"
    )
  )

shots_superliga <- shots_superliga %>%
  select(-any_of("Gameweek")) %>%
  left_join(
    match_order %>% select(MATCH_WYID, Gameweek),
    by = "MATCH_WYID"
  )

##########################################################################################################################################
# Byg xP-modeller
##########################################################################################################################################

set.seed(123)
n_sim <- 10000

xp_own <- build_xp_model(
  data = shots_superliga,
  xg_col = "xG_pred",
  model_name = "Egen xG-model",
  n_sim = n_sim
)

xp_wyscout <- build_xp_model(
  data = shots_superliga,
  xg_col = "SHOTXG",
  model_name = "Wyscout xG-model",
  n_sim = n_sim
)

##########################################################################################################################################
# Læs Team rankings.csv korrekt
##########################################################################################################################################

team_rankings <- read.csv(
  "Team rankings.csv",
  sep = ";",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(team_rankings) <- trimws(names(team_rankings))

##########################################################################################################################################
# Find kolonner robust
##########################################################################################################################################

point_col <- names(team_rankings)[names(team_rankings) %in% c("Point", "Points", "Pts", "POINTS")][1]
slutspil_col <- names(team_rankings)[names(team_rankings) %in% c("Slutspil", "Slutspilstype", "Gruppe", "Playoff", "Fase")][1]

##########################################################################################################################################
# Byg reel tabel
##########################################################################################################################################

real_table <- team_rankings %>%
  rename(
    TEAMNAME = Hold,
    real_rank = Placering
  ) %>%
  mutate(
    TEAMNAME = trimws(TEAMNAME),
    TEAMNAME_clean = clean_teamname(TEAMNAME),
    real_rank = as.numeric(real_rank)
  )

if (!is.na(point_col)) {
  real_table <- real_table %>%
    mutate(real_points = as.numeric(.data[[point_col]]))
}

if (!is.na(slutspil_col)) {
  real_table <- real_table %>%
    mutate(slutspil = as.character(.data[[slutspil_col]]))
}

##########################################################################################################################################
# Merge med reel tabel - hele sæsonen
##########################################################################################################################################

xp_vs_real_own <- xp_own$xp_table %>%
  mutate(
    TEAMNAME = trimws(TEAMNAME),
    TEAMNAME_clean = clean_teamname(TEAMNAME)
  ) %>%
  left_join(
    real_table %>%
      select(any_of(c("TEAMNAME", "TEAMNAME_clean", "real_rank", "real_points", "slutspil"))),
    by = "TEAMNAME_clean",
    suffix = c("_xp", "_real")
  ) %>%
  mutate(
    TEAMNAME = coalesce(TEAMNAME_xp, TEAMNAME_real),
    point_diff = if ("real_points" %in% names(.)) round(real_points - xP_total, 2) else NA_real_,
    rank_diff = real_rank - xP_rank,
    model = "Egen xG"
  ) %>%
  select(
    TEAMNAME,
    model,
    xP_rank,
    xP_total,
    xP_pr_kamp,
    real_rank,
    real_points,
    point_diff,
    rank_diff,
    slutspil
  ) %>%
  arrange(xP_rank)

xp_vs_real_wyscout <- xp_wyscout$xp_table %>%
  mutate(
    TEAMNAME = trimws(TEAMNAME),
    TEAMNAME_clean = clean_teamname(TEAMNAME)
  ) %>%
  left_join(
    real_table %>%
      select(any_of(c("TEAMNAME", "TEAMNAME_clean", "real_rank", "real_points", "slutspil"))),
    by = "TEAMNAME_clean",
    suffix = c("_xp", "_real")
  ) %>%
  mutate(
    TEAMNAME = coalesce(TEAMNAME_xp, TEAMNAME_real),
    point_diff = if ("real_points" %in% names(.)) round(real_points - xP_total, 2) else NA_real_,
    rank_diff = real_rank - xP_rank,
    model = "Wyscout xG"
  ) %>%
  select(
    TEAMNAME,
    model,
    xP_rank,
    xP_total,
    xP_pr_kamp,
    real_rank,
    real_points,
    point_diff,
    rank_diff,
    slutspil
  ) %>%
  arrange(xP_rank)

##########################################################################################################################################
# Merge med reel tabel - KUN efter 22 runder
##########################################################################################################################################

xp_vs_real_22_own <- xp_own$xp_table_22 %>%
  mutate(
    TEAMNAME = trimws(TEAMNAME),
    TEAMNAME_clean = clean_teamname(TEAMNAME)
  ) %>%
  left_join(
    real_table %>%
      select(any_of(c("TEAMNAME", "TEAMNAME_clean", "slutspil"))),
    by = "TEAMNAME_clean",
    suffix = c("_xp", "_real")
  ) %>%
  mutate(
    TEAMNAME = coalesce(TEAMNAME_xp, TEAMNAME_real),
    korrekt_slutspil = ifelse(!is.na(slutspil), slutspil_xP == slutspil, NA),
    model = "Egen xG"
  ) %>%
  select(
    TEAMNAME,
    model,
    xP_rank_22,
    xP_22,
    xP_pr_kamp_22,
    slutspil_xP,
    slutspil,
    korrekt_slutspil
  ) %>%
  arrange(xP_rank_22)

xp_vs_real_22_wyscout <- xp_wyscout$xp_table_22 %>%
  mutate(
    TEAMNAME = trimws(TEAMNAME),
    TEAMNAME_clean = clean_teamname(TEAMNAME)
  ) %>%
  left_join(
    real_table %>%
      select(any_of(c("TEAMNAME", "TEAMNAME_clean", "slutspil"))),
    by = "TEAMNAME_clean",
    suffix = c("_xp", "_real")
  ) %>%
  mutate(
    TEAMNAME = coalesce(TEAMNAME_xp, TEAMNAME_real),
    korrekt_slutspil = ifelse(!is.na(slutspil), slutspil_xP == slutspil, NA),
    model = "Wyscout xG"
  ) %>%
  select(
    TEAMNAME,
    model,
    xP_rank_22,
    xP_22,
    xP_pr_kamp_22,
    slutspil_xP,
    slutspil,
    korrekt_slutspil
  ) %>%
  arrange(xP_rank_22)

##########################################################################################################################################
# Sammenligning mellem modellerne
##########################################################################################################################################

xp_model_compare <- bind_rows(
  xp_vs_real_own,
  xp_vs_real_wyscout
) %>%
  group_by(model) %>%
  summarise(
    mean_abs_point_diff = mean(abs(point_diff), na.rm = TRUE),
    mean_abs_rank_diff = mean(abs(rank_diff), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_abs_point_diff, mean_abs_rank_diff)

xp_model_compare_22 <- bind_rows(
  xp_vs_real_22_own,
  xp_vs_real_22_wyscout
) %>%
  group_by(model) %>%
  summarise(
    andel_korrekt_slutspil = mean(korrekt_slutspil, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(andel_korrekt_slutspil))

##########################################################################################################################################
# Print
##########################################################################################################################################

print(xp_vs_real_own)
print(xp_vs_real_wyscout)
print(xp_vs_real_22_own)
print(xp_vs_real_22_wyscout)
print(xp_model_compare)
print(xp_model_compare_22)

##########################################################################################################################################
# Kontrol
##########################################################################################################################################

cat("Antal hold i xp_vs_real_22_own:", nrow(xp_vs_real_22_own), "\n")
cat("Andel korrekt forudsagte slutspilsplaceringer - egen xG:",
    mean(xp_vs_real_22_own$korrekt_slutspil, na.rm = TRUE), "\n")

cat("Antal hold i xp_vs_real_22_wyscout:", nrow(xp_vs_real_22_wyscout), "\n")
cat("Andel korrekt forudsagte slutspilsplaceringer - Wyscout xG:",
    mean(xp_vs_real_22_wyscout$korrekt_slutspil, na.rm = TRUE), "\n")

##########################################################################################################################################
# Gem RDS-filer
##########################################################################################################################################

saveRDS(match_results, "match_results.rds")
saveRDS(home_away_wins, "home_away_wins.rds")
saveRDS(plot_home_away_wins, "plot_home_away_wins.rds")

saveRDS(xp_own$shots_xp, "shots_superliga_xp_own_2425.rds")
saveRDS(xp_own$xp_results, "xp_results_own_2425.rds")
saveRDS(xp_own$xp_table, "xp_table_own_2425.rds")
saveRDS(xp_own$xp_table_22, "xp_table_22_own_2425.rds")
saveRDS(xp_vs_real_own, "xp_vs_real_own_2425.rds")
saveRDS(xp_vs_real_22_own, "xp_vs_real_22_own_2425.rds")

saveRDS(xp_wyscout$shots_xp, "shots_superliga_xp_wyscout_2425.rds")
saveRDS(xp_wyscout$xp_results, "xp_results_wyscout_2425.rds")
saveRDS(xp_wyscout$xp_table, "xp_table_wyscout_2425.rds")
saveRDS(xp_wyscout$xp_table_22, "xp_table_22_wyscout_2425.rds")
saveRDS(xp_vs_real_wyscout, "xp_vs_real_wyscout_2425.rds")
saveRDS(xp_vs_real_22_wyscout, "xp_vs_real_22_wyscout_2425.rds")

saveRDS(xp_model_compare, "xp_model_compare_2425.rds")
saveRDS(xp_model_compare_22, "xp_model_compare_22_2425.rds")