# =========================================================
# Analyse af afleveringer, skud og possession
# Mænd vs. kvinder
# =========================================================

library(dplyr)

# =========================================================
# 1. Afleveringssucces
# =========================================================

# --- Mænd ---
passes_men <- EventsMen[EventsMen$type.name == "Pass", ]
pass_success_men <- mean(is.na(passes_men$pass.outcome.name))

# --- Kvinder ---
passes_women <- EventsWomen[EventsWomen$type.name == "Pass", ]
pass_success_women <- mean(is.na(passes_women$pass.outcome.name))

pass_success_men
# 0.8420486

pass_success_women
# 0.7626226


# =========================================================
# 2. Skud pr. kamp
# =========================================================

# --- Mænd ---
shots_per_game_men <- sum(EventsMen$type.name == "Shot") / 
  length(unique(EventsMen$matchId))

# --- Kvinder ---
shots_per_game_women <- sum(EventsWomen$type.name == "Shot") / 
  length(unique(EventsWomen$match_id))

shots_per_game_men
# 26.27451

shots_per_game_women
# 28.41935


# =========================================================
# 3. Conversion rate
# =========================================================

# --- Mænd ---
shots_men <- EventsMen[EventsMen$type.name == "Shot", ]
goals_men <- sum(shots_men$shot.outcome.name == "Goal", na.rm = TRUE)
conversion_men <- goals_men / nrow(shots_men)

# --- Kvinder ---
shots_women <- EventsWomen[EventsWomen$type.name == "Shot", ]
goals_women <- sum(shots_women$shot.outcome.name == "Goal", na.rm = TRUE)
conversion_women <- goals_women / nrow(shots_women)

conversion_men
# 0.09402985

conversion_women
# 0.1032917


# =========================================================
# 4. xG pr. skud
# =========================================================
# Vi kan ikke vurdere skud og conversion uden også at kigge på xG

# --- Mænd ---
xg_per_shot_men <- mean(EventsMen$shot.statsbomb_xg, na.rm = TRUE)

# --- Kvinder ---
xg_per_shot_women <- mean(EventsWomen$shot.statsbomb_xg, na.rm = TRUE)

xg_per_shot_men
# 0.1041826

xg_per_shot_women
# 0.1062265


# =========================================================
# 5. Redningsprocent for keepere
# =========================================================
# Her ser vi kun på skud på mål: Goal eller Saved

# --- Mænd ---
shots_men <- EventsMen[EventsMen$type.name == "Shot", ]

on_target_men <- shots_men[
  shots_men$shot.outcome.name %in% c("Goal", "Saved"),
]

save_pct_men <- sum(on_target_men$shot.outcome.name == "Saved") / 
  nrow(on_target_men)

# --- Kvinder ---
shots_women <- EventsWomen[EventsWomen$type.name == "Shot", ]

on_target_women <- shots_women[
  shots_women$shot.outcome.name %in% c("Goal", "Saved"),
]

save_pct_women <- sum(on_target_women$shot.outcome.name == "Saved") / 
  nrow(on_target_women)

save_pct_men
# 0.7149321

save_pct_women
# 0.6738351


# =========================================================
# 6. Gennemsnitligt antal afleveringer pr. possession
# =========================================================

# --- Mænd ---
passes_per_possession_men <- EventsMen %>%
  filter(type.name == "Pass") %>%
  group_by(matchId, possession) %>%
  summarise(n_passes = n(), .groups = "drop") %>%
  summarise(mean_passes = mean(n_passes)) %>%
  pull(mean_passes)

# --- Kvinder ---
passes_per_possession_women <- EventsWomen %>%
  filter(type.name == "Pass") %>%
  group_by(match_id, possession) %>%
  summarise(n_passes = n(), .groups = "drop") %>%
  summarise(mean_passes = mean(n_passes)) %>%
  pull(mean_passes)

passes_per_possession_men
# 7.137748

passes_per_possession_women
# 5.079528


# =========================================================
# 7. Andel af possessions med mindst ét skud
# =========================================================

# --- Mænd ---
shots_per_possession_men <- EventsMen %>%
  group_by(matchId, possession) %>%
  summarise(has_shot = any(type.name == "Shot"), .groups = "drop") %>%
  summarise(rate = mean(has_shot)) %>%
  pull(rate)

# --- Kvinder ---
shots_per_possession_women <- EventsWomen %>%
  group_by(match_id, possession) %>%
  summarise(has_shot = any(type.name == "Shot"), .groups = "drop") %>%
  summarise(rate = mean(has_shot)) %>%
  pull(rate)

shots_per_possession_men
# 0.1517181

shots_per_possession_women
# 0.129657