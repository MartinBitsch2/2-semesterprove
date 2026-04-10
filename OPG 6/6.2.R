
women_events <- readRDS("women_events.rds")
# OPGAVE 6.2 – LOGISK POSITIVISME


# Pakker
library(dplyr)
library(tibble)
library(stringr)

# 1. Udvalgte features
features <- c(
  "match_id", "minute", "second",
  "team.name", "player.name", "type.name",
  "shot.outcome.name", "pass.outcome.name",
  "shot.statsbomb_xg", "location",
  "position.name", "under_pressure"
)

# 2. Manglende værdier
feature_overblik <- tibble(feature = features) %>%
  rowwise() %>%
  mutate(
    antal_manglende = sum(is.na(women_events[[feature]])),
    andel_manglende_pct = round(mean(is.na(women_events[[feature]])) * 100, 2)
  ) %>%
  ungroup()

feature_overblik

# 3. Eventtyper
women_events %>%
  count(type.name, sort = TRUE)

# 4. Logisk konsistens

# A: mål uden skud
maal_uden_skud <- women_events %>%
  filter(shot.outcome.name == "Goal" & type.name != "Shot")

# B: xG uden skud
xg_uden_skud <- women_events %>%
  filter(!is.na(shot.statsbomb_xg) & type.name != "Shot")

# C: skud uden outcome
skud_uden_outcome <- women_events %>%
  filter(type.name == "Shot" & is.na(shot.outcome.name))

# D: pass uden spiller
pass_uden_spiller <- women_events %>%
  filter(type.name == "Pass" & is.na(player.name))

# E: events uden location
events_uden_location <- women_events %>%
  filter(is.na(location)) %>%
  count(type.name, sort = TRUE)

# 5. Fordelinger

# Shot outcome
women_events %>%
  filter(type.name == "Shot") %>%
  count(shot.outcome.name, sort = TRUE)

# Pass outcome
women_events %>%
  filter(type.name == "Pass") %>%
  count(pass.outcome.name, sort = TRUE)

# Under pressure
women_events %>%
  count(under_pressure, sort = TRUE)

# 6. Prioriteringsliste
prioriteret_liste <- tibble(
  feature = c(
    "match_id","minute","second","team.name","player.name",
    "type.name","shot.outcome.name","pass.outcome.name",
    "location","position.name","under_pressure","shot.statsbomb_xg"
  ),
  prioritet = 1:12
)

prioriteret_liste %>%
  arrange(prioritet)
