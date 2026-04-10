
# OPGAVE 6.1 – TOP 10 + SPILLERPROFIL

women_events <- readRDS("women_events.rds")

# Pakker
library(dplyr)
library(stringr)

# 1. Overblik over datasættet
str(women_events)
head(women_events)
names(women_events)[str_detect(names(women_events), "shot")]

# 2. Klargør data
women_events_clean <- women_events %>%
  filter(!is.na(player.name)) %>%
  mutate(
    maal = if_else(type.name == "Shot" & shot.outcome.name == "Goal", 1, 0, missing = 0),
    assist = if_else(pass.goal_assist == TRUE, 1, 0, missing = 0),
    xg = if_else(type.name == "Shot", shot.statsbomb_xg, 0, missing = 0),
    afslutning = if_else(type.name == "Shot", 1, 0, missing = 0),
    aflevering = if_else(type.name == "Pass", 1, 0, missing = 0),
    carry = if_else(type.name == "Carry", 1, 0, missing = 0),
    dribling = if_else(type.name == "Dribble", 1, 0, missing = 0)
  )

# 3. Top 10 spillere (baseret på mål)
top10_scorere <- women_events_clean %>%
  group_by(player.name) %>%
  summarise(
    hold = first(na.omit(team.name)),
    primær_position = names(sort(table(position.name), decreasing = TRUE))[1],
    maal = sum(maal, na.rm = TRUE),
    assists = sum(assist, na.rm = TRUE),
    xg = sum(xg, na.rm = TRUE),
    afslutninger = sum(afslutning, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(maal), desc(xg)) %>%
  slice(1:10)

top10_scorere

# 4. Vælg topscorer
valgt_spiller <- top10_scorere$player.name[1]

# 5. Spillerprofil
spiller_profil <- women_events_clean %>%
  filter(player.name == valgt_spiller) %>%
  summarise(
    hold = first(na.omit(team.name)),
    maal = sum(maal, na.rm = TRUE),
    assists = sum(assist, na.rm = TRUE),
    xg = sum(xg, na.rm = TRUE),
    afslutninger = sum(afslutning, na.rm = TRUE),
    afleveringer = sum(aflevering, na.rm = TRUE),
    carries = sum(carry, na.rm = TRUE),
    driblinger = sum(dribling, na.rm = TRUE)
  )

spiller_profil

# 6. Spillerens positioner
spiller_positioner <- women_events_clean %>%
  filter(player.name == valgt_spiller) %>%
  count(position.name, sort = TRUE)

spiller_positioner

# 7. Ekstra: alle scorere (overblik)
alle_scorere <- women_events_clean %>%
  group_by(player.name) %>%
  summarise(
    hold = first(na.omit(team.name)),
    maal = sum(maal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(maal))

alle_scorere

# 8. Tjek nr. 1
alle_scorere[1, ]

