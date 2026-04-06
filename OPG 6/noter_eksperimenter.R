
# NOTER / EKSPERIMENTER
# Bruges til test, ekstra analyser og forståelse


# Pakker
library(dplyr)
library(stringr)
library(tidyr)


# 1. Overblik over datasæt


str(women_events)
head(women_events)

# Se shot variabler
names(women_events)[str_detect(names(women_events), "shot")]


# 2. Alternativ top 10 (offensiv score)


top10_offensiv <- women_events %>%
  filter(!is.na(player.name)) %>%
  mutate(
    maal = if_else(type.name == "Shot" & shot.outcome.name == "Goal", 1, 0, missing = 0),
    assist = if_else(pass.goal_assist == TRUE, 1, 0, missing = 0),
    xg = if_else(type.name == "Shot", shot.statsbomb_xg, 0, missing = 0),
    afslutning = if_else(type.name == "Shot", 1, 0, missing = 0)
  ) %>%
  group_by(player.name, team.name, position.name) %>%
  summarise(
    maal = sum(maal, na.rm = TRUE),
    assists = sum(assist, na.rm = TRUE),
    xg = sum(xg, na.rm = TRUE),
    afslutninger = sum(afslutning, na.rm = TRUE),
    offensiv_score = maal * 4 + assists * 3 + xg,
    .groups = "drop"
  ) %>%
  arrange(desc(offensiv_score)) %>%
  slice(1:10)

top10_offensiv


# 3. Tjek specifikke spillere


# Find navn korrekt
women_events %>%
  filter(str_detect(player.name, "Nadim")) %>%
  distinct(player.name)

women_events %>%
  filter(str_detect(player.name, "Harder")) %>%
  distinct(player.name)

# Nadia Nadim
spiller <- "Nadia Nadim"

women_events_clean %>%
  filter(player.name == spiller) %>%
  summarise(
    maal = sum(maal, na.rm = TRUE),
    assists = sum(assist, na.rm = TRUE),
    xg = sum(xg, na.rm = TRUE),
    afslutninger = sum(afslutning, na.rm = TRUE),
    afleveringer = sum(aflevering, na.rm = TRUE),
    carries = sum(carry, na.rm = TRUE),
    driblinger = sum(dribling, na.rm = TRUE)
  )

# Pernille Harder
spiller <- "Pernille Mosegaard Harder"

women_events_clean %>%
  filter(player.name == spiller) %>%
  summarise(
    maal = sum(maal, na.rm = TRUE),
    assists = sum(assist, na.rm = TRUE),
    xg = sum(xg, na.rm = TRUE),
    afslutninger = sum(afslutning, na.rm = TRUE),
    afleveringer = sum(aflevering, na.rm = TRUE),
    carries = sum(carry, na.rm = TRUE),
    driblinger = sum(dribling, na.rm = TRUE)
  )


# 4. Korrekt topscorerliste


top_scorere_korrekt <- women_events %>%
  filter(type.name == "Shot",
         shot.outcome.name == "Goal",
         !is.na(player.name)) %>%
  group_by(player.name) %>%
  summarise(
    maal = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(maal))

top_scorere_korrekt

# Tjek Alexandra Popp specifikt
women_events %>%
  filter(player.name == "Alexandra Popp",
         type.name == "Shot",
         shot.outcome.name == "Goal") %>%
  nrow()


# 5. Sammenligning mellem turneringer


# Kobler eventdata med kampdata
women_events_turnering <- women_events %>%
  left_join(
    all_matches %>%
      select(match_id,
             competition.competition_name,
             season.season_name),
    by = "match_id"
  )

# Mål pr spiller pr turnering
maal_pr_turnering <- women_events_turnering %>%
  filter(type.name == "Shot",
         shot.outcome.name == "Goal",
         !is.na(player.name)) %>%
  group_by(player.name, competition.competition_name, season.season_name) %>%
  summarise(
    maal = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(maal))

maal_pr_turnering


# 6. Top 10 spillere fordelt på EM og VM


top10_navne <- top10_scorere$player.name

maal_top10_total <- women_events_turnering %>%
  filter(player.name %in% top10_navne,
         type.name == "Shot",
         shot.outcome.name == "Goal") %>%
  mutate(turnering = case_when(
    competition.competition_name == "UEFA Women's Euro" &
      season.season_name == "2022" ~ "EM2022",
    competition.competition_name == "Women's World Cup" &
      season.season_name == "2023" ~ "VM2023"
  )) %>%
  group_by(player.name, turnering) %>%
  summarise(maal = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = turnering,
    values_from = maal,
    values_fill = 0
  ) %>%
  mutate(total = EM2022 + VM2023) %>%
  arrange(desc(total))

maal_top10_total


# 7. Top 10 statistik (mål, assist, xG)


top10_statistik <- women_events %>%
  filter(!is.na(player.name)) %>%
  mutate(
    maal = if_else(type.name == "Shot" & shot.outcome.name == "Goal", 1, 0, missing = 0),
    assist = if_else(pass.goal_assist == TRUE, 1, 0, missing = 0),
    xg = if_else(type.name == "Shot", shot.statsbomb_xg, 0, missing = 0)
  ) %>%
  group_by(player.name) %>%
  summarise(
    hold = first(na.omit(team.name)),
    maal = sum(maal, na.rm = TRUE),
    assists = sum(assist, na.rm = TRUE),
    xg = sum(xg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(maal)) %>%
  slice(1:10)

top10_statistik