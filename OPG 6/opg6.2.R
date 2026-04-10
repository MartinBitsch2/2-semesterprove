
women_events <- readRDS("women_events.rds")

# OPGAVE 6.2
# Logisk positivisme i fodbold
# Formål:
# Vi undersøger udvalgte features fra Statsbomb-data og vurderer, hvor tæt de er på virkeligheden.
# Vi ser både på:
# 1) hvor komplette de er
# 2) om de hænger logisk sammen
# 3) hvordan fejl ville kunne opdages


# Pakker
library(dplyr)
library(tibble)
library(stringr)


# 1. Vælger nogle features vi vil undersøge
# Disse features er udvalgt, fordi de repræsenterer både meget konkrete observationer 
# og mere fortolkende eller modelbaserede variable


features <- c(
  "match_id",
  "minute",
  "second",
  "team.name",
  "player.name",
  "type.name",
  "shot.outcome.name",
  "pass.outcome.name",
  "shot.statsbomb_xg",
  "location",
  "position.name",
  "under_pressure"
)

# 2. Undersøger hvor mange manglende værdier der er
# i hver feature


feature_overblik <- tibble(feature = features) %>%
  rowwise() %>%
  mutate(
    antal_manglende = sum(is.na(women_events[[feature]])),
    andel_manglende_pct = round(mean(is.na(women_events[[feature]])) * 100, 2)
  ) %>%
  ungroup()

feature_overblik


# 3. Tæller hvor mange forskellige eventtyper der findes
# Det giver et overblik over hvad datasættet består af


women_events %>%
  count(type.name, sort = TRUE)


# 4. Tjekker logisk konsistens i datasættet
# Her undersøger vi, hvordan fejl kunne se ud


# 4A. Finder mål der ikke er registreret som skud
# Hvis denne tabel indeholder rækker, kan det være en fejl
maal_uden_skud <- women_events %>%
  filter(shot.outcome.name == "Goal" & type.name != "Shot") %>%
  select(match_id, player.name, team.name, type.name, shot.outcome.name)

maal_uden_skud

# 4B. Find xG-værdier på hændelser, der ikke er skud
# xG burde kun optræde ved skud
xg_uden_skud <- women_events %>%
  filter(!is.na(shot.statsbomb_xg) & type.name != "Shot") %>%
  select(match_id, player.name, team.name, type.name, shot.statsbomb_xg)

xg_uden_skud

# 4C. Find afslutninger uden registreret udfald
# Et skud burde normalt have et outcome
skud_uden_outcome <- women_events %>%
  filter(type.name == "Shot" & is.na(shot.outcome.name)) %>%
  select(match_id, player.name, team.name, minute, second, type.name, shot.outcome.name)

skud_uden_outcome

# 4D. Find afleveringer uden registreret spiller
# Det kan være en fejl, hvis pass-event mangler player.name
pass_uden_spiller <- women_events %>%
  filter(type.name == "Pass" & is.na(player.name)) %>%
  select(match_id, team.name, minute, second, type.name, player.name)

pass_uden_spiller

# 4E. Find hændelser uden lokation
# Her skal man være varsom, fordi ikke alle eventtyper nødvendigvis
# skal have en location. Men det er stadig interessant at undersøge
events_uden_location <- women_events %>%
  filter(is.na(location)) %>%
  count(type.name, sort = TRUE)

events_uden_location


# 5. Undersøger udvalgte features nærmere
# Her ser vi på nogle konkrete variable


# 5A. Fordeling af shot outcome
women_events %>%
  filter(type.name == "Shot") %>%
  count(shot.outcome.name, sort = TRUE)

# 5B. Fordeling af pass outcome
women_events %>%
  filter(type.name == "Pass") %>%
  count(pass.outcome.name, sort = TRUE)

# 5C. Fordeling af under_pressure
# Denne feature er mere fortolkende og ofte mindre komplet
women_events %>%
  count(under_pressure, sort = TRUE)


# 6. Laver en prioriteret liste over features
# Prioriteringen er lavet ud fra hvor tæt de er på virkeligheden
# og hvor let en fejl ville være at opdage og rette


prioriteret_liste <- tibble(
  feature = c(
    "match_id",
    "minute",
    "second",
    "team.name",
    "player.name",
    "type.name",
    "shot.outcome.name",
    "pass.outcome.name",
    "location",
    "position.name",
    "under_pressure",
    "shot.statsbomb_xg"
  ),
  prioritet = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  tæt_på_virkeligheden = c(
    "Meget høj",
    "Meget høj",
    "Meget høj",
    "Høj",
    "Høj",
    "Høj",
    "Høj",
    "Middel",
    "Middel",
    "Middel",
    "Lav",
    "Lav"
  ),
  hvorfor = c(
    "Entydigt kamp-id og let at kontrollere",
    "Direkte tidsangivelse som er let at opdage fejl i",
    "Direkte tidsangivelse som er let at opdage fejl i",
    "Kan sammenholdes med kampen og holdoplysninger",
    "Kan kontrolleres mod lineups og kampvideo",
    "Eventtypen er ofte tydelig i virkeligheden",
    "Kan kontrolleres mod video og kampforløb",
    "Kan være sværere at validere fordi udfald ikke altid er lige tydelige",
    "Placering på banen kan være mere upræcis",
    "Position kan ændre sig og være svær at afgrænse",
    "Mere fortolkende og sværere at observere direkte",
    "Modelbaseret sandsynlighed og ikke direkte observerbar"
  ),
  hvordan_fejl_ser_ud = c(
    "Forkert kamp-id vil placere en hændelse i forkert kamp",
    "Forkert minut vil gøre kampforløbet utroværdigt",
    "Forkert sekund vil forskyde hændelser i tid",
    "Forkert holdnavn vil gøre eventen ulogisk i kampen",
    "Forkert spillernavn vil give misvisende statistik",
    "Forkert eventtype vil ændre forståelsen af situationen",
    "Et mål registreret forkert vil påvirke resultat og statistik",
    "En aflevering kan stå som completed eller incomplete forkert",
    "Forkert location vil flytte hændelsen til et forkert sted på banen",
    "En spiller kan være registreret i forkert rolle",
    "Pres kan være vurderet forskelligt af forskellige observatører",
    "xG kan virke forkert uden at man direkte kan bevise det i video"
  ),
  hvordan_fejl_kan_rettes = c(
    "Sammenlignes med officiel kampinformation",
    "Sammenlignes med kampvideo og eventlog",
    "Sammenlignes med kampvideo og eventlog",
    "Kontrolleres mod officielle holdopstillinger",
    "Kontrolleres mod lineups og kampvideo",
    "Sammenlignes med video af situationen",
    "Kontrolleres mod video og kampens resultat",
    "Gennemgang af afleveringssituationen i video",
    "Kontrolleres ved at se situationen på video",
    "Vurderes ud fra spillerens rolle i sekvensen",
    "Kræver ny vurdering af situationen",
    "Kan kun revurderes ved at ændre model eller inputdata"
  )
)

prioriteret_liste


# 7. Sortere listen efter prioritet


prioriteret_liste %>%
  arrange(prioritet)


# 8. Ekstra: Gem tabeller hvis vi vil bruge dem i opgaven
# write.csv(feature_overblik, "feature_overblik.csv", row.names = FALSE)
# write.csv(prioriteret_liste, "prioriteret_liste.csv", row.names = FALSE)


# 9. Kort konklusion i kodeform
# Disse tabeller er de vigtigste til opgaven:
# feature_overblik
# maal_uden_skud
# xg_uden_skud
# skud_uden_outcome
# prioriteret_liste
