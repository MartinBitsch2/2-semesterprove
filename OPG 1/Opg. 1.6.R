# Opg. 1 Eksamens-case – Sammenligning af glm_2526 på 24/25 og 25/26
##########################################################################################################################################

# Pakker
##########################################################################################################################################
library(dplyr)
library(tidyr)
library(stringr)
library(caret)
library(pROC)
library(ggplot2)
library(RMariaDB)

##########################################################################################################################################
# Indlæs data
##########################################################################################################################################

con <- dbConnect(
  MariaDB(),
  host = "talmedos.com",
  port = "3306",
  db = "superliga2",
  user = "dalremote",
  password = "OttoRehagel123456789Long2026!"
)

shots <- readRDS("shots.rds")
players <- readRDS("players.rds")
teams <- readRDS("teams.rds")
matchdetail <- readRDS("matchdetail.rds")
common2526 <- readRDS("common2526.rds")
poss2526 <- readRDS("poss2526.rds")
secondary <- readRDS("secondary2526.rds")

#secondary <- dbGetQuery(con, "SELECT * FROM superliga2.wyscout_matches;")

##########################################################################################################################################
# Indlæs model og threshold
##########################################################################################################################################

glm_model <- readRDS("glm_2526_unweighted.rds")
thr_glm <- readRDS("thr_glm_unweighted.rds")

##########################################################################################################################################
# Hjælpefunktion
##########################################################################################################################################

get_threshold <- function(probs, y) {
  roc_obj <- roc(y, probs, quiet = TRUE)
  coords(
    roc_obj,
    x = "best",
    best.method = "youden",
    transpose = FALSE
  )$threshold
}

##########################################################################################################################################
# 1. Konstanter
##########################################################################################################################################

goal_width <- 7.32
goal_center_x <- 105
goal_center_y <- 34

left_post_y  <- goal_center_y + goal_width / 2
right_post_y <- goal_center_y - goal_width / 2

##########################################################################################################################################
# 2. Keys
##########################################################################################################################################

players_key <- players %>%
  select(PLAYER_WYID, SHORTNAME, ROLENAME, FOOT) %>%
  distinct(PLAYER_WYID, .keep_all = TRUE)

teams_key <- teams %>%
  select(TEAM_WYID, TEAMNAME) %>%
  distinct(TEAM_WYID, .keep_all = TRUE)

side_key <- matchdetail %>%
  select(MATCH_WYID, TEAM_WYID, SIDE) %>%
  distinct(MATCH_WYID, TEAM_WYID, .keep_all = TRUE)

shot_bodypart_key <- shots %>%
  select(EVENT_WYID, SHOTBODYPART) %>%
  distinct(EVENT_WYID, .keep_all = TRUE) %>%
  mutate(
    SHOTBODYPART = str_replace_all(coalesce(SHOTBODYPART, ""), "\\[|\\]|\\n|\"", ""),
    SHOTBODYPART = str_trim(SHOTBODYPART),
    SHOTBODYPART = tolower(SHOTBODYPART)
  )

##########################################################################################################################################
# 3. Secondary 25/26
##########################################################################################################################################

secondary2526 <- secondary %>%
  semi_join(
    common2526 %>% distinct(MATCH_WYID),
    by = "MATCH_WYID"
  )

goal_events_secondary2526 <- secondary2526 %>%
  mutate(
    sec1_goal = str_trim(str_to_lower(coalesce(SECONDARYTYPE1, ""))) == "goal",
    sec2_goal = str_trim(str_to_lower(coalesce(SECONDARYTYPE2, ""))) == "goal"
  ) %>%
  filter(sec1_goal | sec2_goal) %>%
  distinct(EVENT_WYID) %>%
  mutate(maal = 1L)

##########################################################################################################################################
# 4. Shot-events fra common2526
##########################################################################################################################################

shot_events2526 <- common2526 %>%
  filter(PRIMARYTYPE %in% c("shot", "penalty", "own_goal", "free_kick")) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

##########################################################################################################################################
# 5. Byg shots_superliga2526
##########################################################################################################################################

shots_superliga2526 <- shot_events2526 %>%
  left_join(shot_bodypart_key, by = "EVENT_WYID") %>%
  left_join(players_key, by = "PLAYER_WYID") %>%
  left_join(teams_key, by = "TEAM_WYID") %>%
  left_join(side_key, by = c("MATCH_WYID", "TEAM_WYID")) %>%
  left_join(goal_events_secondary2526, by = "EVENT_WYID") %>%
  mutate(
    maal = ifelse(PRIMARYTYPE == "own_goal", 1L, coalesce(maal, 0L)),
    selvmaal = ifelse(PRIMARYTYPE == "own_goal", 1L, 0L),
    hovedstød = as.integer(str_detect(coalesce(SHOTBODYPART, ""), "head")),
    competition = factor(COMPETITION_WYID, levels = 335, labels = "Superliga"),
    x_meter = LOCATIONX * 0.01 * 105,
    y_meter = LOCATIONY * 0.01 * 68,
    afstand_til_mål = sqrt((goal_center_x - x_meter)^2 + (goal_center_y - y_meter)^2),
    afstand_left = sqrt((goal_center_x - x_meter)^2 + (left_post_y - y_meter)^2),
    afstand_right = sqrt((goal_center_x - x_meter)^2 + (right_post_y - y_meter)^2),
    cos_vinkel = (afstand_left^2 + afstand_right^2 - goal_width^2) / (2 * afstand_left * afstand_right),
    cos_vinkel = pmin(pmax(cos_vinkel, -1), 1),
    vinkel_mellem_stolper = acos(cos_vinkel) * 180 / pi
  ) %>%
  select(-cos_vinkel)

##########################################################################################################################################
# 6. Join possession-variabler på
##########################################################################################################################################

shots_superliga2526 <- shots_superliga2526 %>%
  left_join(
    poss2526 %>%
      select(EVENT_WYID, POSSESSIONTYPE1, POSSESSIONTYPE2, POSSESSIONTYPE3, POSSESSIONTYPE4) %>%
      distinct(EVENT_WYID, .keep_all = TRUE) %>%
      mutate(
        across(starts_with("POSSESSIONTYPE"), ~ tolower(trimws(as.character(.x)))),
        Kontraangreb = as.integer(if_any(starts_with("POSSESSIONTYPE"), ~ .x == "counterattack")),
        corner = as.integer(if_any(starts_with("POSSESSIONTYPE"), ~ .x == "corner")),
        frispark = as.integer(if_any(starts_with("POSSESSIONTYPE"), ~ .x %in% c("free_kick", "direct_free_kick", "free_kick_cross")))
      ) %>%
      select(EVENT_WYID, Kontraangreb, corner, frispark),
    by = "EVENT_WYID"
  ) %>%
  mutate(
    across(c(Kontraangreb, corner, frispark), ~ replace_na(.x, 0L)),
    SHOTBODYPART = str_trim(coalesce(SHOTBODYPART, ""))
  )

##########################################################################################################################################
# 7. Fjern åbenlyst forkerte og tomme SHOTBODYPART-værdier
##########################################################################################################################################

shots_superliga2526 <- shots_superliga2526 %>%
  filter(
    !SHOTBODYPART %in% c("assist", "free_kic", "intercep", "touch_in"),
    SHOTBODYPART != ""
  )

##########################################################################################################################################
# 8. Kontrol
##########################################################################################################################################

cat("Antal rows i shots_superliga2526:", nrow(shots_superliga2526), "\n")
cat("Antal unikke events i shots_superliga2526:", n_distinct(shots_superliga2526$EVENT_WYID), "\n")
cat("Antal mål i secondary2526:", nrow(goal_events_secondary2526), "\n")
cat(
  "Antal mål i shots_superliga2526:",
  shots_superliga2526 %>%
    filter(maal == 1) %>%
    summarise(n = n_distinct(EVENT_WYID)) %>%
    pull(n),
  "\n"
)

missing_goals2526 <- goal_events_secondary2526 %>%
  anti_join(
    shots_superliga2526 %>% distinct(EVENT_WYID),
    by = "EVENT_WYID"
  )

cat("Antal mål i secondary2526 som stadig mangler i shots_superliga2526:", nrow(missing_goals2526), "\n")

print(table(shots_superliga2526$PRIMARYTYPE, useNA = "ifany"))
print(table(shots_superliga2526$maal, useNA = "ifany"))
print(table(shots_superliga2526$SHOTBODYPART, useNA = "ifany"))

##########################################################################################################################################
# 9. Klargøring af modeldatasæt
# Ingen selvmål, ingen straffespark, ingen frispark, ingen own_goal
# Skud fra 35 meter eller længere fra mål sorteres også fra
##########################################################################################################################################

shots_model2526 <- shots_superliga2526 %>%
  filter(
    selvmaal == 0,
    PRIMARYTYPE != "penalty",
    PRIMARYTYPE != "free_kick",
    PRIMARYTYPE != "own_goal",
    afstand_til_mål < 35
  ) %>%
  mutate(
    maal = as.integer(maal),
    hovedstød = as.integer(hovedstød),
    Kontraangreb = as.integer(Kontraangreb),
    corner = as.integer(corner),
    frispark = as.integer(frispark),
    ROLENAME = factor(ROLENAME)
  )

cat("Antal rækker i shots_model2526:", nrow(shots_model2526), "\n")
cat("Maks afstand i shots_model2526:", max(shots_model2526$afstand_til_mål, na.rm = TRUE), "\n")
print(table(shots_model2526$PRIMARYTYPE, useNA = "ifany"))
print(table(shots_model2526$SHOTBODYPART, useNA = "ifany"))

##########################################################################################################################################
# 10. xG prediction på 25/26 med uvægtet glm_2526
# Modellen bruger KUN: afstand_til_mål, vinkel_mellem_stolper, Kontraangreb, corner
# Ingen selvmål, straffespark, frispark, own_goal eller skud fra 35m+ får xG_pred
##########################################################################################################################################

shots_superliga2526$xG_pred <- NA_real_

pred_idx_2526 <- which(
  shots_superliga2526$selvmaal == 0 &
    shots_superliga2526$PRIMARYTYPE != "penalty" &
    shots_superliga2526$PRIMARYTYPE != "free_kick" &
    shots_superliga2526$PRIMARYTYPE != "own_goal" &
    shots_superliga2526$afstand_til_mål < 35
)

pred_data_2526 <- shots_superliga2526[pred_idx_2526, ] %>%
  mutate(
    Kontraangreb = as.integer(Kontraangreb),
    corner = as.integer(corner)
  )

pred_ok_2526 <- complete.cases(
  pred_data_2526 %>%
    select(
      afstand_til_mål,
      vinkel_mellem_stolper,
      Kontraangreb,
      corner
    )
)

shots_superliga2526$xG_pred[pred_idx_2526[pred_ok_2526]] <- predict(
  glm_model,
  newdata = pred_data_2526[pred_ok_2526, ],
  type = "response"
)

##########################################################################################################################################
# 11. Kontrol af prediction
##########################################################################################################################################

cat("Antal skud i shots_superliga2526:", nrow(shots_superliga2526), "\n")
cat(
  "Antal skud i xG-beregningen:",
  sum(
    shots_superliga2526$selvmaal == 0 &
      shots_superliga2526$PRIMARYTYPE != "penalty" &
      shots_superliga2526$PRIMARYTYPE != "free_kick" &
      shots_superliga2526$PRIMARYTYPE != "own_goal" &
      shots_superliga2526$afstand_til_mål < 35,
    na.rm = TRUE
  ),
  "\n"
)
cat("Antal xG predictions lavet:", sum(!is.na(shots_superliga2526$xG_pred)), "\n")
cat("Samlet xG i 25/26:", sum(shots_superliga2526$xG_pred, na.rm = TRUE), "\n")
cat("Samlet antal mål i 25/26:", sum(shots_superliga2526$maal, na.rm = TRUE), "\n")
cat(
  "Maks afstand blandt skud med xG_pred i 25/26:",
  max(shots_superliga2526$afstand_til_mål[!is.na(shots_superliga2526$xG_pred)], na.rm = TRUE),
  "\n"
)

summary(shots_superliga2526$xG_pred)

##########################################################################################################################################
# 12. Performance på hele 24/25 for uvægtet glm_2526
##########################################################################################################################################

shots_superliga2425 <- readRDS("shots_superliga_2425.rds")

shots_model2425_full <- shots_superliga2425 %>%
  filter(
    selvmaal == 0,
    PRIMARYTYPE != "penalty",
    PRIMARYTYPE != "free_kick",
    PRIMARYTYPE != "own_goal",
    afstand_til_mål < 35
  ) %>%
  mutate(
    maal = as.integer(maal),
    Kontraangreb = as.integer(Kontraangreb),
    corner = as.integer(corner)
  )

eval_2425_full <- shots_model2425_full %>%
  select(
    maal,
    afstand_til_mål,
    vinkel_mellem_stolper,
    Kontraangreb,
    corner
  ) %>%
  drop_na()

p_2425_full <- predict(glm_model, newdata = eval_2425_full, type = "response")

roc_2425 <- roc(eval_2425_full$maal, p_2425_full, quiet = TRUE)
auc_2425 <- auc(roc_2425)

brier_2425 <- mean((eval_2425_full$maal - p_2425_full)^2)

pred_2425 <- factor(ifelse(p_2425_full >= thr_glm, 1, 0), levels = c(0, 1))
obs_2425  <- factor(eval_2425_full$maal, levels = c(0, 1))

cm_2425 <- confusionMatrix(pred_2425, obs_2425, positive = "1")

##########################################################################################################################################
# 13. Performance på hele 25/26 for uvægtet glm_2526
##########################################################################################################################################

eval_2526 <- shots_model2526 %>%
  select(
    maal,
    afstand_til_mål,
    vinkel_mellem_stolper,
    Kontraangreb,
    corner
  ) %>%
  drop_na()

p_2526 <- predict(glm_model, newdata = eval_2526, type = "response")

roc_2526 <- roc(eval_2526$maal, p_2526, quiet = TRUE)
auc_2526 <- auc(roc_2526)

brier_2526 <- mean((eval_2526$maal - p_2526)^2)

pred_2526 <- factor(ifelse(p_2526 >= thr_glm, 1, 0), levels = c(0, 1))
obs_2526  <- factor(eval_2526$maal, levels = c(0, 1))

cm_2526 <- confusionMatrix(pred_2526, obs_2526, positive = "1")

##########################################################################################################################################
# 14. Sammenligning hele 24/25 vs hele 25/26
##########################################################################################################################################

glm_compare_2426 <- data.frame(
  Dataset = c("Hele 2024/25", "Hele 2025/26"),
  AUC = c(as.numeric(auc_2425), as.numeric(auc_2526)),
  Brier = c(brier_2425, brier_2526),
  Sensitivity = c(cm_2425$byClass["Sensitivity"], cm_2526$byClass["Sensitivity"]),
  Specificity = c(cm_2425$byClass["Specificity"], cm_2526$byClass["Specificity"]),
  Balanced_Accuracy = c(cm_2425$byClass["Balanced Accuracy"], cm_2526$byClass["Balanced Accuracy"]),
  Threshold = c(thr_glm, thr_glm),
  Antal_skud = c(nrow(eval_2425_full), nrow(eval_2526)),
  Antal_mål = c(sum(eval_2425_full$maal, na.rm = TRUE), sum(eval_2526$maal, na.rm = TRUE)),
  Samlet_xG = c(sum(p_2425_full, na.rm = TRUE), sum(p_2526, na.rm = TRUE))
)

glm_compare_2426

##########################################################################################################################################
# 15. Gem som rds
##########################################################################################################################################

saveRDS(shots_superliga2526, "shots_superliga_2526.rds")
saveRDS(glm_compare_2426, "glm_compare_2426.rds")
saveRDS(secondary2526, "secondary2526.rds")
