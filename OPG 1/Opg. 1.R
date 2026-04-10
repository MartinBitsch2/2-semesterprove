##########################################################################################################################################
# Opg. 1 Eksamens-case – 24/25
##########################################################################################################################################

##########################################################################################################################################
# Pakker
##########################################################################################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
library(randomForest)
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
matches301 <- readRDS("matches301.rds")
players <- readRDS("players.rds")
common2425 <- readRDS("common2425.rds")
poss2425 <- readRDS("poss2425.rds")
secondary <- readRDS("assist.rds")
teams <- readRDS("teams.rds")
matchdetail <- readRDS("matchdetail.rds")

##########################################################################################################################################
# 1. Konstanter
##########################################################################################################################################

goal_width <- 7.32
goal_center_x <- 105
goal_center_y <- 34

left_post_y  <- goal_center_y + goal_width / 2
right_post_y <- goal_center_y - goal_width / 2

##########################################################################################################################################
# 2. Player key
##########################################################################################################################################

players_key <- players %>%
  select(PLAYER_WYID, SHORTNAME, ROLENAME, FOOT) %>%
  distinct(PLAYER_WYID, .keep_all = TRUE) %>%
  mutate(
    FOOT = str_trim(str_to_lower(coalesce(FOOT, "")))
  )

##########################################################################################################################################
# 3. Secondary 24/25
##########################################################################################################################################

secondary2425 <- secondary %>%
  semi_join(
    common2425 %>% distinct(MATCH_WYID),
    by = "MATCH_WYID"
  )

##########################################################################################################################################
# 4. Goal-events fra secondary2425
##########################################################################################################################################

goal_events_secondary <- secondary2425 %>%
  mutate(
    sec1_goal = str_trim(str_to_lower(coalesce(SECONDARYTYPE1, ""))) == "goal",
    sec2_goal = str_trim(str_to_lower(coalesce(SECONDARYTYPE2, ""))) == "goal"
  ) %>%
  filter(sec1_goal | sec2_goal) %>%
  distinct(EVENT_WYID) %>%
  mutate(maal = 1L)

##########################################################################################################################################
# 5. Shot-events fra common2425
##########################################################################################################################################

shot_events <- common2425 %>%
  filter(PRIMARYTYPE %in% c("shot", "penalty", "own_goal", "free_kick")) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

##########################################################################################################################################
# 6. SHOTBODYPART fra shots
##########################################################################################################################################

shot_bodypart_key <- shots %>%
  select(EVENT_WYID, SHOTBODYPART) %>%
  distinct(EVENT_WYID, .keep_all = TRUE) %>%
  mutate(
    SHOTBODYPART = str_replace_all(coalesce(SHOTBODYPART, ""), "\\[|\\]|\\n|\"", ""),
    SHOTBODYPART = str_trim(str_to_lower(SHOTBODYPART))
  )

##########################################################################################################################################
# SIDE fra matchdetail
##########################################################################################################################################

side_key <- matchdetail %>%
  select(MATCH_WYID, TEAM_WYID, SIDE) %>%
  distinct(MATCH_WYID, TEAM_WYID, .keep_all = TRUE)

##########################################################################################################################################
# Team key
##########################################################################################################################################

teams_key <- teams %>%
  select(TEAM_WYID, TEAMNAME) %>%
  distinct(TEAM_WYID, .keep_all = TRUE)

##########################################################################################################################################
# Shot xG key fra shots
##########################################################################################################################################

shot_xg_key <- shots %>%
  select(EVENT_WYID, SHOTXG) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

##########################################################################################################################################
# 7. Byg shots_superliga
##########################################################################################################################################

shots_superliga <- shot_events %>%
  left_join(shot_bodypart_key, by = "EVENT_WYID") %>%
  left_join(shot_xg_key, by = "EVENT_WYID") %>%
  left_join(players_key, by = "PLAYER_WYID") %>%
  left_join(teams_key, by = "TEAM_WYID") %>%
  left_join(side_key, by = c("MATCH_WYID", "TEAM_WYID")) %>%
  left_join(goal_events_secondary, by = "EVENT_WYID") %>%
  mutate(
    SHOTXG = coalesce(SHOTXG, 0),
    maal = ifelse(PRIMARYTYPE == "own_goal", 1L, coalesce(maal, 0L)),
    selvmaal = ifelse(PRIMARYTYPE == "own_goal", 1L, 0L),
    
    SHOTBODYPART = str_trim(str_to_lower(coalesce(SHOTBODYPART, ""))),
    FOOT = str_trim(str_to_lower(coalesce(FOOT, ""))),
    
    hovedstød = as.integer(str_detect(SHOTBODYPART, "head")),
    
    strong_foot = case_when(
      FOOT == "right" & SHOTBODYPART == "right_foot" ~ 1L,
      FOOT == "left"  & SHOTBODYPART == "left_foot"  ~ 1L,
      FOOT == "right" & SHOTBODYPART == "left_foot"  ~ 0L,
      FOOT == "left"  & SHOTBODYPART == "right_foot" ~ 0L,
      SHOTBODYPART %in% c("head_or_other", "head", "other") ~ NA_integer_,
      FOOT == "" | SHOTBODYPART == "" ~ NA_integer_,
      TRUE ~ NA_integer_
    ),
    
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
# 8. Join possession-variabler på
##########################################################################################################################################

shots_superliga <- shots_superliga %>%
  left_join(
    poss2425 %>%
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
# 9. Fjern åbenlyst forkerte og tomme SHOTBODYPART-værdier
##########################################################################################################################################

shots_superliga <- shots_superliga %>%
  filter(
    maal == 1 | (
      !SHOTBODYPART %in% c("assist", "intercep", "touch_in") &
        !is.na(SHOTBODYPART) &
        SHOTBODYPART != ""
    )
  )

##########################################################################################################################################
# 10. Kontrol
##########################################################################################################################################

cat("Antal rows i shots_superliga:", nrow(shots_superliga), "\n")
cat("Antal unikke events i shots_superliga:", n_distinct(shots_superliga$EVENT_WYID), "\n")
cat("Antal mål i secondary2425:", nrow(goal_events_secondary), "\n")
cat(
  "Antal mål i shots_superliga:",
  shots_superliga %>%
    filter(maal == 1) %>%
    summarise(n = n_distinct(EVENT_WYID)) %>%
    pull(n),
  "\n"
)

missing_goals <- goal_events_secondary %>%
  anti_join(
    shots_superliga %>% distinct(EVENT_WYID),
    by = "EVENT_WYID"
  )

cat("Antal mål i secondary2425 som stadig mangler i shots_superliga:", nrow(missing_goals), "\n")
print(table(shots_superliga$PRIMARYTYPE, useNA = "ifany"))
print(table(shots_superliga$maal, useNA = "ifany"))
print(table(shots_superliga$SHOTBODYPART, useNA = "ifany"))
print(table(shots_superliga$strong_foot, useNA = "ifany"))

##########################################################################################################################################
# 11. Klargøring af modeldatasæt
# Ingen selvmål, ingen straffespark, ingen frispark, ingen own_goal
# Skud fra 35 meter eller længere fra mål sorteres også fra
##########################################################################################################################################

shots_model <- shots_superliga %>%
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
    strong_foot = as.integer(strong_foot),
    Kontraangreb = as.integer(Kontraangreb),
    corner = as.integer(corner),
    frispark = as.integer(frispark),
    ROLENAME = factor(ROLENAME)
  )

cat("Antal rækker i shots_model:", nrow(shots_model), "\n")
cat("Maks afstand i shots_model:", max(shots_model$afstand_til_mål, na.rm = TRUE), "\n")
print(table(shots_model$PRIMARYTYPE, useNA = "ifany"))
print(table(shots_model$SHOTBODYPART, useNA = "ifany"))
print(table(shots_model$strong_foot, useNA = "ifany"))

##########################################################################################################################################
# 12. Train/test split på kampniveau
##########################################################################################################################################

set.seed(123)

match_ids <- unique(shots_model$MATCH_WYID)
best_split <- NULL
best_diff <- Inf

for (i in 1:200) {
  train_matches <- sample(match_ids, size = floor(0.8 * length(match_ids)))
  
  train_tmp <- shots_model %>% filter(MATCH_WYID %in% train_matches)
  test_tmp  <- shots_model %>% filter(!MATCH_WYID %in% train_matches)
  
  diff_rate <- abs(mean(train_tmp$maal, na.rm = TRUE) - mean(test_tmp$maal, na.rm = TRUE))
  
  if (diff_rate < best_diff) {
    best_diff <- diff_rate
    best_split <- train_matches
  }
}

train_data <- shots_model %>% filter(MATCH_WYID %in% best_split)
test_data  <- shots_model %>% filter(!MATCH_WYID %in% best_split)

test_data$ROLENAME <- factor(test_data$ROLENAME, levels = levels(train_data$ROLENAME))

cat("Målraten i train:", mean(train_data$maal, na.rm = TRUE), "\n")
cat("Målraten i test:", mean(test_data$maal, na.rm = TRUE), "\n")

saveRDS(best_split, "train_matches_split.rds")

##########################################################################################################################################
# 13. Dokumentation før vægtning
##########################################################################################################################################

train_data_before <- train_data

cat("\n--- FØR VÆGTNING ---\n")
cat("Antal rækker i train_data_before:", nrow(train_data_before), "\n")
cat("Fordeling af maal før vægtning:\n")
print(table(train_data_before$maal))
cat("Andele af maal før vægtning:\n")
print(prop.table(table(train_data_before$maal)))

##########################################################################################################################################
# 14. Håndtering af ubalance: klassevægte
##########################################################################################################################################

pos_rate <- mean(train_data$maal, na.rm = TRUE)

w_goal    <- 1 / pos_rate
w_no_goal <- 1 / (1 - pos_rate)

train_data <- train_data %>%
  mutate(
    case_weight = ifelse(maal == 1, w_goal, w_no_goal)
  )

cat("\n--- EFTER VÆGTNING ---\n")
cat("Antal rækker i train_data:", nrow(train_data), "\n")
cat("Fordeling af maal efter vægtning:\n")
print(table(train_data$maal))

cat("\n--- KONTROL ---\n")
cat("Samme antal rækker før og efter vægtning?: ", nrow(train_data_before) == nrow(train_data), "\n")
cat("Samme antal mål før og efter vægtning?: ", sum(train_data_before$maal == 1, na.rm = TRUE) == sum(train_data$maal == 1, na.rm = TRUE), "\n")
cat("Samme antal ikke-mål før og efter vægtning?: ", sum(train_data_before$maal == 0, na.rm = TRUE) == sum(train_data$maal == 0, na.rm = TRUE), "\n")

cat("\nPositiv klasse-rate (mål) i træningsdata:", pos_rate, "\n")
cat("Vægt for mål:", w_goal, "\n")
cat("Vægt for ikke-mål:", w_no_goal, "\n")

##########################################################################################################################################
# 15. Hjælpefunktion til threshold
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
# 16. Fuld modeldata til GLM
##########################################################################################################################################

model_data_full <- train_data %>%
  select(
    maal,
    afstand_til_mål,
    vinkel_mellem_stolper,
    hovedstød,
    MINUTE,
    ROLENAME,
    Kontraangreb,
    corner,
    frispark,
    case_weight
  ) %>%
  drop_na()

test_data_glm_full <- test_data %>%
  select(
    maal,
    afstand_til_mål,
    vinkel_mellem_stolper,
    hovedstød,
    strong_foot,
    MINUTE,
    ROLENAME,
    Kontraangreb,
    corner,
    frispark
  ) %>%
  drop_na()

test_data_glm_full$ROLENAME <- factor(test_data_glm_full$ROLENAME, levels = levels(train_data$ROLENAME))

##########################################################################################################################################
# 17. Fuld logistisk regression
##########################################################################################################################################

glm_full <- glm(
  maal ~ afstand_til_mål + vinkel_mellem_stolper + hovedstød + MINUTE +
    ROLENAME + Kontraangreb + corner + frispark,
  data = model_data_full,
  family = binomial(),
  weights = case_weight
)

summary(glm_full)
drop1(glm_full, test = "Chisq")
exp(coef(glm_full))

##########################################################################################################################################
# 18. Signifikant GLM
# strong_foot testes her, men indgår ikke senere i den endelige 25/26-model
##########################################################################################################################################

model_data_significant <- train_data %>%
  select(
    maal,
    afstand_til_mål,
    vinkel_mellem_stolper,
    hovedstød,
    strong_foot,
    Kontraangreb,
    corner,
    case_weight
  ) %>%
  drop_na()

test_data_glm_significant <- test_data %>%
  select(
    maal,
    afstand_til_mål,
    vinkel_mellem_stolper,
    hovedstød,
    strong_foot,
    Kontraangreb,
    corner
  ) %>%
  drop_na()

glm_significant <- glm(
  maal ~ afstand_til_mål + vinkel_mellem_stolper + hovedstød +
    strong_foot + Kontraangreb + corner,
  data = model_data_significant,
  family = binomial(),
  weights = case_weight
)

summary(glm_significant)
drop1(glm_significant, test = "Chisq")
exp(coef(glm_significant))

##########################################################################################################################################
# 19. 25/26-kompatibel GLM
# strong_foot indgår IKKE i den endelige model
##########################################################################################################################################

model_data_2526 <- train_data %>%
  select(
    maal,
    afstand_til_mål,
    vinkel_mellem_stolper,
    Kontraangreb,
    corner,
    case_weight
  ) %>%
  drop_na()

test_data_glm_2526 <- test_data %>%
  select(
    maal,
    afstand_til_mål,
    vinkel_mellem_stolper,
    Kontraangreb,
    corner
  ) %>%
  drop_na()

glm_2526 <- glm(
  maal ~ afstand_til_mål + vinkel_mellem_stolper +
    Kontraangreb + corner,
  data = model_data_2526,
  family = binomial(),
  weights = case_weight
)

summary(glm_2526)
drop1(glm_2526, test = "Chisq")
exp(coef(glm_2526))

##########################################################################################################################################
# 20. Sammenligning af de tre GLM-modeller på testdatasæt
##########################################################################################################################################

p_glm_full <- predict(glm_full, newdata = test_data_glm_full, type = "response")
p_glm_significant <- predict(glm_significant, newdata = test_data_glm_significant, type = "response")
p_glm_2526 <- predict(glm_2526, newdata = test_data_glm_2526, type = "response")

roc_glm_full <- roc(test_data_glm_full$maal, p_glm_full, quiet = TRUE)
roc_glm_significant <- roc(test_data_glm_significant$maal, p_glm_significant, quiet = TRUE)
roc_glm_2526 <- roc(test_data_glm_2526$maal, p_glm_2526, quiet = TRUE)

auc_glm_full <- auc(roc_glm_full)
auc_glm_significant <- auc(roc_glm_significant)
auc_glm_2526 <- auc(roc_glm_2526)

brier_glm_full <- mean((test_data_glm_full$maal - p_glm_full)^2)
brier_glm_significant <- mean((test_data_glm_significant$maal - p_glm_significant)^2)
brier_glm_2526 <- mean((test_data_glm_2526$maal - p_glm_2526)^2)

glm_compare <- data.frame(
  Model = c("Fuld GLM", "Signifikant GLM", "25/26-kompatibel GLM"),
  AUC = c(as.numeric(auc_glm_full), as.numeric(auc_glm_significant), as.numeric(auc_glm_2526)),
  Brier = c(brier_glm_full, brier_glm_significant, brier_glm_2526)
)

glm_compare

##########################################################################################################################################
# 21. Sammenligning: før vs. efter vægtning for 25/26-modellen
##########################################################################################################################################

train_data_before_2526 <- train_data_before %>%
  select(
    maal,
    afstand_til_mål,
    vinkel_mellem_stolper,
    Kontraangreb,
    corner
  ) %>%
  drop_na()

test_data_2526_compare <- test_data %>%
  select(
    maal,
    afstand_til_mål,
    vinkel_mellem_stolper,
    Kontraangreb,
    corner
  ) %>%
  drop_na()

glm_2526_unweighted <- glm(
  maal ~ afstand_til_mål + vinkel_mellem_stolper +
    Kontraangreb + corner,
  data = train_data_before_2526,
  family = binomial()
)

glm_2526_weighted <- glm(
  maal ~ afstand_til_mål + vinkel_mellem_stolper +
    Kontraangreb + corner,
  data = model_data_2526,
  family = binomial(),
  weights = case_weight
)

p_glm_unweighted <- predict(glm_2526_unweighted, newdata = test_data_2526_compare, type = "response")
p_glm_weighted   <- predict(glm_2526_weighted, newdata = test_data_2526_compare, type = "response")

roc_glm_unweighted <- roc(test_data_2526_compare$maal, p_glm_unweighted, quiet = TRUE)
roc_glm_weighted   <- roc(test_data_2526_compare$maal, p_glm_weighted, quiet = TRUE)

auc_glm_unweighted <- auc(roc_glm_unweighted)
auc_glm_weighted   <- auc(roc_glm_weighted)

brier_glm_unweighted <- mean((test_data_2526_compare$maal - p_glm_unweighted)^2)
brier_glm_weighted   <- mean((test_data_2526_compare$maal - p_glm_weighted)^2)

thr_glm_unweighted <- get_threshold(
  predict(glm_2526_unweighted, newdata = train_data_before_2526, type = "response"),
  train_data_before_2526$maal
)

thr_glm_weighted <- get_threshold(
  predict(glm_2526_weighted, newdata = model_data_2526, type = "response"),
  model_data_2526$maal
)

pred_glm_unweighted <- factor(ifelse(p_glm_unweighted >= thr_glm_unweighted, 1, 0), levels = c(0, 1))
pred_glm_weighted   <- factor(ifelse(p_glm_weighted >= thr_glm_weighted, 1, 0), levels = c(0, 1))

obs_glm_2526 <- factor(test_data_2526_compare$maal, levels = c(0, 1))

cm_glm_unweighted <- confusionMatrix(pred_glm_unweighted, obs_glm_2526, positive = "1")
cm_glm_weighted   <- confusionMatrix(pred_glm_weighted, obs_glm_2526, positive = "1")

before_after_weights <- data.frame(
  Model = c("25/26-GLM før vægtning", "25/26-GLM efter vægtning"),
  AUC = c(as.numeric(auc_glm_unweighted), as.numeric(auc_glm_weighted)),
  Brier = c(brier_glm_unweighted, brier_glm_weighted),
  Sensitivity = c(cm_glm_unweighted$byClass["Sensitivity"], cm_glm_weighted$byClass["Sensitivity"]),
  Specificity = c(cm_glm_unweighted$byClass["Specificity"], cm_glm_weighted$byClass["Specificity"]),
  Balanced_Accuracy = c(cm_glm_unweighted$byClass["Balanced Accuracy"], cm_glm_weighted$byClass["Balanced Accuracy"]),
  Threshold = c(thr_glm_unweighted, thr_glm_weighted)
)

before_after_weights

thr_weighted_common <- get_threshold(
  predict(glm_2526_weighted, newdata = model_data_2526, type = "response"),
  model_data_2526$maal
)

pred_glm_unweighted_youden <- factor(ifelse(p_glm_unweighted >= thr_weighted_common, 1, 0), levels = c(0, 1))
pred_glm_weighted_youden   <- factor(ifelse(p_glm_weighted >= thr_weighted_common, 1, 0), levels = c(0, 1))

cm_glm_unweighted_youden <- confusionMatrix(pred_glm_unweighted_youden, obs_glm_2526, positive = "1")
cm_glm_weighted_youden   <- confusionMatrix(pred_glm_weighted_youden, obs_glm_2526, positive = "1")

before_after_weights_youden <- data.frame(
  Model = c("25/26-GLM før vægtning", "25/26-GLM efter vægtning"),
  AUC = c(as.numeric(auc_glm_unweighted), as.numeric(auc_glm_weighted)),
  Brier = c(brier_glm_unweighted, brier_glm_weighted),
  Sensitivity = c(cm_glm_unweighted_youden$byClass["Sensitivity"], cm_glm_weighted_youden$byClass["Sensitivity"]),
  Specificity = c(cm_glm_unweighted_youden$byClass["Specificity"], cm_glm_weighted_youden$byClass["Specificity"]),
  Balanced_Accuracy = c(cm_glm_unweighted_youden$byClass["Balanced Accuracy"], cm_glm_weighted_youden$byClass["Balanced Accuracy"]),
  Threshold = c(thr_weighted_common, thr_weighted_common)
)

before_after_weights_youden

table(pred_glm_unweighted, pred_glm_weighted)
mean(pred_glm_unweighted == pred_glm_weighted)
cor(p_glm_unweighted, p_glm_weighted)
summary(p_glm_unweighted - p_glm_weighted)
coef(glm_2526_unweighted)
coef(glm_2526_weighted)

##########################################################################################################################################
# 22. Endelig valgt GLM til xG-beregning
# Uvægtet model vælges til xG, fordi xG skal fortolkes som kalibreret sandsynlighed
##########################################################################################################################################

glm_model <- glm_2526_unweighted

p_glm <- predict(glm_model, newdata = test_data_2526_compare, type = "response")
roc_glm <- roc(test_data_2526_compare$maal, p_glm, quiet = TRUE)
auc_glm <- auc(roc_glm)
brier_glm <- mean((test_data_2526_compare$maal - p_glm)^2)

##########################################################################################################################################
# 23. Beregn xG på shots_superliga med den uvægtede model
# Ingen selvmål, straffespark, frispark, own_goal eller skud fra 35m+ får xG_pred
##########################################################################################################################################

shots_superliga$xG_pred <- NA_real_

pred_idx <- which(
  shots_superliga$selvmaal == 0 &
    shots_superliga$PRIMARYTYPE != "penalty" &
    shots_superliga$PRIMARYTYPE != "free_kick" &
    shots_superliga$PRIMARYTYPE != "own_goal" &
    shots_superliga$afstand_til_mål < 35
)

tmp_pred_data <- shots_superliga[pred_idx, ] %>%
  mutate(
    Kontraangreb = as.integer(Kontraangreb),
    corner = as.integer(corner)
  )

pred_ok <- complete.cases(
  tmp_pred_data %>%
    select(
      afstand_til_mål,
      vinkel_mellem_stolper,
      Kontraangreb,
      corner
    )
)

shots_superliga$xG_pred[pred_idx[pred_ok]] <- predict(
  glm_model,
  newdata = tmp_pred_data[pred_ok, ],
  type = "response"
)

cat("Antal skud med beregnet xG_pred:", sum(!is.na(shots_superliga$xG_pred)), "\n")
cat("Maks afstand blandt skud med xG_pred:", max(shots_superliga$afstand_til_mål[!is.na(shots_superliga$xG_pred)], na.rm = TRUE), "\n")

##########################################################################################################################################
# 24. Beslutningstræ
##########################################################################################################################################

tree_train <- train_data %>%
  select(maal, afstand_til_mål, vinkel_mellem_stolper, Kontraangreb, corner) %>%
  drop_na() %>%
  mutate(maal_factor = factor(maal, levels = c(0, 1), labels = c("ikke-mål", "mål")))

tree_model <- rpart(
  maal_factor ~ afstand_til_mål + vinkel_mellem_stolper + Kontraangreb + corner,
  data = tree_train,
  method = "class",
  control = rpart.control(minsplit = 20, cp = 0.0001, xval = 10)
)

printcp(tree_model)
print(sort(tree_model$variable.importance, decreasing = TRUE))

best_cp <- tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"]
tree_pruned <- prune(tree_model, cp = best_cp)

target_splits <- 4
cp_table <- tree_model$cptable
idx <- which.min(abs(cp_table[, "nsplit"] - target_splits))
cp_vis <- cp_table[idx, "CP"]
tree_vis <- prune(tree_model, cp = cp_vis)

rpart.plot(
  tree_vis,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  tweak = 1.2,
  branch.lty = 3,
  main = paste0(
    "Beslutningstræ (", target_splits, " splits)\n",
    "Afstand, vinkel og dødbolds-/omstillingstyper"
  )
)

test_data_tree <- test_data %>%
  select(maal, afstand_til_mål, vinkel_mellem_stolper, Kontraangreb, corner) %>%
  drop_na()

test_tree_eval <- test_data_tree %>%
  mutate(maal_factor = factor(maal, levels = c(0, 1), labels = c("ikke-mål", "mål")))

p_tree <- predict(tree_pruned, newdata = test_tree_eval, type = "prob")[, "mål"]

roc_tree <- roc(test_tree_eval$maal, p_tree, quiet = TRUE)
auc_tree <- auc(roc_tree)
brier_tree <- mean((test_tree_eval$maal - p_tree)^2)

##########################################################################################################################################
# 25. Random Forest
##########################################################################################################################################

set.seed(123)

train_data_rf <- train_data %>%
  select(maal, afstand_til_mål, vinkel_mellem_stolper, Kontraangreb, corner) %>%
  drop_na() %>%
  mutate(maal_factor = factor(maal, levels = c(0, 1), labels = c("ikke-mål", "mål")))

rf_model <- randomForest(
  maal_factor ~ afstand_til_mål + vinkel_mellem_stolper + Kontraangreb + corner,
  data = train_data_rf,
  ntree = 500,
  importance = TRUE,
  classwt = c("ikke-mål" = 1, "mål" = 1 / pos_rate)
)

importance(rf_model)

test_data_rf <- test_data %>%
  select(maal, afstand_til_mål, vinkel_mellem_stolper, Kontraangreb, corner) %>%
  drop_na()

test_data_rf_eval <- test_data_rf %>%
  mutate(maal_factor = factor(maal, levels = c(0, 1), labels = c("ikke-mål", "mål")))

p_rf <- predict(rf_model, newdata = test_data_rf_eval, type = "prob")[, "mål"]

roc_rf <- roc(test_data_rf_eval$maal, p_rf, quiet = TRUE)
auc_rf <- auc(roc_rf)
brier_rf <- mean((test_data_rf_eval$maal - p_rf)^2)

##########################################################################################################################################
# 26. Thresholds fra træningsdata
##########################################################################################################################################

thr_glm <- get_threshold(
  predict(glm_model, newdata = train_data_before_2526, type = "response"),
  train_data_before_2526$maal
)

thr_tree <- get_threshold(
  predict(tree_pruned, newdata = tree_train, type = "prob")[, "mål"],
  tree_train$maal
)

thr_rf <- get_threshold(
  predict(rf_model, newdata = train_data_rf, type = "prob")[, "mål"],
  train_data_rf$maal
)

##########################################################################################################################################
# 27. ROC-kurve med markeret threshold for logistisk regression
##########################################################################################################################################

thr_point <- coords(
  roc_glm,
  x = thr_glm,
  input = "threshold",
  ret = c("specificity", "sensitivity", "threshold"),
  transpose = FALSE
)

plot(
  roc_glm,
  col = "darkblue",
  lwd = 3,
  legacy.axes = TRUE,
  main = "ROC-kurve for logistisk regression\nmed optimal threshold"
)

points(
  1 - thr_point["specificity"],
  thr_point["sensitivity"],
  pch = 19,
  col = "red",
  cex = 1.5
)

text(
  1 - thr_point["specificity"],
  thr_point["sensitivity"],
  labels = paste0("Threshold = ", round(thr_point["threshold"], 3)),
  pos = 4,
  col = "red"
)

abline(a = 0, b = 1, lty = 2, col = "grey50")

##########################################################################################################################################
# 28. Klassifikation på testdata
##########################################################################################################################################

pred_glm  <- factor(ifelse(p_glm  >= thr_glm,  1, 0), levels = c(0, 1))
pred_tree <- factor(ifelse(p_tree >= thr_tree, 1, 0), levels = c(0, 1))
pred_rf   <- factor(ifelse(p_rf   >= thr_rf,   1, 0), levels = c(0, 1))

obs_glm  <- factor(test_data_2526_compare$maal, levels = c(0, 1))
obs_tree <- factor(test_tree_eval$maal, levels = c(0, 1))
obs_rf   <- factor(test_data_rf_eval$maal, levels = c(0, 1))

cm_glm  <- confusionMatrix(pred_glm,  obs_glm, positive = "1")
cm_tree <- confusionMatrix(pred_tree, obs_tree, positive = "1")
cm_rf   <- confusionMatrix(pred_rf,   obs_rf, positive = "1")

cm_glm
cm_tree
cm_rf

##########################################################################################################################################
# 29. Samlet tabel
##########################################################################################################################################

results <- data.frame(
  Model = c("Logistisk regression", "Random Forest", "Beslutningstræ"),
  AUC = c(as.numeric(auc_glm), as.numeric(auc_rf), as.numeric(auc_tree)),
  Brier = c(brier_glm, brier_rf, brier_tree),
  Sensitivity = c(cm_glm$byClass["Sensitivity"], cm_rf$byClass["Sensitivity"], cm_tree$byClass["Sensitivity"]),
  Specificity = c(cm_glm$byClass["Specificity"], cm_rf$byClass["Specificity"], cm_tree$byClass["Specificity"]),
  Balanced_Accuracy = c(cm_glm$byClass["Balanced Accuracy"], cm_rf$byClass["Balanced Accuracy"], cm_tree$byClass["Balanced Accuracy"])
)

results

##########################################################################################################################################
# 30. Visualiseringer
##########################################################################################################################################

plot(
  roc_glm,
  col = "darkblue",
  lwd = 3,
  legacy.axes = TRUE,
  main = "ROC-kurver for klassifikationsmodeller"
)
plot(roc_rf, col = "darkgreen", lwd = 2, add = TRUE)
plot(roc_tree, col = "darkred", lwd = 2, add = TRUE)

legend(
  "bottomright",
  legend = c(
    paste0("Logistisk regression (AUC = ", round(as.numeric(auc_glm), 3), ")"),
    paste0("Random Forest (AUC = ", round(as.numeric(auc_rf), 3), ")"),
    paste0("Beslutningstræ (AUC = ", round(as.numeric(auc_tree), 3), ")")
  ),
  col = c("darkblue", "darkgreen", "darkred"),
  lwd = c(3, 2, 2),
  bty = "n"
)

plot_cm_gg <- function(cm_obj, title = "") {
  cm_df <- as.data.frame(cm_obj$table)
  colnames(cm_df) <- c("Forudsagt", "Faktisk", "Antal")
  
  ggplot(cm_df, aes(x = Faktisk, y = Forudsagt, fill = Antal)) +
    geom_tile() +
    geom_text(aes(label = Antal), size = 5) +
    labs(
      title = title,
      x = "Faktisk klasse",
      y = "Forudsagt klasse"
    ) +
    theme_minimal()
}

plot_cm_gg(cm_glm, "Confusion matrix – Logistisk regression")
plot_cm_gg(cm_rf, "Confusion matrix – Random Forest")
plot_cm_gg(cm_tree, "Confusion matrix – Beslutningstræ")

results_long <- results %>%
  select(Model, AUC, Brier, Balanced_Accuracy) %>%
  pivot_longer(
    cols = c(AUC, Brier, Balanced_Accuracy),
    names_to = "Mål",
    values_to = "Værdi"
  )

ggplot(results_long, aes(x = Model, y = Værdi, fill = Model)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Mål, scales = "free_y") +
  labs(
    title = "Sammenligning af modeller på testdata",
    x = "Model",
    y = "Værdi"
  ) +
  theme_minimal()

cm_compare <- data.frame(
  Model = c("Logistisk regression", "Random Forest", "Beslutningstræ"),
  Sensitivity = c(cm_glm$byClass["Sensitivity"], cm_rf$byClass["Sensitivity"], cm_tree$byClass["Sensitivity"]),
  Specificity = c(cm_glm$byClass["Specificity"], cm_rf$byClass["Specificity"], cm_tree$byClass["Specificity"]),
  Balanced_Accuracy = c(cm_glm$byClass["Balanced Accuracy"], cm_rf$byClass["Balanced Accuracy"], cm_tree$byClass["Balanced Accuracy"])
)

cm_compare_long <- cm_compare %>%
  pivot_longer(
    cols = c(Sensitivity, Specificity, Balanced_Accuracy),
    names_to = "Mål",
    values_to = "Værdi"
  )

ggplot(cm_compare_long, aes(x = Model, y = Værdi, fill = Model)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Mål) +
  labs(
    title = "Klassifikationsmål for modellerne",
    x = "Model",
    y = "Score"
  ) +
  ylim(0, 1) +
  theme_minimal()

##########################################################################################################################################
# 31. SD på xG
##########################################################################################################################################

sd_compare <- data.frame(
  Model = c("Egen xG", "WyScout xG"),
  Standardafvigelse = c(
    sd(shots_superliga$xG_pred, na.rm = TRUE),
    sd(shots_superliga$SHOTXG, na.rm = TRUE)
  )
)

sd_compare

##########################################################################################################################################
# 32. Gem som rds
##########################################################################################################################################

saveRDS(shots_superliga, "shots_superliga_2425.rds")
saveRDS(results, "results.rds")
saveRDS(glm_full, "glm_full_2425.rds")
saveRDS(glm_significant, "glm_significant_2425.rds")
saveRDS(glm_2526_unweighted, "glm_2526_unweighted.rds")
saveRDS(thr_glm_unweighted, "thr_glm_unweighted.rds")
saveRDS(test_data_2526_compare, "test_data_2526_compare_2425.rds")
saveRDS(p_glm_unweighted, "p_glm_unweighted_2425.rds")
saveRDS(cm_glm_unweighted, "cm_glm_unweighted.rds")

##########################################################################################################################################
# 33. Ekstra filer til Shiny-app: model performance
##########################################################################################################################################

make_calibration_data <- function(obs, pred, model_name, n_bins = 10) {
  data.frame(
    observed = as.numeric(obs),
    predicted_prob = as.numeric(pred)
  ) %>%
    filter(!is.na(observed), !is.na(predicted_prob)) %>%
    mutate(bin = ntile(predicted_prob, n_bins)) %>%
    group_by(bin) %>%
    summarise(
      predicted = mean(predicted_prob, na.rm = TRUE),
      observed = mean(observed, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(model = model_name)
}

make_roc_data <- function(roc_obj, model_name) {
  data.frame(
    fpr = 1 - roc_obj$specificities,
    tpr = roc_obj$sensitivities,
    threshold = roc_obj$thresholds,
    model = model_name
  )
}

make_cm_data <- function(cm_obj, model_name) {
  cm_df <- as.data.frame(cm_obj$table)
  colnames(cm_df) <- c("Predicted", "Actual", "Count")
  cm_df$model <- model_name
  cm_df
}

##########################################################################################################################################
# A. MODEL METRICS
##########################################################################################################################################

model_metrics <- results %>%
  pivot_longer(
    cols = c(AUC, Brier, Sensitivity, Specificity, Balanced_Accuracy),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(model = Model) %>%
  select(model, metric, value)

saveRDS(model_metrics, "model_metrics.rds")

##########################################################################################################################################
# B. ROC-DATA
##########################################################################################################################################

roc_data <- bind_rows(
  make_roc_data(roc_glm, "Logistisk regression"),
  make_roc_data(roc_rf, "Random Forest"),
  make_roc_data(roc_tree, "Beslutningstræ")
)

saveRDS(roc_data, "roc_data.rds")

##########################################################################################################################################
# C. KALIBRERINGSDATA
##########################################################################################################################################

calibration_data <- bind_rows(
  make_calibration_data(test_data_2526_compare$maal, p_glm, "Logistisk regression", n_bins = 10),
  make_calibration_data(test_data_rf_eval$maal, p_rf, "Random Forest", n_bins = 10),
  make_calibration_data(test_tree_eval$maal, p_tree, "Beslutningstræ", n_bins = 10)
)

saveRDS(calibration_data, "calibration_data.rds")

##########################################################################################################################################
# D. KOEFFICIENTER / FEATURE-EFFEKTER
##########################################################################################################################################

coef_mat <- summary(glm_2526_unweighted)$coefficients

coef_data <- data.frame(
  term = rownames(coef_mat),
  estimate = coef_mat[, "Estimate"],
  std_error = coef_mat[, "Std. Error"],
  z_value = coef_mat[, "z value"],
  p_value = coef_mat[, "Pr(>|z|)"],
  row.names = NULL
) %>%
  filter(term != "(Intercept)")

saveRDS(coef_data, "coef_data.rds")

##########################################################################################################################################
# E. EKSTRA: CONFUSION MATRIX-DATA
##########################################################################################################################################

cm_data <- bind_rows(
  make_cm_data(cm_glm, "Logistisk regression"),
  make_cm_data(cm_rf, "Random Forest"),
  make_cm_data(cm_tree, "Beslutningstræ")
)

saveRDS(cm_data, "cm_data.rds")

##########################################################################################################################################
# F. EKSTRA: bred metric-tabel
##########################################################################################################################################

model_metrics_wide <- results
saveRDS(model_metrics_wide, "model_metrics_wide.rds")

##########################################################################################################################################
# G. Kontrol af ekstra RDS-filer
##########################################################################################################################################

cat("\n--- EKSTRA RDS-FILER GEMT ---\n")
cat("model_metrics.rds:", file.exists("model_metrics.rds"), "\n")
cat("roc_data.rds:", file.exists("roc_data.rds"), "\n")
cat("calibration_data.rds:", file.exists("calibration_data.rds"), "\n")
cat("coef_data.rds:", file.exists("coef_data.rds"), "\n")
cat("cm_data.rds:", file.exists("cm_data.rds"), "\n")
cat("model_metrics_wide.rds:", file.exists("model_metrics_wide.rds"), "\n")
cat("cm_glm_unweighted.rds:", file.exists("cm_glm_unweighted.rds"), "\n")

############ Visualiseringer itl opgave

## Viser hvorfor vi fjerner skud længere end 35 m

cutoff_table <- lapply(seq(25, 40, by = 1), function(cutoff) {
  tmp <- shots_superliga %>%
    filter(
      selvmaal == 0,
      PRIMARYTYPE != "penalty",
      PRIMARYTYPE != "free_kick",
      PRIMARYTYPE != "own_goal"
    )
  
  data.frame(
    Cutoff = cutoff,
    `Skud beholdt` = sum(tmp$afstand_til_mål < cutoff, na.rm = TRUE),
    `Skud fjernet` = sum(tmp$afstand_til_mål >= cutoff, na.rm = TRUE),
    `Mål beholdt` = sum(tmp$maal[tmp$afstand_til_mål < cutoff], na.rm = TRUE),
    `Mål fjernet` = sum(tmp$maal[tmp$afstand_til_mål >= cutoff], na.rm = TRUE),
    `Andel mål beholdt (%)` = round(
      100 * sum(tmp$maal[tmp$afstand_til_mål < cutoff], na.rm = TRUE) / sum(tmp$maal, na.rm = TRUE),
      2
    )
  )
}) %>%
  bind_rows()

cutoff_table

ggplot(cutoff_table, aes(x = Cutoff, y = Andel.mål.beholdt....)) +
  geom_line(color = "#123c7c", linewidth = 1.2) +
  geom_point(color = "#ffd400", size = 3) +
  geom_vline(xintercept = 32, linetype = "dashed", color = "green", linewidth = 1) +
  geom_vline(xintercept = 35, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "Andel af mål bevaret ved forskellige afstands-cutoffs",
    subtitle = "Grøn linje: 32 m (ingen mål fjernes) | Rød linje: 35 m (valgt cutoff)",
    x = "Cutoff (meter)",
    y = "Andel mål beholdt (%)"
  ) +
  theme_minimal()

## Fordeling af mål og skud

shots_model %>%
  mutate(Udfald = ifelse(maal == 1, "Mål", "Ikke mål")) %>%
  ggplot(aes(x = Udfald, fill = Udfald)) +
  geom_bar() +
  geom_text(
    stat = "count",
    aes(label = paste0(round(..count../sum(..count..) * 100, 1), "%")),
    vjust = -0.5
  ) +
  scale_fill_manual(values = c(
    "Mål" = "yellow",
    "Ikke mål" = "blue"
  )) +
  labs(
    title = "Fordeling af mål og ikke-mål",
    x = NULL,
    y = "Antal"
  ) +
  theme_minimal()

## Fordeling af distance til mål intervaller

afstand_plot_data <- shots_model %>%
  mutate(
    afstand_bin = cut(
      afstand_til_mål,
      breaks = c(0, 5, 10, 15, 20, 25, 30, 35),
      labels = c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35"),
      include.lowest = TRUE
    ),
    Udfald = ifelse(maal == 1, "Mål", "Ikke mål")
  ) %>%
  count(afstand_bin, Udfald) %>%
  group_by(afstand_bin) %>%
  mutate(
    pct = n / sum(n),
    pct_label = paste0(round(pct * 100, 1), "%")
  ) %>%
  ungroup()

ggplot(afstand_plot_data, aes(x = afstand_bin, y = pct, fill = Udfald)) +
  geom_col() +
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  scale_fill_manual(values = c(
    "Mål" = "#ffd400",
    "Ikke mål" = "#123c7c"
  )) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Andel af mål stiger jo tættere man kommer på mål",
    x = "Afstand til mål (meter)",
    y = "Andel (%)",
    caption = "Kilde = Wyscout"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Målrate fordelt på vinkel

vinkel_plot_data <- shots_model %>%
  mutate(
    vinkel_bin = cut(
      vinkel_mellem_stolper,
      breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 180),
      labels = c("0-20", "21-40", "41-60", "61-80", "81-100", "101-120", "121-140", "140+"),
      include.lowest = TRUE
    ),
    Udfald = ifelse(maal == 1, "Mål", "Ikke mål")
  ) %>%
  count(vinkel_bin, Udfald) %>%
  group_by(vinkel_bin) %>%
  mutate(
    pct = n / sum(n),
    pct_label = paste0(round(pct * 100, 1), "%")
  ) %>%
  ungroup()

ggplot(vinkel_plot_data, aes(x = vinkel_bin, y = pct, fill = Udfald)) +
  geom_col() +
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  scale_fill_manual(values = c(
    "Mål" = "#ffd400",
    "Ikke mål" = "#123c7c"
  )) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Andel af mål stiger jo større en vinkel",
    x = "Vinkel (grader)",
    y = "Andel (%)",
    caption = "Kilde = Wyscout"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

## cutoff

cutoff_table <- lapply(seq(25, 40, by = 1), function(cutoff) {
  tmp <- shots_superliga %>%
    filter(
      selvmaal == 0,
      PRIMARYTYPE != "penalty",
      PRIMARYTYPE != "free_kick",
      PRIMARYTYPE != "own_goal"
    )
  
  data.frame(
    Cutoff = cutoff,
    Skud_beholdt = sum(tmp$afstand_til_mål < cutoff, na.rm = TRUE),
    Skud_fjernet = sum(tmp$afstand_til_mål >= cutoff, na.rm = TRUE),
    Mål_beholdt = sum(tmp$maal[tmp$afstand_til_mål < cutoff], na.rm = TRUE),
    Mål_fjernet = sum(tmp$maal[tmp$afstand_til_mål >= cutoff], na.rm = TRUE),
    Andel_mål_beholdt = 100 * sum(tmp$maal[tmp$afstand_til_mål < cutoff], na.rm = TRUE) / sum(tmp$maal, na.rm = TRUE)
  )
}) %>%
  bind_rows()

ggplot(cutoff_table, aes(x = Cutoff, y = Skud_fjernet)) +
  geom_line(color = "#d62728", linewidth = 1.2) +
  geom_point(color = "#123c7c", size = 3) +
  geom_vline(xintercept = 32, linetype = "dashed", color = "green", linewidth = 1) +
  geom_vline(xintercept = 35, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(
    aes(label = Skud_fjernet),
    vjust = -0.8,
    size = 3
  ) +
  labs(
    title = "Antal fjernede skud ved forskellige afstands-cutoffs",
    subtitle = "Flere skud fjernes ved højere cutoff, men uden yderligere gevinst i bevarede mål efter 32 meter",
    x = "Cutoff (meter)",
    y = "Antal fjernede skud"
  ) +
  theme_minimal()

## Kontra plot

shots_model %>%
  mutate(
    Kontra = ifelse(Kontraangreb == 1, "Kontraangreb", "Ikke kontraangreb")
  ) %>%
  count(Kontra) %>%
  ggplot(aes(x = Kontra, y = n, fill = Kontra)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3, size = 4) +
  scale_fill_manual(values = c(
    "Kontraangreb" = "#ffd400",
    "Ikke kontraangreb" = "#123c7c"
  )) +
  labs(
    title = "Antal skud ved kontraangreb og ikke-kontraangreb",
    x = NULL,
    y = "Antal skud"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

kontra_plot_data <- shots_model %>%
  mutate(
    Kontra = ifelse(Kontraangreb == 1, "Kontraangreb", "Ikke kontraangreb"),
    Udfald = ifelse(maal == 1, "Mål", "Ikke mål")
  ) %>%
  count(Kontra, Udfald) %>%
  group_by(Kontra) %>%
  mutate(
    pct = n / sum(n),
    pct_label = paste0(round(pct * 100, 1), "%")
  ) %>%
  ungroup()

ggplot(kontra_plot_data, aes(x = Kontra, y = pct, fill = Udfald)) +
  geom_col() +
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) +
  scale_fill_manual(values = c(
    "Mål" = "#ffd400",
    "Ikke mål" = "#123c7c"
  )) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Fordobling i andel af mål ved kontraangreb",
    x = NULL,
    y = "Andel (%)",
    caption = "Kilde = Wyscout"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

## Mål og corner

## Corner: andel mål og ikke-mål

corner_plot <- shots_model %>%
  filter(corner == 1) %>%
  mutate(
    Udfald = ifelse(maal == 1, "Mål", "Ikke mål")
  ) %>%
  count(Udfald) %>%
  mutate(
    pct = n / sum(n),
    pct_label = paste0(round(pct * 100, 1), "%")
  )

ggplot(corner_plot, aes(x = "Hjørnespark", y = pct, fill = Udfald)) +
  geom_col(width = 0.5) +
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5
  ) +
  scale_fill_manual(values = c(
    "Ikke mål" = "#123c7c",
    "Mål" = "#ffd400"
  )) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "11% af hjørnespark bliver til mål",
    x = NULL,
    y = "Andel (%)",
    caption = "Kilde = Wyscout"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

## Laver visualisering af performance for Wyscouts egen xG

library(pROC)

roc_obj <- roc(shots_model$maal, shots_model$SHOTXG)

auc_value <- auc(roc_obj)

auc_value

brier_score <- mean((shots_model$SHOTXG - shots_model$maal)^2, na.rm = TRUE)

brier_score

threshold <- 0.1

shots_model <- shots_model %>%
  mutate(
    pred_wyscout = ifelse(SHOTXG >= threshold, 1, 0)
  )

# Confusion matrix
cm <- table(Predicted = shots_model$pred_wyscout, Actual = shots_model$maal)

cm

TP <- cm["1","1"]
TN <- cm["0","0"]
FP <- cm["1","0"]
FN <- cm["0","1"]

sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)

balanced_accuracy <- (sensitivity + specificity) / 2

balanced_accuracy

cat("AUC:", round(auc_value, 3), "\n")
cat("Brier score:", round(brier_score, 3), "\n")
cat("Sensitivity:", round(sensitivity, 3), "\n")
cat("Specificity:", round(specificity, 3), "\n")
cat("Balanced Accuracy:", round(balanced_accuracy, 3), "\n")
