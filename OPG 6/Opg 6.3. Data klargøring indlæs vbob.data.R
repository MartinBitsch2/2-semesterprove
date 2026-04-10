# ============================================================
# OPGAVE 6.3 - CSV VERSION (OPDATERET SAMLET KODE)
# ============================================================

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jsonlite)
library(deldir)
library(stringr)

# ------------------------------------------------------------
# 1) LOAD DATA
# ------------------------------------------------------------

csv <- read_csv("vbob.csv", col_names = FALSE, show_col_types = FALSE) #find den på nedenstående link
#https://erhvervsakademikbenhavn.sharepoint.com/sites/cph-Lyngby/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2Fcph%2DLyngby%2FShared%20Documents%2F4%2E%20Indhold%20%26%20Niveau%2FDAT%2FDAL%2F%C3%85rgang%20Efter%C3%A5ret%202025%2F2%2E%20semester%2FDALEksamenF26%2FEksamenF26&p=true&ga=1
meta <- fromJSON("vbob-meta.json", simplifyVector = FALSE)

names(csv)[1:2] <- c("frame", "time")

# ------------------------------------------------------------
# 2) BYG SPILLER-MAPPING FRA META
# ------------------------------------------------------------

home_meta <- bind_rows(lapply(meta$homePlayers, function(x) {
  tibble(
    playerId = x$ssiId,
    name = x$name,
    team = "home",
    number = x$number,
    position = x$position
  )
}))

away_meta <- bind_rows(lapply(meta$awayPlayers, function(x) {
  tibble(
    playerId = x$ssiId,
    name = x$name,
    team = "away",
    number = x$number,
    position = x$position
  )
}))

players_meta <- bind_rows(home_meta, away_meta)

# ------------------------------------------------------------
# 3) UDPAK CSV STRUKTUR
# ------------------------------------------------------------

# Layout ifølge lærer:
# [frame, time,
#  11 home ids,
#  11 home x,
#  11 home y,
#  11 away ids,
#  11 away x,
#  11 away y,
#  ball x, ball y]

home_ids <- 3:13
home_x   <- 14:24
home_y   <- 25:35
away_ids <- 36:46
away_x   <- 47:57
away_y   <- 58:68
ball_xy  <- 69:70

# ------------------------------------------------------------
# 4) LAV LONG DATASET
# ------------------------------------------------------------

build_team <- function(df, ids, xs, ys, team_name) {
  out <- vector("list", 11)
  
  for (i in 1:11) {
    out[[i]] <- df %>%
      transmute(
        frame = frame,
        time = time,
        playerId = .data[[names(df)[ids[i]]]],
        x = as.numeric(.data[[names(df)[xs[i]]]]),
        y = as.numeric(.data[[names(df)[ys[i]]]]),
        team = team_name
      )
  }
  
  bind_rows(out)
}

home_df <- build_team(csv, home_ids, home_x, home_y, "home")
away_df <- build_team(csv, away_ids, away_x, away_y, "away")

players <- bind_rows(home_df, away_df) %>%
  left_join(players_meta, by = c("playerId", "team"))

ball <- csv %>%
  transmute(
    frame,
    time,
    ball_x = as.numeric(.data[[names(csv)[ball_xy[1]]]]),
    ball_y = as.numeric(.data[[names(csv)[ball_xy[2]]]])
  )

# ------------------------------------------------------------
# 5) BANEDIMENSIONER OG HOLDNAVNE
# ------------------------------------------------------------

pitch_length <- meta$pitchLength
pitch_width  <- meta$pitchWidth
half_length  <- pitch_length / 2
half_width   <- pitch_width / 2

match_desc <- meta$description
teams_from_desc <- str_split(match_desc, " : ", simplify = TRUE)[1]
teams_from_desc <- str_split(teams_from_desc, " - ", simplify = TRUE)

home_team_name <- teams_from_desc[1]
away_team_name <- teams_from_desc[2]

cat("Kamp:", match_desc, "\n")
cat("Home:", home_team_name, "\n")
cat("Away:", away_team_name, "\n")
cat("Bane:", pitch_length, "x", pitch_width, "\n")

# ------------------------------------------------------------
# 6) FIND PLAUSIBLE MÅLSEKVENSER
# ------------------------------------------------------------

goal_candidates <- ball %>%
  filter(abs(ball_x) >= (half_length - 0.5)) %>%
  arrange(frame)

goal_candidates_seq <- goal_candidates %>%
  mutate(
    new_seq = c(TRUE, diff(frame) > 10),
    seq_id = cumsum(new_seq)
  )

goal_sequences <- goal_candidates_seq %>%
  group_by(seq_id) %>%
  summarise(
    start_frame = min(frame),
    end_frame = max(frame),
    start_time = min(time),
    end_time = max(time),
    mean_ball_x = mean(ball_x, na.rm = TRUE),
    mean_ball_y = mean(ball_y, na.rm = TRUE),
    n_frames = n(),
    .groups = "drop"
  ) %>%
  arrange(start_frame)

plausible_goals <- goal_sequences %>%
  filter(abs(mean_ball_y) <= 3.66) %>%
  arrange(start_frame)

cat("\nPlausible målsekvenser:\n")
print(plausible_goals, n = Inf)

# Første plausible målsekvens fundet tidligere
chosen_seq <- 17
chosen_goal <- goal_sequences %>% filter(seq_id == chosen_seq)

goal_frame <- floor((chosen_goal$start_frame + chosen_goal$end_frame) / 2)

cat("\nValgt målsekvens:\n")
print(chosen_goal)
cat("Valgt goal_frame:", goal_frame, "\n")

frames <- seq(goal_frame - 25, goal_frame + 25, by = 5)

cat("\nFrames til analyse:\n")
print(frames)

# ------------------------------------------------------------
# 7) FIND ANGRIBENDE HOLD I DEN VALGTE SEKvens
# ------------------------------------------------------------

# Brug spillerpositioner i 1. halvleg til at afgøre retning:
# hold med negativ mean x i starten antages at angribe mod højre mål i 1. halvleg

direction_tbl <- players %>%
  filter(frame <= 3000) %>%
  group_by(team) %>%
  summarise(mean_x = mean(x, na.rm = TRUE), .groups = "drop") %>%
  mutate(attacks_to = ifelse(mean_x < 0, "right_goal", "left_goal"))

print(direction_tbl)

goal_side <- ifelse(chosen_goal$mean_ball_x < 0, "left_goal", "right_goal")

attacking_team <- direction_tbl %>%
  filter(attacks_to == goal_side) %>%
  pull(team)

attacking_team <- attacking_team[1]
defending_team <- ifelse(attacking_team == "home", "away", "home")

attacking_team_name <- ifelse(attacking_team == "home", home_team_name, away_team_name)
defending_team_name <- ifelse(defending_team == "home", home_team_name, away_team_name)

cat("\nMålet sker ved:", goal_side, "\n")
cat("Angribende hold:", attacking_team_name, "(", attacking_team, ")\n")
cat("Forsvarende hold:", defending_team_name, "(", defending_team, ")\n")

# ------------------------------------------------------------
# 8) Delaunay-plot for valgt frame
# ------------------------------------------------------------

frame_plot <- goal_frame

df_frame <- players %>%
  filter(frame == frame_plot)

tri <- deldir(df_frame$x, df_frame$y)

delaunay_plot <- ggplot() +
  geom_segment(
    data = tri$delsgs,
    aes(x = x1, y = y1, xend = x2, yend = y2)
  ) +
  geom_point(
    data = df_frame,
    aes(x = x, y = y, color = team),
    size = 3
  ) +
  geom_text(
    data = df_frame,
    aes(x = x, y = y, label = number),
    nudge_y = 1
  ) +
  coord_fixed(xlim = c(-half_length, half_length),
              ylim = c(-half_width, half_width)) +
  theme_minimal() +
  labs(
    title = paste("Delaunay-triangulering - frame", frame_plot),
    subtitle = paste("Første målsekvens | Angribende hold:", attacking_team_name)
  )

print(delaunay_plot)

# ------------------------------------------------------------
# 9) GRID-METODE TIL RUM I MODSTANDERENS FELT
# ------------------------------------------------------------

if (goal_side == "right_goal") {
  box_xmin <- half_length - 16.5
  box_xmax <- half_length
} else {
  box_xmin <- -half_length
  box_xmax <- -half_length + 16.5
}

box_ymin <- -20.16
box_ymax <- 20.16

grid <- expand.grid(
  x = seq(box_xmin, box_xmax, by = 1),
  y = seq(box_ymin, box_ymax, by = 1)
)

compute_space <- function(f) {
  df <- players %>%
    filter(frame == f)
  
  mat <- as.matrix(df[, c("x", "y")])
  
  nearest <- apply(grid, 1, function(p) {
    which.min((mat[,1] - p[1])^2 + (mat[,2] - p[2])^2)
  })
  
  area <- tibble(id = nearest) %>%
    count(id, name = "cells") %>%
    mutate(area = cells)
  
  df %>%
    mutate(id = row_number()) %>%
    left_join(area, by = "id") %>%
    mutate(area = ifelse(is.na(area), 0, area))
}

space <- bind_rows(lapply(frames, compute_space))

space_box <- space %>%
  filter(
    team == attacking_team,
    x > box_xmin, x < box_xmax,
    y > box_ymin, y < box_ymax
  )

summary_space <- space_box %>%
  group_by(playerId, name, number, position) %>%
  summarise(
    mean_area = mean(area, na.rm = TRUE),
    max_area = max(area, na.rm = TRUE),
    n_frames = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_area))

cat("\nSpillere med mest rum i modstanderens felt:\n")
print(summary_space)

# ------------------------------------------------------------
# 10) SØJLEDIAGRAM
# ------------------------------------------------------------

space_plot <- ggplot(
  summary_space,
  aes(
    x = reorder(paste0(name, " (#", number, ", ", position, ")"), mean_area),
    y = mean_area
  )
) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Spillere med mest rum i modstanderens felt",
    subtitle = paste("Første målsekvens | Angribende hold:", attacking_team_name),
    x = "Spiller",
    y = "Gennemsnitligt areal"
  )

print(space_plot)

# ------------------------------------------------------------
# 11) RESULTATTABEL
# ------------------------------------------------------------

result_table <- summary_space %>%
  mutate(team_name = attacking_team_name) %>%
  select(team_name, player_name = name, shirt_number = number, position, mean_area, max_area, n_frames)

cat("\nResultattabel:\n")
print(result_table)

# ------------------------------------------------------------
# 12) OUTPUT TIL OPGAVEN
# ------------------------------------------------------------

cat("\n=====================================================\n")
cat("SVARHJÆLP TIL OPGAVEN\n")
cat("=====================================================\n")

cat("\n2) Frames der dækker første mål:\n")
cat("Start frame:", chosen_goal$start_frame, "\n")
cat("Slut frame:", chosen_goal$end_frame, "\n")
cat("Valgt visualiseringsframe:", goal_frame, "\n")

cat("\n3) Relevante frames til plot og rum-analyse:\n")
print(frames)

cat("\n5) Spillere med mest rum i modstanderens felt:\n")
print(result_table)
###############################
#####
# ============================================================
# TOP PLOTS TIL OPGAVE 6.3
# ============================================================

library(dplyr)
library(ggplot2)
library(deldir)
library(stringr)

# ------------------------------------------------------------
# 1) Klargør navne til plots
# ------------------------------------------------------------

players_pretty <- players %>%
  mutate(
    short_name = case_when(
      str_detect(name, "\\.") ~ str_trim(name),
      TRUE ~ word(name, -1)
    ),
    team_label = case_when(
      team == "home" ~ home_team_name,
      team == "away" ~ away_team_name,
      TRUE ~ team
    )
  )

ball_pretty <- ball

# ------------------------------------------------------------
# 2) Funktion til at tegne fodboldbane
# ------------------------------------------------------------

plot_pitch_custom <- function() {
  center_circle_r <- 9.15
  penalty_box_length <- 16.5
  penalty_box_width <- 40.32
  six_box_length <- 5.5
  six_box_width <- 18.32
  penalty_spot_dist <- 11
  
  ggplot() +
    # Ydre linjer
    geom_rect(
      aes(
        xmin = -half_length, xmax = half_length,
        ymin = -half_width, ymax = half_width
      ),
      fill = "#1f7a3d", color = "white", linewidth = 1
    ) +
    # Midterlinje
    geom_segment(
      aes(x = 0, xend = 0, y = -half_width, yend = half_width),
      color = "white", linewidth = 1
    ) +
    # Midtercirkel
    annotate(
      "path",
      x = center_circle_r * cos(seq(0, 2*pi, length.out = 200)),
      y = center_circle_r * sin(seq(0, 2*pi, length.out = 200)),
      color = "white", linewidth = 1
    ) +
    # Midtpunkt
    geom_point(aes(x = 0, y = 0), color = "white", size = 2) +
    
    # Venstre straffesparksfelt
    geom_rect(
      aes(
        xmin = -half_length,
        xmax = -half_length + penalty_box_length,
        ymin = -penalty_box_width/2,
        ymax = penalty_box_width/2
      ),
      fill = NA, color = "white", linewidth = 1
    ) +
    # Højre straffesparksfelt
    geom_rect(
      aes(
        xmin = half_length - penalty_box_length,
        xmax = half_length,
        ymin = -penalty_box_width/2,
        ymax = penalty_box_width/2
      ),
      fill = NA, color = "white", linewidth = 1
    ) +
    
    # Venstre målfelt
    geom_rect(
      aes(
        xmin = -half_length,
        xmax = -half_length + six_box_length,
        ymin = -six_box_width/2,
        ymax = six_box_width/2
      ),
      fill = NA, color = "white", linewidth = 1
    ) +
    # Højre målfelt
    geom_rect(
      aes(
        xmin = half_length - six_box_length,
        xmax = half_length,
        ymin = -six_box_width/2,
        ymax = six_box_width/2
      ),
      fill = NA, color = "white", linewidth = 1
    ) +
    
    # Straffesparkspletter
    geom_point(
      aes(x = -half_length + penalty_spot_dist, y = 0),
      color = "white", size = 2
    ) +
    geom_point(
      aes(x = half_length - penalty_spot_dist, y = 0),
      color = "white", size = 2
    ) +
    
    coord_fixed(
      xlim = c(-half_length - 2, half_length + 2),
      ylim = c(-half_width - 2, half_width + 2)
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#1f7a3d", color = NA),
      panel.background = element_rect(fill = "#1f7a3d", color = NA),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", size = 18, color = "black"),
      plot.subtitle = element_text(size = 13, color = "black")
    )
}

# ------------------------------------------------------------
# 3) Pænt Delaunay-plot på rigtig bane
# ------------------------------------------------------------

frame_plot <- goal_frame

df_frame <- players_pretty %>%
  filter(frame == frame_plot)

ball_frame <- ball_pretty %>%
  filter(frame == frame_plot)

tri <- deldir(df_frame$x, df_frame$y)
tri_seg <- tri$delsgs

delaunay_plot_top <- plot_pitch_custom() +
  geom_segment(
    data = tri_seg,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    color = "black",
    linewidth = 0.6,
    alpha = 0.85
  ) +
  geom_point(
    data = df_frame,
    aes(x = x, y = y, fill = team_label),
    shape = 21,
    color = "white",
    size = 5,
    stroke = 1.2
  ) +
  geom_text(
    data = df_frame,
    aes(x = x, y = y, label = number),
    color = "white",
    fontface = "bold",
    size = 3.4
  ) +
  geom_text(
    data = df_frame,
    aes(x = x, y = y - 3, label = short_name),
    color = "white",
    size = 3.1,
    fontface = "bold"
  ) +
  geom_point(
    data = ball_frame,
    aes(x = ball_x, y = ball_y),
    color = "white",
    fill = "black",
    shape = 21,
    size = 3.8,
    stroke = 1
  ) +
  geom_rect(
    aes(
      xmin = box_xmin, xmax = box_xmax,
      ymin = box_ymin, ymax = box_ymax
    ),
    fill = NA, color = "gold", linewidth = 1.2, linetype = "dashed"
  ) +
  labs(
    title = paste("Delaunay-triangulering – frame", frame_plot),
    subtitle = paste(
      "Første målsekvens | Angribende hold:", attacking_team_name,
      "| Gult område = modstanderens felt"
    )
  ) +
  theme(
    legend.text = element_text(size = 11, face = "bold")
  )

print(delaunay_plot_top)

# ------------------------------------------------------------
# 4) Gør resultattabellen pænere til søjlediagram
# ------------------------------------------------------------

plot_table <- summary_space %>%
  mutate(
    player_label = paste0(name, " (#", number, ", ", position, ")")
  ) %>%
  arrange(mean_area)

# ------------------------------------------------------------
# 5) Top class søjlediagram
# ------------------------------------------------------------

space_plot_top <- ggplot(
  plot_table,
  aes(x = reorder(player_label, mean_area), y = mean_area)
) +
  geom_col(width = 0.65, fill = "grey25") +
  geom_text(
    aes(label = round(mean_area, 1)),
    hjust = -0.15,
    size = 4,
    fontface = "bold"
  ) +
  coord_flip(clip = "off") +
  labs(
    title = "Spillere med mest rum i modstanderens felt",
    subtitle = paste(
      "Første målsekvens | Angribende hold:", attacking_team_name
    ),
    x = NULL,
    y = "Gennemsnitligt areal"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(size = 13, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(10, 40, 10, 10) # <--- Løsningen er her
  ) +
  expand_limits(y = max(plot_table$mean_area, na.rm = TRUE) * 1.12)

print(space_plot_top)

# ------------------------------------------------------------
# 6) Valgfrit: gem plots i høj opløsning
# ------------------------------------------------------------

ggsave("delaunay_top_class.png", delaunay_plot_top, width = 14, height = 9, dpi = 300)
ggsave("space_plot_top_class.png", space_plot_top, width = 13, height = 8, dpi = 300)
####
plot_table <- summary_space %>%
  mutate(
    player_label = paste0(name, " (#", number, ", ", position, ")")
  ) %>%
  arrange(mean_area)

space_plot_top <- ggplot(
  plot_table,
  aes(x = reorder(player_label, mean_area), y = mean_area)
) +
  geom_col(
    fill = "#d71920",
    width = 0.62
  ) +
  geom_text(
    aes(label = round(mean_area, 1)),
    hjust = -0.15,
    size = 5,
    fontface = "bold",
    color = "black"
  ) +
  coord_flip(clip = "off") +
  labs(
    title = "Hvem har mest rum i modstanderens felt?",
    subtitle = paste("Første målsekvens • Angribende hold:", attacking_team_name),
    x = NULL,
    y = expression("Gennemsnitligt areal (m"^2*")")
  ) +
  # Tilføjet na.rm = TRUE for at undgå fejl ved NA-værdier
  expand_limits(y = max(plot_table$mean_area, na.rm = TRUE) * 1.12) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0),
    # FØRSTE RETTELSE: ggplot2:: tilføjet
    plot.subtitle = element_text(size = 15, margin = ggplot2::margin(b = 15)), 
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 15, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.5),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    panel.background = element_rect(fill = "#f7f7f7", color = NA),
    # ANDEN RETTELSE: ggplot2:: tilføjet
    plot.margin = ggplot2::margin(15, 50, 15, 15) 
  )

print(space_plot_top)
