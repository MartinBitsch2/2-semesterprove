#### Ny kode til Delspørgsmål 5, i opgaven 6.3
# ============================================================
# DELSPØRGSMÅL 5 - HELE KAMPEN
# Spillere med mest rum i modstanderens felt (begge hold)
# ============================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(stringr)

# ------------------------------------------------------------
# 1) INDSTILLINGER
# ------------------------------------------------------------

grid_spacing <- 1     # 1x1 meter grid
frame_step   <- 5     # sæt til 1 hvis I vil bruge ALLE frames

# ------------------------------------------------------------
# 2) BANEMÅL
# ------------------------------------------------------------

pitch_length <- meta$pitchLength
pitch_width  <- meta$pitchWidth

half_length <- pitch_length / 2
half_width  <- pitch_width / 2

# Straffesparksfeltets mål
penalty_box_length <- 16.5
penalty_box_width  <- 40.32

# Venstre og højre felt
left_box <- list(
  xmin = -half_length,
  xmax = -half_length + penalty_box_length,
  ymin = -penalty_box_width / 2,
  ymax =  penalty_box_width / 2
)

right_box <- list(
  xmin =  half_length - penalty_box_length,
  xmax =  half_length,
  ymin = -penalty_box_width / 2,
  ymax =  penalty_box_width / 2
)

# ------------------------------------------------------------
# 3) KLARGØR FRAMES / HALVEGE
# ------------------------------------------------------------

frame_time <- players %>%
  distinct(frame, time) %>%
  arrange(frame)

# Forsøg at finde pausen via største spring i time
time_diff <- diff(frame_time$time)

if (length(time_diff) > 0 && max(time_diff, na.rm = TRUE) > median(time_diff, na.rm = TRUE) * 10) {
  split_idx <- which.max(time_diff)
  halftime_frame <- frame_time$frame[split_idx]
} else {
  # fallback hvis pausen ikke kan opdages tydeligt
  halftime_frame <- median(frame_time$frame, na.rm = TRUE)
}

cat("Halftime split frame:", halftime_frame, "\n")

players_full <- players %>%
  mutate(
    half = ifelse(frame <= halftime_frame, 1, 2)
  ) %>%
  filter(!is.na(x), !is.na(y), !is.na(team), !is.na(name), !is.na(number), !is.na(position))

# evt. neddrosling for hurtigere beregning
frames_used <- sort(unique(players_full$frame))
frames_used <- frames_used[seq(1, length(frames_used), by = frame_step)]

players_used <- players_full %>%
  filter(frame %in% frames_used)

# ------------------------------------------------------------
# 4) FIND ANGREBSRETNING PR. HOLD PR. HALVLEG
# ------------------------------------------------------------
# Idé:
# Hvis et hold i gennemsnit står på venstre side (mean x < 0),
# antages de at angribe mod højre mål i den halvleg.

direction_table <- players_used %>%
  group_by(team, half) %>%
  summarise(
    mean_x = mean(x, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    attacks_to = ifelse(mean_x < 0, "right_goal", "left_goal"),
    opponent_box_side = ifelse(attacks_to == "right_goal", "right", "left")
  )

print(direction_table)

# ------------------------------------------------------------
# 5) GRID I FELTET
# ------------------------------------------------------------

make_box_grid <- function(box, spacing = 1) {
  expand.grid(
    x = seq(box$xmin, box$xmax, by = spacing),
    y = seq(box$ymin, box$ymax, by = spacing)
  ) %>%
    as_tibble()
}

grid_left  <- make_box_grid(left_box,  grid_spacing)
grid_right <- make_box_grid(right_box, grid_spacing)

cell_area <- grid_spacing * grid_spacing

# ------------------------------------------------------------
# 6) HJÆLPEFUNKTIONER
# ------------------------------------------------------------

player_in_box <- function(df, box) {
  df %>%
    filter(
      x >= box$xmin, x <= box$xmax,
      y >= box$ymin, y <= box$ymax
    )
}

compute_space_for_team_frame <- function(df_frame, attacking_team, half_now, direction_table,
                                         left_box, right_box, grid_left, grid_right, cell_area) {
  
  # Angrebsretning for holdet i denne halvleg
  dir_row <- direction_table %>%
    filter(team == attacking_team, half == half_now)
  
  if (nrow(dir_row) == 0) return(tibble())
  
  box_side <- dir_row$opponent_box_side[1]
  
  if (box_side == "left") {
    box <- left_box
    grid_box <- grid_left
  } else {
    box <- right_box
    grid_box <- grid_right
  }
  
  # Alle spillere i frame
  if (nrow(df_frame) == 0) return(tibble())
  
  # Angribende spillere, der fysisk står i modstanderens felt
  attackers_in_box <- df_frame %>%
    filter(team == attacking_team) %>%
    player_in_box(box)
  
  if (nrow(attackers_in_box) == 0) return(tibble())
  
  # Nærmeste spiller til hvert gridpunkt blandt ALLE spillere i framen
  # ------------------------------------------------------------
  # Fjern målmænd
  # ------------------------------------------------------------
  df_frame <- df_frame %>%
    filter(position != "GK")
  
  # ------------------------------------------------------------
  # Find boldens position i dette frame
  # ------------------------------------------------------------
  ball_pos <- ball %>% filter(frame == unique(df_frame$frame))
  
  if (nrow(ball_pos) == 0) return(tibble())
  
  # ------------------------------------------------------------
  # Behold kun spillere tæt på bolden (relevant spil)
  # ------------------------------------------------------------
  df_frame <- df_frame %>%
    mutate(
      dist_to_ball = sqrt((x - ball_pos$ball_x)^2 + (y - ball_pos$ball_y)^2)
    ) %>%
    filter(dist_to_ball <= 20)
  
  if (nrow(df_frame) == 0) return(tibble())
  
  # Alle markspillere i framen skal konkurrere om rummet
  all_players <- df_frame %>%
    filter(position != "GK")
  
  if (nrow(all_players) == 0) return(tibble())
  
  # Angribende spillere, som står i modstanderens felt
  attacking_players_in_box <- all_players %>%
    filter(team == attacking_team) %>%
    player_in_box(box)
  
  if (nrow(attacking_players_in_box) == 0) return(tibble())
  
  player_mat <- as.matrix(all_players[, c("x", "y")])
  grid_mat   <- as.matrix(grid_box[, c("x", "y")])
  
  nearest_idx <- apply(grid_mat, 1, function(gp) {
    which.min((player_mat[,1] - gp[1])^2 + (player_mat[,2] - gp[2])^2)
  })
  
  area_tbl <- tibble(row_id = nearest_idx) %>%
    count(row_id, name = "n_cells") %>%
    mutate(area_m2 = n_cells * cell_area)
  
  all_players_with_area <- all_players %>%
    mutate(row_id = row_number()) %>%
    left_join(area_tbl, by = "row_id") %>%
    mutate(
      n_cells = ifelse(is.na(n_cells), 0, n_cells),
      area_m2 = ifelse(is.na(area_m2), 0, area_m2)
    )
  
  all_players_with_area %>%
    filter(team == attacking_team) %>%
    player_in_box(box) %>%
    mutate(
      box_side = box_side,
      half = half_now
    )
  grid_mat   <- as.matrix(grid_box[, c("x", "y")])
  
  nearest_idx <- apply(grid_mat, 1, function(gp) {
    which.min((player_mat[,1] - gp[1])^2 + (player_mat[,2] - gp[2])^2)
  })
  
  area_tbl <- tibble(row_id = nearest_idx) %>%
    count(row_id, name = "n_cells") %>%
    mutate(area_m2 = n_cells * cell_area)
  
  df_frame_with_area <- df_frame %>%
    mutate(row_id = row_number()) %>%
    left_join(area_tbl, by = "row_id") %>%
    mutate(
      n_cells = ifelse(is.na(n_cells), 0, n_cells),
      area_m2 = ifelse(is.na(area_m2), 0, area_m2)
    )
  
  # Kun angribende spillere, som er i modstanderens felt
  df_frame_with_area %>%
    filter(team == attacking_team) %>%
    player_in_box(box) %>%
    mutate(
      box_side = box_side,
      half = half_now
    )
}

# ------------------------------------------------------------
# 7) BEREGN RUM FOR BEGGE HOLD I ALLE FRAMES
# ------------------------------------------------------------

all_results <- vector("list", length(frames_used) * 2)
counter <- 1

for (f in frames_used) {
  
  df_frame <- players_used %>%
    filter(frame == f)
  
  if (nrow(df_frame) == 0) next
  
  half_now <- unique(df_frame$half)
  if (length(half_now) != 1) next
  half_now <- half_now[1]
  
  # home
  res_home <- compute_space_for_team_frame(
    df_frame = df_frame,
    attacking_team = "home",
    half_now = half_now,
    direction_table = direction_table,
    left_box = left_box,
    right_box = right_box,
    grid_left = grid_left,
    grid_right = grid_right,
    cell_area = cell_area
  )
  
  all_results[[counter]] <- res_home
  counter <- counter + 1
  
  # away
  res_away <- compute_space_for_team_frame(
    df_frame = df_frame,
    attacking_team = "away",
    half_now = half_now,
    direction_table = direction_table,
    left_box = left_box,
    right_box = right_box,
    grid_left = grid_left,
    grid_right = grid_right,
    cell_area = cell_area
  )
  
  all_results[[counter]] <- res_away
  counter <- counter + 1
}

space_all_match <- bind_rows(all_results)

# ------------------------------------------------------------
# 8) OPSUMMÉR PR. SPILLER
# ------------------------------------------------------------

space_summary_match <- space_all_match %>%
  mutate(
    team_label = ifelse(team == "home", home_team_name, away_team_name),
    name = str_squish(name)
  ) %>%
  group_by(team, team_label, playerId, name, number, position) %>%
  summarise(
    total_area_m2 = sum(area_m2, na.rm = TRUE),
    mean_area_m2 = mean(area_m2, na.rm = TRUE),
    max_area_m2 = max(area_m2, na.rm = TRUE),
    frames_in_box = n(),
    .groups = "drop"
  ) %>%
  mutate(
    share_pct = 100 * total_area_m2 / sum(total_area_m2, na.rm = TRUE)
  ) %>%
  arrange(desc(total_area_m2))

print(space_summary_match, n = 25)

# ------------------------------------------------------------
# 9) TOP 10 TIL PLOT
# ------------------------------------------------------------

top_n_players <- 10

plot_df <- space_summary_match %>%
  group_by(team_label) %>%
  slice_max(order_by = total_area_m2, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    player_label = paste0(name, "\n", team_label, " • #", number, " • ", position),
    player_label = reorder(player_label, mean_area_m2)
  )

print(plot_df)

# ------------------------------------------------------------
# 10) SØJLEDIAGRAM
# ------------------------------------------------------------

space_plot_whole_match <- ggplot(
  plot_df,
  aes(x = reorder(player_label, mean_area_m2), y = mean_area_m2, fill = team_label)
) +
  geom_col(width = 0.68) +
  geom_text(
    aes (label = round(mean_area_m2, 1)),
    vjust = -0.35,
    size = 4.3,
    fontface = "bold",
    color = "black"
  ) +
  labs(
    title = "Jebali dominerer OB’s rum i feltet – VB fordeler det bredere",
    subtitle = paste0(
      "Hele kampen | Top ", top_n_players,
      " spillere | Gennemsnitligt rum (m²) pr. frame i modstanderens felt"
    ),
    x = "Spiller",
    y = expression("Gennemsnitligt areal (m"^2*")"),
    fill = "Hold"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 13, margin = ggplot2::margin(b = 14)),
    axis.text.x = element_text(size = 11, face = "bold", lineheight = 0.95),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    panel.background = element_rect(fill = "#f7f7f7", color = NA)
  ) +
  expand_limits(y = max(plot_df$mean_area_m2, na.rm = TRUE) * 1.12)

print(space_plot_whole_match)

ggsave(
  "space_plot_whole_match_top10.png",
  space_plot_whole_match,
  width = 12,
  height = 8,
  dpi = 300
)

# ------------------------------------------------------------
# 11) EKSTRA TABEL TIL BRUG I TEKSTEN
# ------------------------------------------------------------

result_table_match <- space_summary_match %>%
  mutate(
    rank_total_area = row_number()
  ) %>%
  select(
    rank_total_area,
    team_label,
    player_name = name,
    shirt_number = number,
    position,
    total_area_m2,
    mean_area_m2,
    max_area_m2,
    frames_in_box,
    share_pct
  )

print(result_table_match, n = 25)

