#### Plot til målet, opgave
library(dplyr)
library(ggplot2)
library(stringr)
library(tibble)

# ------------------------------------------------------------
# DATA
# ------------------------------------------------------------

goal_start <- chosen_goal$start_frame
goal_end   <- chosen_goal$end_frame
goal_mid   <- goal_frame

ball_seq <- ball %>%
  filter(frame >= goal_start, frame <= goal_end) %>%
  mutate(
    point_type = case_when(
      frame == goal_start ~ "Start",
      frame == goal_mid ~ "Valgt frame",
      frame == goal_end ~ "Slut",
      TRUE ~ "Sekvens"
    )
  )

players_mid <- players %>%
  filter(frame == goal_mid) %>%
  mutate(
    name_clean = str_squish(name),
    team_label = ifelse(team == "home", home_team_name, away_team_name)
  )

# ------------------------------------------------------------
# IDENTIFICÉR MÅLSCOREREN
# ------------------------------------------------------------

scorer_row <- players_mid %>%
  filter(str_detect(name_clean, "Frokjaer|Frøkjær|Froekjaer"))

if (nrow(scorer_row) == 0) {
  scorer_row <- players_mid %>%
    filter(team_label == "OB", number == 29)
}

if (nrow(scorer_row) == 0) {
  stop("Mads Frøkjær-Jensen findes ikke i data i dette frame")
}



# ------------------------------------------------------------
# IDENTIFICÉR MÅLMÆND
# ------------------------------------------------------------

gk_ids <- bind_rows(
  home_meta %>% filter(position == "GK"),
  away_meta %>% filter(position == "GK")
) %>% pull(playerId)

gk_rows <- players_mid %>%
  filter(playerId %in% gk_ids)

# ------------------------------------------------------------
# BOLD I VALGT FRAME
# ------------------------------------------------------------

ball_mid <- ball_seq %>% filter(frame == goal_mid)

# 2 nærmeste øvrige spillere
near_rows <- players_mid %>%
  mutate(
    dist_to_ball = sqrt((x - ball_mid$ball_x[1])^2 + (y - ball_mid$ball_y[1])^2)
  ) %>%
  filter(!(playerId %in% gk_ids)) %>%
  arrange(dist_to_ball) %>%
  slice(1:2)

players_plot <- bind_rows(
  scorer_row,
  gk_rows,
  near_rows
) %>%
  distinct(playerId, .keep_all = TRUE)

# ------------------------------------------------------------
# TIDSPUNKT
# ------------------------------------------------------------

goal_seconds <- chosen_goal$start_time
goal_minute <- floor(goal_seconds / 60)
goal_second <- floor(goal_seconds %% 60)

# ------------------------------------------------------------
# ZOOM
# ------------------------------------------------------------

xlim_use <- c(-58.5, -37.5)
ylim_use <- c(-25, 10)

goal_x <- -52.5
goal_post_y1 <- -3.66
goal_post_y2 <-  3.66

penalty_box_xmin <- -52.5
penalty_box_xmax <- -36.0
penalty_box_ymin <- -20.16
penalty_box_ymax <-  20.16

six_box_xmin <- -52.5
six_box_xmax <- -47.0
six_box_ymin <- -9.16
six_box_ymax <-  9.16

penalty_spot_x <- -41.5
penalty_spot_y <- 0

# ------------------------------------------------------------
# INFOBOKS
# ------------------------------------------------------------

info_df <- tibble(
  x = -43.8,
  y = 8.3,
  label = paste(
    "Første målsekvens: 36613–36642",
    paste0("Tidspunkt: ca. ", goal_minute, ":", sprintf("%02d", goal_second)),
    "Scorer: Mads Frøkjær-Jensen",
    "Situation: ligner et indlæg/skud",
    sep = "\n"
  )
)

# ------------------------------------------------------------
# PLOT
# ------------------------------------------------------------

goal_sequence_plot_exam_final3 <- ggplot() +
  # baggrund
  geom_rect(
    aes(xmin = -52.5, xmax = 52.5, ymin = -34, ymax = 34),
    fill = "#1f7a3d", color = NA
  ) +
  
  # straffesparksfelt
  geom_rect(
    aes(
      xmin = penalty_box_xmin, xmax = penalty_box_xmax,
      ymin = penalty_box_ymin, ymax = penalty_box_ymax
    ),
    fill = NA, color = "white", linewidth = 1
  ) +
  
  # målfelt
  geom_rect(
    aes(
      xmin = six_box_xmin, xmax = six_box_xmax,
      ymin = six_box_ymin, ymax = six_box_ymax
    ),
    fill = NA, color = "white", linewidth = 1
  ) +
  
  # mållinje
  geom_segment(
    aes(x = goal_x, xend = goal_x, y = -25, yend = 10),
    color = "white", linewidth = 1
  ) +
  
  # målramme
  geom_segment(
    aes(x = goal_x, xend = goal_x - 0.8, y = goal_post_y1, yend = goal_post_y1),
    color = "gold",
    linewidth = 1.6
  ) +
  geom_segment(
    aes(x = goal_x, xend = goal_x - 0.8, y = goal_post_y2, yend = goal_post_y2),
    color = "gold",
    linewidth = 1.6
  ) +
  geom_segment(
    aes(x = goal_x - 0.8, xend = goal_x - 0.8, y = goal_post_y1, yend = goal_post_y2),
    color = "gold",
    linewidth = 1.6
  ) +
  
  # straffesparksplet
  geom_point(
    aes(x = penalty_spot_x, y = penalty_spot_y),
    color = "white",
    size = 2
  ) +
  
  # titel og undertitel
  annotate(
    "text",
    x = -58.8, y = 11.5,
    label = "Identifikation af første målsekvens",
    hjust = 0,
    size = 6,
    fontface = "bold",
    color = "black"
  ) +
  annotate(
    "text",
    x = -56.8, y = 10.0,
    label = "Trackingdata lokaliserer sekvensen; video identificerer målscoreren",
    hjust = 0,
    size = 4,
    color = "black"
  ) +
  
  # boldens bane
  geom_path(
    data = ball_seq,
    aes(x = ball_x, y = ball_y),
    color = "black",
    linewidth = 1
  ) +
  
  # sekvenspunkter
  geom_point(
    data = ball_seq %>% filter(point_type == "Sekvens"),
    aes(x = ball_x, y = ball_y),
    shape = 21,
    fill = "black",
    color = "white",
    size = 2.2,
    stroke = 0.4
  ) +
  
  # start / valgt frame / slut
  geom_point(
    data = ball_seq %>% filter(point_type %in% c("Start", "Valgt frame", "Slut")),
    aes(x = ball_x, y = ball_y, fill = point_type),
    shape = 21,
    color = "white",
    size = 5,
    stroke = 1
  ) +
  
  # ekstra tydelig slutmarkering
  geom_point(
    data = ball_seq %>% filter(point_type == "Slut"),
    aes(x = ball_x, y = ball_y),
    size = 6,
    shape = 21,
    fill = "#d7191c",
    color = "white",
    stroke = 1.2
  ) +
  
  # spillere
  geom_point(
    data = players_plot,
    aes(x = x, y = y, fill = team_label),
    shape = 21,
    color = "white",
    size = 4.8,
    stroke = 1
  ) +
  geom_text(
    data = players_plot,
    aes(x = x, y = y, label = number),
    color = "white",
    fontface = "bold",
    size = 3
  ) +
  
  # highlight omkring målscorer
  geom_point(
    data = scorer_row,
    aes(x = x, y = y),
    size = 8,
    shape = 21,
    fill = NA,
    color = "#FFD700",
    stroke = 1.8
  ) +
  
  # label scorer
  geom_label(
    data = scorer_row,
    aes(x = x + 0.9, y = y + 1.4, label = "Scorer\nM. Frøkjær-Jensen"),
    fill = "#FFD700",
    color = "black",
    fontface = "bold",
    size = 3.3,
    linewidth = 0.25
  ) +
  
  # labels til øvrige spillere
  geom_text(
    data = players_plot %>% filter(!(playerId %in% scorer_row$playerId)),
    aes(x = x + 0.55, y = y - 0.9, label = name_clean),
    color = "white",
    hjust = 0,
    size = 3,
    fontface = "bold"
  ) +
  
  # labels på boldpunkter
  geom_text(
    data = ball_seq %>% filter(point_type == "Start"),
    aes(x = ball_x + 0.3, y = ball_y + 1.2, label = "Start"),
    color = "white",
    hjust = 0,
    size = 3.1,
    fontface = "bold"
  ) +
  geom_text(
    data = ball_seq %>% filter(point_type == "Valgt frame"),
    aes(x = ball_x - 0.3, y = ball_y + 1.2, label = "Valgt frame"),
    color = "white",
    hjust = 1,
    size = 3.1,
    fontface = "bold"
  ) +
  geom_text(
    data = ball_seq %>% filter(point_type == "Slut"),
    aes(x = ball_x + 0.25, y = ball_y + 1.2, label = "Slut"),
    color = "white",
    hjust = 0,
    size = 3.1,
    fontface = "bold"
  ) +
  
  # infoboks
  geom_label(
    data = info_df,
    aes(x = x, y = y, label = label),
    hjust = 0,
    vjust = 1,
    size = 3.45,
    fontface = "bold",
    fill = "white",
    color = "black",
    linewidth = 0.25
  ) +
  
  scale_fill_manual(
    values = c(
      "Start" = "#2c7fb8",
      "Valgt frame" = "#fdbf11",
      "Slut" = "#d7191c",
      "OB" = "#e45756",
      "VB" = "#17becf"
    ),
    breaks = c("OB", "VB", "Start", "Valgt frame", "Slut")
  ) +
  
  coord_fixed(xlim = xlim_use, ylim = ylim_use, clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#1f7a3d", color = NA),
    panel.background = element_rect(fill = "#1f7a3d", color = NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )



ggsave("goal_sequence_plot_exam_final3.png", goal_sequence_plot_exam_final3, width = 11, height = 6.8, dpi = 300)