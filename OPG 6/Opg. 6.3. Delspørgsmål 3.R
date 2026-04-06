#### Opgave 6. opgave 6.3 
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# 1) Find frames hvor bolden er tæt på målet
# ------------------------------------------------------------

goal_candidates <- ball %>%
  filter(abs(ball_x) > 50) %>%   # tæt på mållinje
  arrange(frame)

head(goal_candidates, 20)
ggplot(ball, aes(frame, ball_x)) +
  geom_line(color = "black") +
  geom_hline(yintercept = c(-52.5, 52.5), linetype = "dashed", color = "red") +
  ggtitle("Boldens x-position over tid (mål ~ ±52.5)")

goal_candidates <- goal_candidates %>%
  mutate(diff = frame - lag(frame, default = first(frame)),
         new_seq = diff > 50) %>%
  mutate(seq_id = cumsum(new_seq))

goal_sequences <- goal_candidates %>%
  group_by(seq_id) %>%
  summarise(
    start_frame = min(frame),
    end_frame = max(frame),
    n = n()
  )

goal_sequences
goal_frames <- goal_sequences %>%
  mutate(mid_frame = round((start_frame + end_frame)/2))

goal_frames
library(dplyr)

fps <- 25  # fra meta

goal_sequences2 <- goal_candidates %>%
  mutate(
    diff = frame - lag(frame, default = first(frame)),
    new_seq = diff > 50,
    seq_id = cumsum(new_seq)
  ) %>%
  group_by(seq_id) %>%
  summarise(
    start_frame = min(frame),
    end_frame = max(frame),
    mean_ball_x = mean(ball_x, na.rm = TRUE),
    mean_ball_y = mean(ball_y, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    mid_frame = round((start_frame + end_frame) / 2),
    approx_seconds = mid_frame / fps,
    approx_minute = floor(approx_seconds / 60),
    approx_second = round(approx_seconds %% 60)
  ) %>%
  filter(abs(mean_ball_y) <= 3.66) %>%   # inden for målets bredde
  arrange(mid_frame)

print(goal_sequences2, n = Inf)

####### Nu har vi fundet de præcise frames for vores to andre mål
goal1_frame <- 130412   # 1-1
goal2_frame <- 134295   # 2-1

library(dplyr)
library(ggplot2)
library(deldir)

plot_delaunay_match <- function(frame_id, title_text, subtitle_text = NULL) {
  
  players_frame <- players %>%
    filter(frame == frame_id) %>%
    mutate(
      team_label = ifelse(team == "home", home_team_name, away_team_name)
    )
  
  ball_frame <- ball %>%
    filter(frame == frame_id)
  
  tri <- deldir(players_frame$x, players_frame$y)
  tri_df <- tri$delsgs
  
  ggplot() +
    # bane
    geom_rect(
      aes(xmin = -52.5, xmax = 52.5, ymin = -34, ymax = 34),
      fill = "#1f7a3d", color = NA
    ) +
    
    # banestreger
    geom_rect(
      aes(xmin = -52.5, xmax = 52.5, ymin = -34, ymax = 34),
      fill = NA, color = "white", linewidth = 1
    ) +
    
    geom_segment(aes(x = 0, xend = 0, y = -34, yend = 34),
                 color = "white", linewidth = 1) +
    
    # straffesparksfelter
    geom_rect(aes(xmin = -52.5, xmax = -36, ymin = -20.16, ymax = 20.16),
              fill = NA, color = "white") +
    geom_rect(aes(xmin = 36, xmax = 52.5, ymin = -20.16, ymax = 20.16),
              fill = NA, color = "white") +
    
    # målfelter
    geom_rect(aes(xmin = -52.5, xmax = -47, ymin = -9.16, ymax = 9.16),
              fill = NA, color = "white") +
    geom_rect(aes(xmin = 47, xmax = 52.5, ymin = -9.16, ymax = 9.16),
              fill = NA, color = "white") +
    
    # midtercirkel
    annotate("path",
             x = 9.15 * cos(seq(0, 2*pi, length.out = 200)),
             y = 9.15 * sin(seq(0, 2*pi, length.out = 200)),
             color = "white") +
    
    # Delaunay
    geom_segment(
      data = tri_df,
      aes(x = x1, y = y1, xend = x2, yend = y2),
      color = "black",
      linewidth = 0.6
    ) +
    
    # spillere
    geom_point(
      data = players_frame,
      aes(x = x, y = y, fill = team_label),
      shape = 21,
      color = "white",
      size = 5
    ) +
    
    geom_text(
      data = players_frame,
      aes(x = x, y = y, label = number),
      color = "white",
      size = 3,
      fontface = "bold"
    ) +
    
    # bold
    geom_point(
      data = ball_frame,
      aes(x = ball_x, y = ball_y),
      fill = "white",
      color = "black",
      shape = 21,
      size = 3
    ) +
    
    scale_fill_manual(values = c("OB" = "#e45756", "VB" = "#17becf")) +
    
    coord_fixed() +
    theme_void() +
    labs(title = title_text, subtitle = subtitle_text)
}

# ------------------------------------------------------------
# PLOTS
# ------------------------------------------------------------

plot1 <- plot_delaunay_match(
  goal1_frame,
  "Delaunay – mål til 2-1",
  "Ca. 85:53"
)

plot2 <- plot_delaunay_match(
  goal2_frame,
  "Delaunay – mål til 2-1",
  "Ca. 89:32"
)

plot1
plot2

