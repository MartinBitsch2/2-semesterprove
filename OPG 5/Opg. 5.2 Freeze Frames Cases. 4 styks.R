# =========================================================
# Case-visualiseringer: mænd og kvinder
# TRUE-case og FALSE-case
# =========================================================

source("util.R")

library(ggplot2)
library(dplyr)

# =========================================================
# 1. Udvælg cases for mænd og kvinder
# =========================================================

# --- Mænd: case med bedre afleveringsmulighed ---
men_case_true <- men_results_realistic %>%
  filter(better_placed_teammate_exists == TRUE) %>%
  arrange(best_teammate_blockers) %>%
  slice(1)

men_case_true

# --- Mænd: case uden bedre afleveringsmulighed ---
men_case_false <- men_results_realistic %>%
  filter(better_placed_teammate_exists == FALSE) %>%
  arrange(shooter_blockers) %>%
  slice(1)

men_case_false

# --- Kvinder: case med bedre afleveringsmulighed ---
women_case_true <- women_results_realistic %>%
  filter(better_placed_teammate_exists == TRUE) %>%
  arrange(best_teammate_blockers) %>%
  slice(1)

women_case_true

# --- Kvinder: case uden bedre afleveringsmulighed ---
women_case_false <- women_results_realistic %>%
  filter(better_placed_teammate_exists == FALSE) %>%
  arrange(shooter_blockers) %>%
  slice(1)

women_case_false


# =========================================================
# 2. CASE 1: Mænd (TRUE-case)
# Gündoğan -> Wirtz
# =========================================================

# Hent det præcise skud
shot_men_1 <- ShotsMen %>%
  filter(id == men_case_true$shot_id)

# Hent freeze frame
ff_men_1 <- shot_men_1$shot.freeze_frame[[1]]

# Split koordinater
ff_men_1$x <- sapply(ff_men_1$location, `[`, 1)
ff_men_1$y <- sapply(ff_men_1$location, `[`, 2)

# Hent skytten og bedste medspiller
shooter_x <- shot_men_1$location[[1]][1]
shooter_y <- shot_men_1$location[[1]][2]

best_x <- men_case_true$best_teammate_x
best_y <- men_case_true$best_teammate_y

# Hent skuddet igen og byg et rent dataframe
shot_men_1 <- ShotsMen %>%
  filter(id == men_case_true$shot_id)

ff_raw <- shot_men_1$shot.freeze_frame[[1]]

ff_men_1 <- data.frame(
  teammate = ff_raw$teammate,
  stringsAsFactors = FALSE
)

# Koordinater
ff_men_1$x <- sapply(ff_raw$location, `[`, 1)
ff_men_1$y <- sapply(ff_raw$location, `[`, 2)

# Spillernavn
if ("player.name" %in% names(ff_raw)) {
  ff_men_1$player_name <- ff_raw$player.name
} else if ("player" %in% names(ff_raw) && is.data.frame(ff_raw$player)) {
  ff_men_1$player_name <- ff_raw$player$name
} else {
  ff_men_1$player_name <- NA_character_
}

# Positionsnavn
if ("position.name" %in% names(ff_raw)) {
  ff_men_1$position_name <- ff_raw$position.name
} else if ("position" %in% names(ff_raw) && is.data.frame(ff_raw$position)) {
  ff_men_1$position_name <- ff_raw$position$name
} else {
  ff_men_1$position_name <- NA_character_
}

# Variabler til plottet
shooter_x <- shot_men_1$location[[1]][1]
shooter_y <- shot_men_1$location[[1]][2]
shooter_name <- shot_men_1$player.name[1]

best_x <- men_case_true$best_teammate_x
best_y <- men_case_true$best_teammate_y
best_name <- men_case_true$best_teammate_name[1]

left_post <- c(120, 36)
right_post <- c(120, 44)
goal_center <- c(120, 40)

ff_men_1$label <- ff_men_1$player_name
ff_men_1$label_clean <- ifelse(ff_men_1$player_name == best_name, "", ff_men_1$label)
ff_men_1$is_goalkeeper <- grepl("Goalkeeper", ff_men_1$position_name)

# Labels til øvrige medspillere
label_df <- ff_men_1 %>%
  filter(teammate == TRUE, player_name != best_name, !is_goalkeeper) %>%
  mutate(label_y = y + 1.2)

# Labels til nøglespillere
key_labels <- data.frame(
  x = c(shooter_x, best_x),
  y = c(shooter_y, best_y),
  label_x = c(shooter_x - 2.6, best_x - 1.8),
  label_y = c(shooter_y + 0.8, best_y + 3.0),
  label = c(shooter_name, best_name),
  stringsAsFactors = FALSE
)

# Banemarkeringer
penalty_box <- data.frame(xmin = 102, xmax = 120, ymin = 18, ymax = 62)
six_yard_box <- data.frame(xmin = 114, xmax = 120, ymin = 30, ymax = 50)

# Manuel legend
legend_x <- 120.5
legend_y <- 58

# Plot
ggplot() +
  geom_rect(
    aes(xmin = 0, xmax = 120, ymin = 0, ymax = 80),
    fill = "#2E8B57",
    color = NA
  ) +
  geom_rect(
    data = penalty_box,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA,
    color = "white",
    linewidth = 0.9
  ) +
  geom_rect(
    data = six_yard_box,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA,
    color = "white",
    linewidth = 0.9
  ) +
  geom_segment(
    aes(x = 120, y = 36, xend = 122, yend = 36),
    color = "white",
    linewidth = 1.1
  ) +
  geom_segment(
    aes(x = 120, y = 44, xend = 122, yend = 44),
    color = "white",
    linewidth = 1.1
  ) +
  geom_segment(
    aes(x = 122, y = 36, xend = 122, yend = 44),
    color = "white",
    linewidth = 1.1
  ) +
  geom_point(
    aes(x = 108, y = 40),
    color = "white",
    size = 1.5
  ) +
  
  # Skyttens trekant
  geom_polygon(
    data = data.frame(
      x = c(shooter_x, left_post[1], right_post[1]),
      y = c(shooter_y, left_post[2], right_post[2])
    ),
    aes(x = x, y = y),
    fill = "#17A2A6",
    alpha = 0.24,
    color = "#17A2A6",
    linewidth = 0.8
  ) +
  
  # Bedre muligheds trekant
  geom_polygon(
    data = data.frame(
      x = c(best_x, left_post[1], right_post[1]),
      y = c(best_y, left_post[2], right_post[2])
    ),
    aes(x = x, y = y),
    fill = "#EB6A5B",
    alpha = 0.24,
    color = "#EB6A5B",
    linewidth = 0.8
  ) +
  
  # Linjer til mål
  geom_segment(
    aes(x = shooter_x, y = shooter_y, xend = goal_center[1], yend = goal_center[2]),
    color = "#17A2A6",
    linewidth = 2.0,
    alpha = 0.95
  ) +
  geom_segment(
    aes(x = best_x, y = best_y, xend = goal_center[1], yend = goal_center[2]),
    color = "#EB6A5B",
    linewidth = 2.0,
    alpha = 0.95
  ) +
  
  # Alle ikke-målmænd
  geom_point(
    data = ff_men_1 %>% filter(!is_goalkeeper),
    aes(x = x, y = y, fill = teammate),
    shape = 21,
    color = "white",
    size = 4,
    stroke = 0.8,
    show.legend = FALSE
  ) +
  
  # Målmand
  geom_point(
    data = ff_men_1 %>% filter(is_goalkeeper),
    aes(x = x, y = y),
    shape = 24,
    fill = "gold",
    color = "black",
    size = 5,
    stroke = 1
  ) +
  geom_text(
    data = ff_men_1 %>% filter(is_goalkeeper),
    aes(x = x, y = y + 1.3, label = player_name),
    color = "black",
    size = 3
  ) +
  
  # Highlight skytte
  geom_point(
    aes(x = shooter_x, y = shooter_y),
    shape = 21,
    fill = "#17A2A6",
    color = "black",
    size = 6,
    stroke = 1.5
  ) +
  
  # Highlight bedre mulighed
  geom_point(
    aes(x = best_x, y = best_y),
    shape = 21,
    fill = "#6A4C93",
    color = "black",
    size = 6,
    stroke = 1.5
  ) +
  
  # Labels til øvrige medspillere
  geom_text(
    data = label_df,
    aes(x = x, y = label_y, label = label_clean),
    color = "black",
    size = 3,
    alpha = 0.85
  ) +
  
  # Labels til nøglespillere
  geom_label(
    data = key_labels,
    aes(x = label_x, y = label_y, label = label),
    fill = "white",
    color = "black",
    fontface = "bold",
    size = 3.8,
    linewidth = 0.25
  ) +
  
  # Manuel forklaring til højre
  geom_point(
    aes(x = legend_x, y = legend_y),
    shape = 21,
    fill = "#17A2A6",
    color = "black",
    size = 4
  ) +
  geom_text(
    aes(x = legend_x + 0.6, y = legend_y),
    label = "Skytte",
    hjust = 0,
    size = 3.8
  ) +
  geom_point(
    aes(x = legend_x, y = legend_y - 3),
    shape = 21,
    fill = "#6A4C93",
    color = "black",
    size = 4
  ) +
  geom_text(
    aes(x = legend_x + 0.6, y = legend_y - 3),
    label = "Bedre afleveringsmulighed",
    hjust = 0,
    size = 3.8
  ) +
  geom_point(
    aes(x = legend_x, y = legend_y - 6),
    shape = 24,
    fill = "gold",
    color = "black",
    size = 4
  ) +
  geom_text(
    aes(x = legend_x + 0.6, y = legend_y - 6),
    label = "Modstanderens målmand",
    hjust = 0,
    size = 3.8
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#17A2A6",
      "FALSE" = "#EB6A5B"
    )
  ) +
  coord_fixed(
    xlim = c(95, 130),
    ylim = c(20, 62),
    expand = FALSE
  ) +
  labs(
    title = "Bedre mulighed overset: Gündoğan vs. Wirtz",
    subtitle = paste0(
      best_name, " har ", men_case_true$best_teammate_blockers,
      " modspillere i sin afslutningszone mod ",
      shooter_name, "s ", men_case_true$shooter_blockers,
      ". Modellen indikerer en klar afleveringsmulighed."
    ),
    caption = "Blå = skudmulighed. Lilla = bedre afleveringsmulighed. Koralfarvet trekant = afslutningsvinkel."
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#2E8B57", color = NA),
    plot.title = element_text(face = "bold", size = 18, color = "black"),
    plot.subtitle = element_text(size = 11, color = "black", margin = margin(b = 10)),
    plot.caption = element_text(size = 10, color = "black", hjust = 0),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15)
  )

# Tjek udfald
shot_men_1$shot.outcome.name
# Blocked


# =========================================================
# 3. CASE 2: Mænd (FALSE-case)
# Antoine Griezmann
# =========================================================

shot_men_2 <- ShotsMen %>%
  filter(id == men_case_false$shot_id)

ff_men_2 <- shot_men_2$shot.freeze_frame[[1]]
ff_men_2$x <- sapply(ff_men_2$location, `[`, 1)
ff_men_2$y <- sapply(ff_men_2$location, `[`, 2)

shooter_x <- shot_men_2$location[[1]][1]
shooter_y <- shot_men_2$location[[1]][2]
shooter_name <- shot_men_2$player.name[1]

left_post <- c(120, 36)
right_post <- c(120, 44)
goal_center <- c(120, 40)

ff_men_2$label <- ff_men_2$player$name
ff_men_2$is_goalkeeper <- grepl("Goalkeeper", ff_men_2$position$name)

label_df <- ff_men_2 %>%
  filter(teammate == TRUE, !is_goalkeeper) %>%
  mutate(label_y = y + 1.2)

key_labels <- data.frame(
  x = shooter_x,
  y = shooter_y,
  label_x = shooter_x - 2.4,
  label_y = shooter_y + 1.0,
  label = shooter_name
)

penalty_box <- data.frame(xmin = 102, xmax = 120, ymin = 18, ymax = 62)
six_yard_box <- data.frame(xmin = 114, xmax = 120, ymin = 30, ymax = 50)

legend_x <- 120.5
legend_y <- 58

ggplot() +
  geom_rect(
    aes(xmin = 0, xmax = 120, ymin = 0, ymax = 80),
    fill = "#2E8B57",
    color = NA
  ) +
  geom_rect(
    data = penalty_box,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA,
    color = "white",
    linewidth = 0.9
  ) +
  geom_rect(
    data = six_yard_box,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA,
    color = "white",
    linewidth = 0.9
  ) +
  geom_segment(
    aes(x = 120, y = 36, xend = 122, yend = 36),
    color = "white",
    linewidth = 1.1
  ) +
  geom_segment(
    aes(x = 120, y = 44, xend = 122, yend = 44),
    color = "white",
    linewidth = 1.1
  ) +
  geom_segment(
    aes(x = 122, y = 36, xend = 122, yend = 44),
    color = "white",
    linewidth = 1.1
  ) +
  geom_point(
    aes(x = 108, y = 40),
    color = "white",
    size = 1.5
  ) +
  
  # Skyttens trekant
  geom_polygon(
    data = data.frame(
      x = c(shooter_x, left_post[1], right_post[1]),
      y = c(shooter_y, left_post[2], right_post[2])
    ),
    aes(x = x, y = y),
    fill = "#17A2A6",
    alpha = 0.24,
    color = "#17A2A6",
    linewidth = 0.8
  ) +
  
  # Skudlinje
  geom_segment(
    aes(x = shooter_x, y = shooter_y, xend = goal_center[1], yend = goal_center[2]),
    color = "#17A2A6",
    linewidth = 2.0,
    alpha = 0.95
  ) +
  
  # Ikke-målmænd
  geom_point(
    data = ff_men_2 %>% filter(!is_goalkeeper),
    aes(x = x, y = y, fill = teammate),
    shape = 21,
    color = "white",
    size = 4,
    stroke = 0.8,
    show.legend = FALSE
  ) +
  
  # Målmand
  geom_point(
    data = ff_men_2 %>% filter(is_goalkeeper),
    aes(x = x, y = y),
    shape = 24,
    fill = "gold",
    color = "black",
    size = 5,
    stroke = 1
  ) +
  geom_text(
    data = ff_men_2 %>% filter(is_goalkeeper),
    aes(x = x, y = y + 1.3, label = player$name),
    color = "black",
    size = 3
  ) +
  
  # Highlight skytte
  geom_point(
    aes(x = shooter_x, y = shooter_y),
    shape = 21,
    fill = "#17A2A6",
    color = "black",
    size = 6,
    stroke = 1.5
  ) +
  
  # Labels til øvrige medspillere
  geom_text(
    data = label_df,
    aes(x = x, y = label_y, label = label),
    color = "black",
    size = 3,
    alpha = 0.85
  ) +
  
  # Label til skytten
  geom_label(
    data = key_labels,
    aes(x = label_x, y = label_y, label = label),
    fill = "white",
    color = "black",
    fontface = "bold",
    size = 3.8,
    linewidth = 0.25
  ) +
  
  # Manuel forklaring til højre
  geom_point(
    aes(x = legend_x, y = legend_y),
    shape = 21,
    fill = "#17A2A6",
    color = "black",
    size = 4
  ) +
  geom_text(
    aes(x = legend_x + 1.0, y = legend_y),
    label = "Skytte",
    hjust = 0,
    size = 3.8
  ) +
  geom_point(
    aes(x = legend_x, y = legend_y - 3),
    shape = 24,
    fill = "gold",
    color = "black",
    size = 4
  ) +
  geom_text(
    aes(x = legend_x + 1.0, y = legend_y - 3),
    label = "Modstanderens målmand",
    hjust = 0,
    size = 3.8
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#17A2A6",
      "FALSE" = "#EB6A5B"
    )
  ) +
  coord_fixed(
    xlim = c(95, 130),
    ylim = c(20, 62),
    expand = FALSE
  ) +
  labs(
    title = "Rigtigt valg: Griezmann afslutter selv",
    subtitle = "Ingen medspillere er bedre placeret ifølge modellen",
    caption = "Blå = faktisk skudmulighed."
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#2E8B57", color = NA),
    plot.title = element_text(face = "bold", size = 18, color = "black"),
    plot.subtitle = element_text(size = 11, color = "black", margin = margin(b = 10)),
    plot.caption = element_text(size = 10, color = "black", hjust = 0),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15)
  )


# =========================================================
# 4. CASE 3: Kvinder (TRUE-case)
# Lauren Hemp -> Bethany Mead
# =========================================================

shot_women_1 <- ShotsWomen[women_case_true$shot_id, ]

ff_women_1 <- shot_women_1$shot.freeze_frame[[1]]
ff_women_1$x <- sapply(ff_women_1$location, `[`, 1)
ff_women_1$y <- sapply(ff_women_1$location, `[`, 2)

shooter_x <- shot_women_1$location[[1]][1]
shooter_y <- shot_women_1$location[[1]][2]
shooter_name <- shot_women_1$player.name[1]

best_x <- women_case_true$best_teammate_x
best_y <- women_case_true$best_teammate_y
best_name <- women_case_true$best_teammate_name[1]

left_post <- c(120, 36)
right_post <- c(120, 44)
goal_center <- c(120, 40)

ff_women_1$label <- ff_women_1$player$name
ff_women_1$label_clean <- ifelse(ff_women_1$player$name == best_name, "", ff_women_1$label)
ff_women_1$is_goalkeeper <- grepl("Goalkeeper", ff_women_1$position$name)

label_df <- ff_women_1 %>%
  filter(teammate == TRUE, player$name != best_name) %>%
  mutate(label_y = y + 1.2)

key_labels <- data.frame(
  x = c(shooter_x, best_x),
  y = c(shooter_y, best_y),
  label_x = c(shooter_x - 2.6, best_x - 1.8),
  label_y = c(shooter_y + 0.8, best_y + 3.0),
  label = c(shooter_name, best_name),
  type = c("Skytte", "Bedre mulighed")
)

penalty_box <- data.frame(xmin = 102, xmax = 120, ymin = 18, ymax = 62)
six_yard_box <- data.frame(xmin = 114, xmax = 120, ymin = 30, ymax = 50)

legend_x <- 120.5
legend_y <- 58

ggplot() +
  geom_rect(
    aes(xmin = 0, xmax = 120, ymin = 0, ymax = 80),
    fill = "#2E8B57",
    color = NA
  ) +
  geom_rect(
    data = penalty_box,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA,
    color = "white",
    linewidth = 0.9
  ) +
  geom_rect(
    data = six_yard_box,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA,
    color = "white",
    linewidth = 0.9
  ) +
  geom_segment(
    aes(x = 120, y = 36, xend = 122, yend = 36),
    color = "white",
    linewidth = 1.1
  ) +
  geom_segment(
    aes(x = 120, y = 44, xend = 122, yend = 44),
    color = "white",
    linewidth = 1.1
  ) +
  geom_segment(
    aes(x = 122, y = 36, xend = 122, yend = 44),
    color = "white",
    linewidth = 1.1
  ) +
  geom_point(
    aes(x = 108, y = 40),
    color = "white",
    size = 1.5
  ) +
  
  # Trekanter
  geom_polygon(
    data = data.frame(
      x = c(shooter_x, left_post[1], right_post[1]),
      y = c(shooter_y, left_post[2], right_post[2])
    ),
    aes(x = x, y = y),
    fill = "#17A2A6",
    alpha = 0.24,
    color = "#17A2A6",
    linewidth = 0.8
  ) +
  geom_polygon(
    data = data.frame(
      x = c(best_x, left_post[1], right_post[1]),
      y = c(best_y, left_post[2], right_post[2])
    ),
    aes(x = x, y = y),
    fill = "#EB6A5B",
    alpha = 0.24,
    color = "#EB6A5B",
    linewidth = 0.8
  ) +
  
  # Linjer til mål
  geom_segment(
    aes(x = shooter_x, y = shooter_y, xend = goal_center[1], yend = goal_center[2]),
    color = "#17A2A6",
    linewidth = 2.0,
    alpha = 0.95
  ) +
  geom_segment(
    aes(x = best_x, y = best_y, xend = goal_center[1], yend = goal_center[2]),
    color = "#EB6A5B",
    linewidth = 2.0,
    alpha = 0.95
  ) +
  
  # Ikke-målmænd
  geom_point(
    data = ff_women_1 %>% filter(!is_goalkeeper),
    aes(x = x, y = y, fill = teammate),
    shape = 21,
    color = "white",
    size = 4,
    stroke = 0.8,
    show.legend = FALSE
  ) +
  
  # Målmand
  geom_point(
    data = ff_women_1 %>% filter(is_goalkeeper),
    aes(x = x, y = y),
    shape = 24,
    fill = "gold",
    color = "black",
    size = 5,
    stroke = 1
  ) +
  geom_text(
    data = ff_women_1 %>% filter(is_goalkeeper),
    aes(x = x, y = y + 1.3, label = player$name),
    color = "black",
    size = 3
  ) +
  
  # Highlight skytte
  geom_point(
    aes(x = shooter_x, y = shooter_y),
    shape = 21,
    fill = "#17A2A6",
    color = "black",
    size = 6,
    stroke = 1.5
  ) +
  
  # Highlight bedre mulighed
  geom_point(
    aes(x = best_x, y = best_y),
    shape = 21,
    fill = "#6A4C93",
    color = "black",
    size = 6,
    stroke = 1.5
  ) +
  
  # Labels til øvrige medspillere
  geom_text(
    data = label_df,
    aes(x = x, y = label_y, label = label_clean),
    color = "black",
    size = 3,
    alpha = 0.85
  ) +
  
  # Labels til nøglespillere
  geom_label(
    data = key_labels,
    aes(x = label_x, y = label_y, label = label),
    fill = "white",
    color = "black",
    fontface = "bold",
    size = 3.8,
    linewidth = 0.25
  ) +
  
  # Manuel forklaring til højre
  geom_point(
    aes(x = legend_x, y = legend_y),
    shape = 21,
    fill = "#17A2A6",
    color = "black",
    size = 4
  ) +
  geom_text(
    aes(x = legend_x + 0.6, y = legend_y),
    label = "Skytte",
    hjust = 0,
    size = 3.8
  ) +
  geom_point(
    aes(x = legend_x, y = legend_y - 3),
    shape = 21,
    fill = "#6A4C93",
    color = "black",
    size = 4
  ) +
  geom_text(
    aes(x = legend_x + 0.6, y = legend_y - 3),
    label = "Bedre afleveringsmulighed",
    hjust = 0,
    size = 3.8
  ) +
  geom_point(
    aes(x = legend_x, y = legend_y - 6),
    shape = 24,
    fill = "gold",
    color = "black",
    size = 4
  ) +
  geom_text(
    aes(x = legend_x + 0.6, y = legend_y - 6),
    label = "Modstanderens målmand",
    hjust = 0,
    size = 3.8
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#17A2A6",
      "FALSE" = "#EB6A5B"
    )
  ) +
  coord_fixed(
    xlim = c(95, 130),
    ylim = c(20, 62),
    expand = FALSE
  ) +
  labs(
    title = "Bedre mulighed overset: Hemp vs. Mead",
    subtitle = paste0(
      "Bethany Mead har ", women_case_true$best_teammate_blockers,
      " modspillere i sin afslutningszone mod Lauren Hemps ",
      women_case_true$shooter_blockers,
      ". Modellen indikerer en klar afleveringsmulighed."
    ),
    caption = "Blå = skudmulighed. Lilla = bedre afleveringsmulighed. Koralfarvet trekant = afslutningsvinkel."
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#2E8B57", color = NA),
    plot.title = element_text(face = "bold", size = 18, color = "black"),
    plot.subtitle = element_text(size = 11, color = "black", margin = margin(b = 10)),
    plot.caption = element_text(size = 10, color = "black", hjust = 0),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15)
  )


# =========================================================
# 5. CASE 4: Kvinder (FALSE-case)
# Bethany Mead
# =========================================================

shot_women_2 <- ShotsWomen[women_case_false$shot_id, ]

ff_raw <- shot_women_2$shot.freeze_frame[[1]]

ff_women_2 <- data.frame(
  teammate = ff_raw$teammate,
  stringsAsFactors = FALSE
)

# Koordinater
ff_women_2$x <- sapply(ff_raw$location, `[`, 1)
ff_women_2$y <- sapply(ff_raw$location, `[`, 2)

# Spillernavn
if ("player.name" %in% names(ff_raw)) {
  ff_women_2$player_name <- ff_raw$player.name
} else if ("player" %in% names(ff_raw) && is.data.frame(ff_raw$player)) {
  ff_women_2$player_name <- ff_raw$player$name
} else {
  ff_women_2$player_name <- NA_character_
}

# Positionsnavn
if ("position.name" %in% names(ff_raw)) {
  ff_women_2$position_name <- ff_raw$position.name
} else if ("position" %in% names(ff_raw) && is.data.frame(ff_raw$position)) {
  ff_women_2$position_name <- ff_raw$position$name
} else {
  ff_women_2$position_name <- NA_character_
}

# Variabler
shooter_x <- shot_women_2$location[[1]][1]
shooter_y <- shot_women_2$location[[1]][2]
shooter_name <- shot_women_2$player.name[1]

left_post <- c(120, 36)
right_post <- c(120, 44)
goal_center <- c(120, 40)

ff_women_2$label <- ff_women_2$player_name
ff_women_2$is_goalkeeper <- grepl("Goalkeeper", ff_women_2$position_name)

label_df <- ff_women_2 %>%
  filter(teammate == TRUE, !is_goalkeeper) %>%
  mutate(label_y = y + 1.2)

key_labels <- data.frame(
  x = shooter_x,
  y = shooter_y,
  label_x = shooter_x - 2.4,
  label_y = shooter_y + 1.0,
  label = shooter_name,
  stringsAsFactors = FALSE
)

penalty_box <- data.frame(xmin = 102, xmax = 120, ymin = 18, ymax = 62)
six_yard_box <- data.frame(xmin = 114, xmax = 120, ymin = 30, ymax = 50)

legend_x <- 120.5
legend_y <- 58

ggplot() +
  geom_rect(
    aes(xmin = 0, xmax = 120, ymin = 0, ymax = 80),
    fill = "#2E8B57",
    color = NA
  ) +
  geom_rect(
    data = penalty_box,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA,
    color = "white",
    linewidth = 0.9
  ) +
  geom_rect(
    data = six_yard_box,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA,
    color = "white",
    linewidth = 0.9
  ) +
  geom_segment(
    aes(x = 120, y = 36, xend = 122, yend = 36),
    color = "white",
    linewidth = 1.1
  ) +
  geom_segment(
    aes(x = 120, y = 44, xend = 122, yend = 44),
    color = "white",
    linewidth = 1.1
  ) +
  geom_segment(
    aes(x = 122, y = 36, xend = 122, yend = 44),
    color = "white",
    linewidth = 1.1
  ) +
  geom_point(
    aes(x = 108, y = 40),
    color = "white",
    size = 1.5
  ) +
  
  # Skyttens trekant
  geom_polygon(
    data = data.frame(
      x = c(shooter_x, left_post[1], right_post[1]),
      y = c(shooter_y, left_post[2], right_post[2])
    ),
    aes(x = x, y = y),
    fill = "#17A2A6",
    alpha = 0.24,
    color = "#17A2A6",
    linewidth = 0.8
  ) +
  
  # Skudlinje
  geom_segment(
    aes(x = shooter_x, y = shooter_y, xend = goal_center[1], yend = goal_center[2]),
    color = "#17A2A6",
    linewidth = 2.0,
    alpha = 0.95
  ) +
  
  # Alle ikke-målmænd
  geom_point(
    data = ff_women_2 %>% filter(!is_goalkeeper),
    aes(x = x, y = y, fill = teammate),
    shape = 21,
    color = "white",
    size = 4,
    stroke = 0.8,
    show.legend = FALSE
  ) +
  
  # Målmand
  geom_point(
    data = ff_women_2 %>% filter(is_goalkeeper),
    aes(x = x, y = y),
    shape = 24,
    fill = "gold",
    color = "black",
    size = 5,
    stroke = 1
  ) +
  geom_text(
    data = ff_women_2 %>% filter(is_goalkeeper),
    aes(x = x, y = y + 1.3, label = player_name),
    color = "black",
    size = 3
  ) +
  
  # Highlight skytte
  geom_point(
    aes(x = shooter_x, y = shooter_y),
    shape = 21,
    fill = "#17A2A6",
    color = "black",
    size = 6,
    stroke = 1.5
  ) +
  
  # Labels til øvrige medspillere
  geom_text(
    data = label_df,
    aes(x = x, y = label_y, label = label),
    color = "black",
    size = 3,
    alpha = 0.85
  ) +
  
  # Label til skytten
  geom_label(
    data = key_labels,
    aes(x = label_x, y = label_y, label = label),
    fill = "white",
    color = "black",
    fontface = "bold",
    size = 3.8,
    linewidth = 0.25
  ) +
  
  # Manuel forklaring til højre
  geom_point(
    aes(x = legend_x, y = legend_y),
    shape = 21,
    fill = "#17A2A6",
    color = "black",
    size = 4
  ) +
  geom_text(
    aes(x = legend_x + 0.6, y = legend_y),
    label = "Skytte",
    hjust = 0,
    size = 3.8
  ) +
  geom_point(
    aes(x = legend_x, y = legend_y - 3),
    shape = 24,
    fill = "gold",
    color = "black",
    size = 4
  ) +
  geom_text(
    aes(x = legend_x + 0.6, y = legend_y - 3),
    label = "Modstanderens målmand",
    hjust = 0,
    size = 3.8
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#17A2A6",
      "FALSE" = "#EB6A5B"
    )
  ) +
  coord_fixed(
    xlim = c(95, 130),
    ylim = c(20, 62),
    expand = FALSE
  ) +
  labs(
    title = "Rigtigt valg: Mead afslutter selv",
    subtitle = "Ingen medspillere er bedre placeret ifølge modellen",
    caption = "Blå = faktisk skudmulighed."
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#2E8B57", color = NA),
    plot.title = element_text(face = "bold", size = 18, color = "black"),
    plot.subtitle = element_text(size = 11, color = "black", margin = margin(b = 10)),
    plot.caption = element_text(size = 10, color = "black", hjust = 0),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15)
  )