# =========================================================
# Freeze-frame analyse: skytte vs. bedre placeret medspiller
# =========================================================
# Inden man kører det her, så er der en dataframe/funktion der hedder ff_men_1
# Den bliver først kører igennem i "Opg. 5.2 Freeze Frames Cases. 4 styks.R"
source("util.R")

library(ggplot2)
library(dplyr)

# =========================================================
# 1. Udtræk af skuddata
# =========================================================

ShotsMen <- EventsMen %>%
  filter(type.name == "Shot")

ShotsWomen <- EventsWomen %>%
  filter(type.name == "Shot")

names(ShotsMen)
names(ShotsWomen)

# Eksempel på første freeze frame, der ikke er NULL
ShotsMen$shot.freeze_frame[[which(!sapply(ShotsMen$shot.freeze_frame, is.null))[1]]]
ShotsWomen$shot.freeze_frame[[which(!sapply(ShotsWomen$shot.freeze_frame, is.null))[1]]]


# =========================================================
# 2. Klargøring af freeze-frame data
# =========================================================

str(ff_men_1$location)

ff_men_1$x <- sapply(ff_men_1$location, `[`, 1)
ff_men_1$y <- sapply(ff_men_1$location, `[`, 2)

ff_men_1[, c("player.name", "teammate", "x", "y")]
names(ff_men_1)
ff_men_1[, c("player", "teammate", "x", "y")]

ShotsMen$location[[which(!sapply(ShotsMen$shot.freeze_frame, is.null))[1]]]


# =========================================================
# 3. Skyttens og målets positioner
# =========================================================

# Skyttens position
shooter_x <- ShotsMen$location[[which(!sapply(ShotsMen$shot.freeze_frame, is.null))[1]]][1]
shooter_y <- ShotsMen$location[[which(!sapply(ShotsMen$shot.freeze_frame, is.null))[1]]][2]

# Stolpernes positioner
left_post <- c(120, 36)
right_post <- c(120, 44)


# =========================================================
# 4. Funktion: ligger et punkt inde i trekanten?
# =========================================================

point_in_triangle <- function(px, py, ax, ay, bx, by, cx, cy) {
  denominator <- ((by - cy) * (ax - cx) + (cx - bx) * (ay - cy))
  
  a <- ((by - cy) * (px - cx) + (cx - bx) * (py - cy)) / denominator
  b <- ((cy - ay) * (px - cx) + (ax - cx) * (py - cy)) / denominator
  c <- 1 - a - b
  
  a >= 0 & b >= 0 & c >= 0
}


# =========================================================
# 5. Test af trekantfunktionen
# =========================================================

point_in_triangle(
  px = ff_men_1$x[3],
  py = ff_men_1$y[3],
  ax = shooter_x,
  ay = shooter_y,
  bx = left_post[1],
  by = left_post[2],
  cx = right_post[1],
  cy = right_post[2]
)

point_in_triangle(
  px = ff_men_1$x[10],
  py = ff_men_1$y[10],
  ax = shooter_x,
  ay = shooter_y,
  bx = left_post[1],
  by = left_post[2],
  cx = right_post[1],
  cy = right_post[2]
)


# =========================================================
# 6. Markering af spillere i skyttens trekant
# =========================================================

ff_men_1$in_triangle <- mapply(
  point_in_triangle,
  px = ff_men_1$x,
  py = ff_men_1$y,
  MoreArgs = list(
    ax = shooter_x,
    ay = shooter_y,
    bx = left_post[1],
    by = left_post[2],
    cx = right_post[1],
    cy = right_post[2]
  )
)

ff_men_1[, c("player", "teammate", "x", "y", "in_triangle")]

sum(ff_men_1$teammate == FALSE & ff_men_1$in_triangle == TRUE)


# =========================================================
# 7. Markering af spillere i medspillerens trekant
# =========================================================

mate_x <- ff_men_1$x[1]
mate_y <- ff_men_1$y[1]

ff_men_1$in_triangle_mate <- mapply(
  point_in_triangle,
  px = ff_men_1$x,
  py = ff_men_1$y,
  MoreArgs = list(
    ax = mate_x,
    ay = mate_y,
    bx = left_post[1],
    by = left_post[2],
    cx = right_post[1],
    cy = right_post[2]
  )
)

sum(ff_men_1$teammate == FALSE & ff_men_1$in_triangle_mate == TRUE)


# =========================================================
# 8. Sammenligning af antal blokeringer
# =========================================================

shooter_blockers <- sum(ff_men_1$teammate == FALSE & ff_men_1$in_triangle == TRUE)
shooter_blockers <- sum(ff_men_1$teammate == FALSE & ff_men_1$in_triangle == TRUE)

mate_blockers <- sum(ff_men_1$teammate == FALSE & ff_men_1$in_triangle_mate == TRUE)

mate_blockers < shooter_blockers


# =========================================================
# 9. Labels til visualisering
# =========================================================

ff_men_1$label <- ff_men_1$player$name

shooter_name <- ShotsMen$player.name[which(!sapply(ShotsMen$shot.freeze_frame, is.null))[1]]

ff_men_1$label <- ff_men_1$player$name
ff_men_1$label_clean <- ifelse(
  ff_men_1$player$name == "İlkay Gündoğan",
  "",
  ff_men_1$label
)


# =========================================================
# 10. Plot af freeze frame
# =========================================================

ggplot() +
  geom_rect(
    aes(xmin = 0, xmax = 120, ymin = 0, ymax = 80),
    fill = "#2e8b57",
    color = "white",
    linewidth = 0.8
  ) +
  geom_rect(
    aes(xmin = 102, xmax = 120, ymin = 18, ymax = 62),
    fill = NA,
    color = "white",
    linewidth = 0.8
  ) +
  geom_rect(
    aes(xmin = 114, xmax = 120, ymin = 30, ymax = 50),
    fill = NA,
    color = "white",
    linewidth = 0.8
  ) +
  geom_segment(
    aes(x = 120, y = 36, xend = 122, yend = 36),
    color = "white",
    linewidth = 1
  ) +
  geom_segment(
    aes(x = 120, y = 44, xend = 122, yend = 44),
    color = "white",
    linewidth = 1
  ) +
  geom_segment(
    aes(x = 122, y = 36, xend = 122, yend = 44),
    color = "white",
    linewidth = 1
  ) +
  geom_point(
    aes(x = 108, y = 40),
    color = "white",
    size = 1.5
  ) +
  geom_polygon(
    data = data.frame(
      x = c(shooter_x, left_post[1], right_post[1]),
      y = c(shooter_y, left_post[2], right_post[2])
    ),
    aes(x = x, y = y),
    fill = "red",
    alpha = 0.18
  ) +
  geom_polygon(
    data = data.frame(
      x = c(mate_x, left_post[1], right_post[1]),
      y = c(mate_y, left_post[2], right_post[2])
    ),
    aes(x = x, y = y),
    fill = "yellow",
    alpha = 0.18
  ) +
  geom_point(
    data = ff_men_1,
    aes(x = x, y = y, color = teammate),
    size = 3
  ) +
  geom_point(
    aes(x = shooter_x, y = shooter_y),
    color = "red",
    size = 4
  ) +
  geom_point(
    aes(x = mate_x, y = mate_y),
    color = "yellow",
    size = 4
  ) +
  geom_text(
    data = ff_men_1,
    aes(x = x, y = y, label = label_clean),
    color = "black",
    size = 3,
    vjust = -0.8
  ) +
  annotate(
    "label",
    x = shooter_x,
    y = shooter_y - 2,
    label = shooter_name,
    fill = "white",
    color = "red",
    size = 4,
    fontface = "bold",
    label.size = 0.3
  ) +
  annotate(
    "label",
    x = mate_x,
    y = mate_y + 2,
    label = "İlkay Gündoğan",
    fill = "white",
    color = "black",
    size = 4,
    fontface = "bold",
    label.size = 0.3
  ) +
  coord_fixed(
    xlim = c(80, 122.5),
    ylim = c(18, 65),
    expand = FALSE
  ) +
  scale_color_manual(
    values = c("FALSE" = "blue", "TRUE" = "orange")
  ) +
  labs(
    title = "Freeze-frame: skytte vs. bedre placeret medspiller",
    subtitle = paste0(
      shooter_name,
      " skyder, men Gündoğan har færre modspillere i sin trekant"
    ),
    color = "Medspiller"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#2e8b57", color = NA),
    panel.background = element_rect(fill = "#2e8b57", color = NA),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, color = "black"),
    plot.subtitle = element_text(size = 11, color = "black")
  )