# =========================================================
# Plots til opgave 5.1
# =========================================================

library(ggplot2)

# =========================================================
# 1. Afleveringer pr. angrebssekvens
# =========================================================

plot_data <- data.frame(
  gruppe = c("Kvinder", "Mænd"),
  afleveringer_per_angreb = c(5.08, 7.14)
)

ggplot(plot_data, aes(x = gruppe, y = afleveringer_per_angreb, fill = gruppe)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(
    aes(label = round(afleveringer_per_angreb, 2)),
    vjust = -0.5,
    size = 5
  ) +
  labs(
    title = "Mænd har længere angrebssekvenser end kvinder",
    subtitle = "Målt som gennemsnitligt antal afleveringer per angreb",
    x = NULL,
    y = "Afleveringer pr. angreb"
  ) +
  ylim(0, 8) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )


# =========================================================
# 2. Spilstop pr. kamp
# =========================================================

plot_data <- data.frame(
  kategori = c("Dommerstop", "Naturlige spilstop"),
  mænd = c(25.82 + 0.29, 33.00 + 15.67 + 9.96),
  kvinder = c(20.19 + 0.19, 48.10 + 16.97 + 10.55)
)

plot_data_long <- data.frame(
  kategori = rep(plot_data$kategori, each = 2),
  gruppe = rep(c("Mænd", "Kvinder"), times = nrow(plot_data)),
  værdi = c(rbind(plot_data$mænd, plot_data$kvinder))
)

ggplot(plot_data_long, aes(x = kategori, y = værdi, fill = gruppe)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(
    aes(label = round(værdi, 1)),
    position = position_dodge(width = 0.6),
    vjust = -0.4,
    size = 4.5
  ) +
  labs(
    title = "Kvinders kampe har flere naturlige spilstop, mens mænds har flere dommerstop",
    subtitle = "Dommerstop = frispark og offside. Naturlige spilstop = indkast, målspark og hjørnespark",
    x = NULL,
    y = "Antal per kamp",
    caption = "Kilde: StatsBomb eventdata, egen bearbejdning"
  ) +
  ylim(0, 80) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, hjust = 1)
  )


# =========================================================
# 3. Afleveringsprocent
# =========================================================

plot_data <- data.frame(
  gruppe = c("Kvinder", "Mænd"),
  pass_success = c(0.7626, 0.8420)
)

ggplot(plot_data, aes(x = gruppe, y = pass_success, fill = gruppe)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(round(pass_success * 100, 1), "%")),
    vjust = -0.5,
    size = 5
  ) +
  labs(
    title = "Mænd har højere pasningspræcision end kvinder",
    subtitle = "Andel succesfulde afleveringer",
    x = NULL,
    y = "Succesrate",
    caption = "Kilde: StatsBomb eventdata, egen bearbejdning"
  ) +
  ylim(0, 0.9) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, hjust = 1)
  )


# =========================================================
# 4. Skud, xG og conversion rate
# =========================================================

plot_data <- data.frame(
  gruppe = rep(c("Mænd", "Kvinder"), 3),
  metric = rep(c("Skud per kamp", "xG per skud", "Conversion rate"), each = 2),
  værdi = c(
    26.27, 28.42,   # Skud per kamp
    0.1042, 0.1062, # xG per skud
    0.0940, 0.1033  # Conversion rate
  )
)

ggplot(plot_data, aes(x = gruppe, y = værdi, fill = gruppe)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(
    aes(
      label = ifelse(
        metric == "Conversion rate",
        paste0(round(værdi * 100, 1), "%"),
        round(værdi, 2)
      )
    ),
    vjust = -0.5,
    size = 4.5
  ) +
  facet_wrap(~metric, scales = "free_y") +
  labs(
    title = "Kvinder skaber flere afslutninger og er mere effektive",
    subtitle = "Sammenligning af conversion rate, skud og xG",
    x = NULL,
    y = NULL,
    caption = "Kilde: StatsBomb eventdata, egen bearbejdning"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, hjust = 1)
  )


# =========================================================
# 5. Målvogterens redningsprocent
# =========================================================

plot_data <- data.frame(
  gruppe = c("Kvinder", "Mænd"),
  save_pct = c(0.6738, 0.7149)
)

ggplot(plot_data, aes(x = gruppe, y = save_pct, fill = gruppe)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(round(save_pct * 100, 1), "%")),
    vjust = -0.5,
    size = 5
  ) +
  labs(
    title = "Mænds målmænd redder en større andel af skuddene end kvinders",
    subtitle = "Redningsprocent målt som andel reddede skud på mål",
    x = NULL,
    y = "Redningsprocent",
    caption = "Kilde: StatsBomb eventdata, egen bearbejdning"
  ) +
  ylim(0, 0.8) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, hjust = 1)
  )