library(rsconnect)

app_dir <- getwd()

app_files <- c(
  "app.R",
  "shots_superliga_2425.rds",
  "shots_superliga_2526.rds",
  "xp_vs_real_own_2425.rds",
  "xp_vs_real_22_own_2425.rds",
  "xp_vs_real_wyscout_2425.rds",
  "xp_vs_real_22_wyscout_2425.rds",
  "xp_model_compare_2425.rds",
  "xp_vs_real_2526_18.rds",
  "xp_projection_2526_22.rds",
  "result_mesterskab_2526.rds",
  "result_kval_2526.rds",
  "glm_2526_unweighted.rds",
  "glm_model_2526_compatible.rds",
  "model_metrics.rds",
  "roc_data.rds",
  "coef_data.rds",
  "cm_glm_unweighted.rds",
  "brondby logo.png",
  "women_events.rds"
)

missing_files <- app_files[!file.exists(file.path(app_dir, app_files))]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Disse filer mangler i app-mappen:\n- ",
      paste(missing_files, collapse = "\n- ")
    )
  )
}

rsconnect::deployApp(
  appDir = app_dir,
  appFiles = app_files,
  appName = "xG_xP_visualisering",
  appTitle = "xG_xP_visualisering",
  launch.browser = TRUE
)
