library(shiny)
library(dplyr)
library(ggplot2)
library(factoextra)
library(plotly)
library(purrr)
library(bslib)
library(stringr)
library(scales)
library(DT)

# --- LOAD DATA ---
load("app_data.RData")

available_team_ids <- unique(MATCHADVANCEDSTATS$TEAM_WYID)
filtered_teams <- teams %>% 
  filter(TEAM_WYID %in% available_team_ids) %>%
  arrange(TEAMNAME)

# --- VARIABLER ---
var_choices_stats <- setNames(clean_variables$original_variable_names, clean_variables$new_clean_names)
var_choices_stats <- var_choices_stats[5:length(var_choices_stats)] 
var_choices <- c("PCA (Dimension 1 / Alle variable)" = "PCA1", 
                 "PCA (Dimension 2 / Alle variable)" = "PCA2", 
                 var_choices_stats)

get_clean_label <- function(orig_name) {
  if (is.null(orig_name) || length(orig_name) == 0) return("")
  if (orig_name %in% c("PCA1", "PCA2")) return(orig_name)
  
  match_idx <- match(orig_name, clean_variables$original_variable_names)
  
  if (!is.na(match_idx)) {
    return(as.character(clean_variables$new_clean_names[match_idx]))
  } else {
    return(as.character(orig_name))
  }
}

# --- VARIABLER TIL AFLEVERINGS-FANEN ---
pass_var_choices <- c(
  "PCA (Dimension 1)" = "PCA1",
  "PCA (Dimension 2)" = "PCA2",
  "Gns. X-koordinat (meter)" = "gns_X_afleveringer",
  "Gns. Y-koordinat (meter)" = "gns_Y_afleveringer",
  "Antal afleveringer" = "antal_afleveringer_total",
  "Antal succesfulde" = "antal_succesfulde_afleveringer",
  "Succes-procent (%)" = "succes_procent_afleveringer",
  "Gns. længde på succesfulde (meter)" = "gns_længde_succes_afleveringer"
)

bif_theme <- bs_theme(
  bg = "#FFFFFF", 
  fg = "#002B54", 
  primary = "#FFC425", 
  base_font = font_google("Montserrat")
)

# --- FÆLLES CSS (INKL. NY TYDELIG TOPMENU) ---
fælles_css <- tags$head(
  tags$style(HTML("
      /* --- NY TYDELIG NAVBAR (TOPMENU) --- */
      .navbar { 
        background-color: #002B54 !important; 
        padding: 10px 20px !important; 
        border-bottom: 4px solid #FFC425 !important; 
      }
      .navbar-brand { 
        color: #FFC425 !important; 
        font-size: 1.6rem !important; 
      }
      
      /* --- SIKRER AT KUN TOPMENUENS FANER BLIVER GULE --- */
      .navbar .nav-link { 
        color: #FFFFFF !important; 
        font-size: 1.15rem !important; 
        font-weight: 600 !important; 
        margin-right: 20px; 
        opacity: 0.7;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      .navbar .nav-link.active { 
        color: #FFC425 !important; 
        opacity: 1;
        text-decoration: underline; 
        text-underline-offset: 8px; 
        text-decoration-thickness: 3px; 
      }
      .navbar .nav-link:hover { color: #FFC425 !important; opacity: 1; }

      /* --- SIDEBAR CSS --- */
      .sidebar { overflow-y: auto !important; }
      .control-label { color: white !important; font-weight: 500 !important; margin-bottom: 4px !important; }
      .selectize-input, .form-select, .form-control {
        border-radius: 10px !important; border: 1px solid rgba(255,255,255,0.2) !important;
        background-color: rgba(255,255,255,0.15) !important; color: white !important;
      }
      .selectize-input input, .selectize-input .item { color: white !important; text-shadow: none !important; }
      .selectize-input input::placeholder { color: rgba(255,255,255,0.6) !important; }
      .selectize-dropdown {
        background-color: #002B54 !important; border: 1px solid rgba(255,255,255,0.2) !important;
        border-radius: 10px !important; box-shadow: 0px 8px 16px rgba(0,0,0,0.5) !important; z-index: 10000 !important;
      }
      .selectize-dropdown-content .option { color: white !important; }
      .selectize-dropdown-content .option.active, .selectize-dropdown-content .option:hover {
        background-color: #FFC425 !important; color: #002B54 !important; 
      }
      select option { background-color: #002B54 !important; color: white !important; }
      .sidebar h4 {
        color: #FFC425 !important; font-size: 0.85rem; text-transform: uppercase;
        letter-spacing: 1.5px; margin-top: 15px; margin-bottom: 10px; font-weight: 700;
      }
      hr { margin: 10px 0 !important; border-top: 1px solid rgba(255,255,255,0.1) !important; }
      
/* --- RETTELSE: Farver i tabellers søgefelter og rullemenuer --- */
      .dataTables_wrapper .form-control, 
      .dataTables_wrapper input[type=\"search\"] {
        color: #002B54 !important;
        background-color: #FFFFFF !important;
        border: 1px solid #ccc !important;
      }
      
      /* Gør de valgte \"tags\" i tabellens filter-menu gule med mørk tekst */
      .dataTables_wrapper .selectize-input .item {
        color: #002B54 !important;
        background-color: #FFC425 !important;
        font-weight: 600 !important;
        border-radius: 4px !important;
      }
      
      /* Gør teksten man taster i filteret mørkeblå i stedet for hvid */
      .dataTables_wrapper .selectize-input input {
        color: #002B54 !important;
      }
    "))
)

# --- UI OPBYGNING ---
ui <- page_navbar(
  # 1. VENSTRE: Kun Brøndby-logoet
  title = tags$img(
    src = "https://upload.wikimedia.org/wikipedia/en/thumb/b/b5/Brondby_IF_logo.svg/1280px-Brondby_IF_logo.svg.png", 
    height = "45px"
  ),
  theme = bif_theme,
  fillable = FALSE,
  header = fælles_css,
  
  # 2. SPACER: Skubber fanerne væk fra logoet og ind mod midten
  nav_spacer(),
  

  # =========================================================
  # TAB 0: BRØNDBY IF OVERBLIK (MENU MED UNDERFANER)
  # =========================================================
  nav_menu("Brøndby IF Overblik", 
           
           # --- UNDERFANE 1: VISIONS-TRACKER ---
           nav_panel("Visions-tracker", 
                     layout_sidebar(
                       sidebar = sidebar(
                         width = 400,
                         bg = "#002B54",
                         h4("Klubbens Vision", style = "color: #FFC425; font-weight: bold; margin-bottom: 15px;"),
                         p("Dette dashboard måler, om Brøndby IF lever op til klubbens strategiske DNA: en ", 
                           strong("offensiv og dynamisk spillestil.", style = "color: #FFC425;"), style = "color: white;"),
                         hr(style = "border-top: 1px solid rgba(255,255,255,0.2);"),
                         h5("Sådan beregnes visionen:", style = "color: white; font-weight: bold; font-size: 1rem;"),
                         p("Vi evaluerer det ", strong("samlede taktiske udtryk"), " i kampen:", style = "color: rgba(255,255,255,0.9); font-size: 0.9rem;"),
                         tags$ul(style = "color: rgba(255,255,255,0.8); font-size: 0.85rem; padding-left: 20px; margin-bottom: 15px;",
                                 tags$li("Ligaens kampe grupperes i 5 taktiske profiler baseret på data."),
                                 tags$li("Visionen er defineret som klyngerne ", strong("'Dominant offensiv'"), " og ", strong("'Possession & højt pres'"), "."),
                                 tags$li("Vi tjekker automatisk, om Brøndby lykkedes med at spille sig ind i disse dynamiske profiler.")
                         ),
                         hr(style = "border-top: 1px solid rgba(255,255,255,0.2);")
                       ),
                       fluidRow(
                         column(5, uiOutput("vision_kpi_box")),
                         column(7, card(card_header("Sæsonens fordeling: Rammer vi visionen?", style = "background-color: #002B54; color: white;"), plotlyOutput("vision_donut_chart", height = "300px")))
                       ),
                       card(card_header("Visions-overholdelse kamp for kamp", style = "background-color: #002B54; color: white; font-weight: bold;"), plotlyOutput("vision_timeline_chart", height = "450px"))
                     )
           ),
           
           # --- UNDERFANE 2: SPILLERPROFILER (AFLEVERINGER) ---
           nav_panel("Spillerprofiler: Afleveringer",
                     layout_sidebar(
                       sidebar = sidebar(
                         width = 350,
                         bg = "#002B54",
                         h4("Søg spiller", style = "color: #FFC425; font-weight: bold;"),
                         selectizeInput("bif_player_select", "Vælg Brøndby-spiller:", choices = NULL),
                         hr(),
                         radioButtons("bif_player_baseline", "Sammenlign spillerens tal med:", 
                                      choices = c("Superliga-gennemsnittet" = "liga", 
                                                  "Holdgennemsnittet (BIF)" = "bif")),
                         hr(),
                         p(HTML("<b>Læsevejledning:</b><br>Grønne tal betyder, at spilleren overgår det valgte gennemsnit. Røde tal betyder, han ligger under."), style = "color: rgba(255,255,255,0.7); font-size: 0.85rem;")
                       ),
                       
                       # --- TOP SEKTION: SPILLERKORT & KPI'ER ---
                       fluidRow(
                         # Spillerbillede og info (Venstre side)
                         column(3, uiOutput("player_profile_card")),
                         
                         # KPI Bokse med nøgletal (Højre side)
                         column(9, uiOutput("player_kpi_boxes"))
                       ),
                       
                       br(),
                       
                       # --- BUND SEKTION: TABEL MED ALLE SPILLERE ---
                       card(
                         card_header("Overblik over alle spillere (Afleveringsdata)", style = "background-color: #002B54; color: white; font-weight: bold;"),
                         div(style = "overflow-x: auto;", DTOutput("bif_players_table"))
                       )
                     )
           )),
  
  
  # =========================================================
  # TAB 1: KAMPANALYSE (MENU MED UNDERFANER)
  # =========================================================
  nav_menu("Kampanalyse", 
           
           # --- UNDERFANE 1: TAKTISK KLYNGEANALYSE ---
           nav_panel("Taktisk Klyngeanalyse", 
                     layout_sidebar(
                       sidebar = sidebar(
                         width = 400,
                         bg = "#002B54",
                         h4("Visningsindstillinger"),
                         radioButtons("plot_view", "Vælg visning:", choices = c("Alle kampe" = "all", "Kun cluster-gennemsnit" = "means")),
                         hr(),
                         h4("Variabelvalg"),
                         selectInput("var_x", "Vælg x-akse", choices = var_choices, selected = "PCA1"), 
                         selectInput("var_y", "Vælg y-akse", choices = var_choices, selected = "PCA2"), 
                         hr(),
                         h4("Holdfokus"),
                         selectizeInput("highlight_teams", "Vælg hold til fremhævning",
                                        choices = setNames(filtered_teams$TEAM_WYID, filtered_teams$TEAMNAME), 
                                        multiple = TRUE, options = list(placeholder = 'Søg efter hold...')),
                         hr(),
                         h4("Indstillinger"),
                         numericInput("clusters", "Antal klynger (k)", value = 5, min = 2, max = 15),
                         actionButton("run", "Opdater analyse", class = "btn-primary", 
                                      style = "background-color: #FFC425; border: none; color: #002B54; font-weight: 800; width: 100%; margin-top: 15px;")
                       ),
                       
                       card(
                         card_header("Interaktiv kamp-cluster-analyse", style = "background-color: #002B54; color: white;"),
                         plotlyOutput("cluster_plot", height = "600px")
                       ),
                       
                       navset_card_underline(
                         title = span("Cluster-detaljer & validering", style = "font-weight: bold; color: #002B54;"),
                         nav_panel(span("Cluster-overblik", tags$br(), tags$small("(Originale værdier)", style="font-weight:normal; color:gray;")), icon = icon("table"), div(style = "overflow-x: auto;", DTOutput("cluster_summary"))),
                         nav_panel(span("Cluster-afstande", tags$br(), tags$small("(Skalerede værdier)", style="font-weight:normal; color:gray;")), icon = icon("ruler"), tableOutput("cluster_distances")),
                         nav_panel("Elbow-plot", icon = icon("chart-line"), plotOutput("elbow")),
                         nav_panel("Variabelvægtning (PCA)", icon = icon("weight-hanging"), 
                                   htmlOutput("pca_forklaring"),
                                   div(style = "margin-bottom: 15px; padding: 15px; background-color: #002B54; border-radius: 8px; border-left: 5px solid #FFC425; box-shadow: 0px 4px 10px rgba(0,0,0,0.1);",
                                       selectInput("sort_pca", "Vælg dimension at sortere tabellen efter:",
                                                   choices = c("Dimension 1 (PC1)" = "PC1",
                                                               "Dimension 2 (PC2)" = "PC2",
                                                               "Dimension 3 (PC3)" = "PC3",
                                                               "Dimension 4 (PC4)" = "PC4",
                                                               "Dimension 5 (PC5)" = "PC5"),
                                                   width = "400px")
                                   ),
                                   tableOutput("pca_loadings")),
                         nav_panel("Forklaringsgrad", icon = icon("percentage"), htmlOutput("kmeans_variance"))
                       )
                     )
           ),
           
           # --- UNDERFANE 2: RÅ KAMPDATA (ENKELT KAMPE) ---
           nav_panel("Rå Kampdata",
                     layout_sidebar(
                       sidebar = sidebar(
                         width = 400,
                         bg = "#002B54",
                         h4("Søg efter kamp", style = "color: #FFC425; font-weight: bold;"),
                         selectizeInput("single_match_select", "Vælg kamp og holdfokus:", choices = NULL, options = list(placeholder = 'Søg på dato eller hold...')),
                         hr(),
                         p("Her kan du dykke ned i de rå tal for en specifik kamp og holde dem op imod det valgte holds gennemsnit for hele sæsonen.", style = "color: rgba(255,255,255,0.8); font-size: 0.9rem;")
                       ),
                       card(
                         card_header(uiOutput("single_match_header"), style = "background-color: #002B54; color: white; font-weight: bold;"),
                         div(style = "overflow-x: auto;", DTOutput("single_match_table"))
                       )
                     )
           )
  ),
  
  # =========================================================
  # TAB 2: AFLEVERINGSANALYSE (HOLD & SPILLERE)
  # =========================================================
  nav_panel("Afleveringsanalyse",
            layout_sidebar(
              sidebar = sidebar(
                width = 400,
                bg = "#002B54",
                h4("Analyseniveau"),
                radioButtons("pass_level", "Vælg niveau for klynger:", 
                             choices = c("Holdniveau" = "hold", "Spillerniveau" = "spiller"), 
                             selected = "spiller"),
                hr(),
                # --- NYT: VISNINGSINDSTILLINGER ---
                h4("Visningsindstillinger"),
                radioButtons("pass_plot_view", "Vælg visning:", 
                             choices = c("Alle prikker" = "all", "Kun cluster-gennemsnit" = "means")),
                hr(),
                h4("Variabelvalg"),
                selectInput("pass_var_x", "Vælg x-akse", choices = pass_var_choices, selected = "PCA1"), 
                selectInput("pass_var_y", "Vælg y-akse", choices = pass_var_choices, selected = "PCA2"), 
                hr(),
                h4("Holdfokus"),
                selectizeInput("highlight_teams_pass", "Vælg hold til fremhævning",
                               choices = setNames(filtered_teams$TEAM_WYID, filtered_teams$TEAMNAME), 
                               multiple = TRUE, options = list(placeholder = 'Søg efter hold...')),
                hr(),
                h4("Indstillinger"),
                numericInput("pass_clusters", "Antal klynger (k)", value = 5, min = 2, max = 15),
                actionButton("run_pass", "Opdater afleveringsanalyse", class = "btn-primary", 
                             style = "background-color: #FFC425; border: none; color: #002B54; font-weight: 800; width: 100%; margin-top: 15px;")
              ),
              
              card(
                card_header("Interaktivt afleveringskort", style = "background-color: #002B54; color: white;"),
                plotlyOutput("pass_cluster_plot", height = "600px")
              ),
              
              navset_card_underline(
                id = "pass_tabs", # NYT: Giver menuen et ID, så vi kan styre den fra serveren
                title = span("Cluster-detaljer", style = "font-weight: bold; color: #002B54;"),
                nav_panel(span("Cluster-overblik", tags$br(), tags$small("(Originale værdier)", style="font-weight:normal; color:gray;")), icon = icon("table"), div(style = "overflow-x: auto;", DTOutput("pass_cluster_summary"))),
                nav_panel(span("Cluster-afstande", tags$br(), tags$small("(Skalerede værdier)", style="font-weight:normal; color:gray;")), icon = icon("ruler"), tableOutput("pass_cluster_distances")),
                nav_panel("Elbow-plot", icon = icon("chart-line"), plotOutput("pass_elbow")),
                nav_panel("Variabelvægtning (PCA)", icon = icon("weight-hanging"), tableOutput("pass_pca_loadings")),
                
                # NYT: Tilføjet 'value' så vi kan målrette netop denne fane
                nav_panel("Klynge-medlemmer", value = "klynge_tab", icon = icon("users"), div(style = "overflow-x: auto;", DTOutput("pass_cluster_members"))),
                
                nav_panel("Forklaringsgrad", icon = icon("percentage"), htmlOutput("pass_kmeans_variance"))
              )
            )
  ),
  
  # 3. SPACER: Skubber alt herefter helt ud til højre
  nav_spacer(),
  
  # 4. HØJRE: VESTEGNENS DATA HUB tekst
  nav_item(
    div(
      style = "display: flex; align-items: center; height: 100%; padding-right: 15px;",
      span("VESTEGNENS DATA HUB", style = "font-weight: 700; font-size: 1.1rem; letter-spacing: -0.5px; color: #FFFFFF;")
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  

  # =========================================================
  # SERVER LOGIK: NY VISIONS-TRACKER (TAB 0)
  # =========================================================
  
  # =========================================================
  # VISIONS-TRACKER LOGIK
  # =========================================================
  vision_engine <- reactive({
    res <- analyze_data()
    df_raw <- res$df_raw 
    vision_clusters <- c("Dominant offensiv", "Possession & højt pres")
    
    df_raw$Visions_Status <- ifelse(df_raw$Cluster %in% vision_clusters, "Visionen (Offensiv & Dynamisk)", "Afviger fra visionen")
    
    bif_vision_data <- df_raw %>%
      left_join(filtered_teams %>% select(TEAM_WYID, TEAMNAME), by = "TEAM_WYID") %>%
      left_join(matches %>% select(MATCH_WYID, MATCHLABEL, DATE), by = "MATCH_WYID") %>%
      filter(grepl("Brøndby|Brondby", TEAMNAME, ignore.case = TRUE)) %>%
      arrange(DATE) %>%
      mutate(Kamp_Nr = row_number(), MATCH_DATE_STR = format(DATE, "%d-%m-%Y"))
    
    # Hent de to vigtigste variabler for vores spillestil til hover-teksten
    xg_col_name <- clean_variables$original_variable_names[clean_variables$new_clean_names == "Sum af forventet målchance i kampen for holdet (Xg-sum)"]
    pass_col_name <- clean_variables$original_variable_names[clean_variables$new_clean_names == "Antal afleveringer i kampen for holdet"]
    
    list(data = bif_vision_data, xg_var = xg_col_name, pass_var = pass_col_name)
  })
  
  
  # 2. KPI BOKS: Procent af kampe i Visionen
  output$vision_kpi_box <- renderUI({
    res <- vision_engine()
    df <- res$data
    
    vision_games <- sum(df$Visions_Status == "Visionen (Offensiv & Dynamisk)")
    total_games <- nrow(df)
    pct <- (vision_games / total_games) * 100
    
    value_box(
      title = span("Visionsoverholdelse", style = "font-weight: bold; font-size: 1.1rem;"),
      value = span(paste0(round(pct, 1), "%"), style = "color: #002B54;"),
      p(paste(vision_games, "ud af", total_games, "kampe spillet i Brøndbys DNA."), 
        style = "color: rgba(0,43,84,0.7); margin: 0; font-weight: 500;"),
      theme = value_box_theme(bg = "#FFC425", fg = "#002B54"),
      showcase = icon("bolt-lightning")
    )
  })
  
  # 3. DONUT CHART: Sæson-fordeling
  output$vision_donut_chart <- renderPlotly({
    res <- vision_engine()
    df_pie <- res$data %>% count(Visions_Status)
    
    # Farver: Mørkeblå til visionen, lys grå til afvigelser
    cols <- c("Visionen (Offensiv & Dynamisk)" = "#002B54", "Afviger fra visionen" = "#E0E0E0")
    
    plot_ly(df_pie, labels = ~Visions_Status, values = ~n, type = 'pie', hole = 0.5,
            marker = list(colors = cols[df_pie$Visions_Status], line = list(color = '#FFFFFF', width = 2))) %>%
      layout(showlegend = TRUE, margin = list(l=20, r=20, b=20, t=40),
             legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.1))
  })
  
  output$vision_timeline_chart <- renderPlotly({
    res <- vision_engine()
    df <- res$data
    xg_var <- res$xg_var
    pass_var <- res$pass_var # Hent variabel for afleveringer
    
    df$color_hex <- ifelse(df$Visions_Status == "Visionen (Offensiv & Dynamisk)", "#FFC425", "#002B54")
    
    # NYT: Vi udvider 'text'-parameteren med Taktisk Profil og Antal afleveringer
    p <- ggplot(df, aes(x = Kamp_Nr, y = .data[[xg_var]], group = 1,
                        text = paste0("<b>", MATCHLABEL, "</b><br>",
                                      "Dato: ", MATCH_DATE_STR, "<br>",
                                      "<b>Status:</b> ", Visions_Status, "<br>",
                                      "<b>Taktisk Profil:</b> ", Cluster, "<br><br>",
                                      "<b>xG Sum:</b> ", round(.data[[xg_var]], 2), "<br>",
                                      "<b>Afleveringer:</b> ", round(.data[[pass_var]], 0)))) +
      geom_line(color = "#d3d3d3", size = 1) +
      geom_point(aes(color = Visions_Status), size = 5) +
      scale_color_manual(values = c("Visionen (Offensiv & Dynamisk)" = "#FFC425", "Afviger fra visionen" = "#002B54")) +
      labs(x = "Sæsonens kampe (Kronologisk)", y = "Offensiv Power (xG Sum)", color = "Status") +
      theme_minimal(base_family = "Montserrat") + theme(panel.grid.minor = element_blank())
    
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest")
  })
    
  # =========================================================
  # SERVER LOGIK: SPILLERPROFILER (AFLEVERINGER)
  # =========================================================
  
  # 1. Opdater Dropdown med Brøndby-spillere dynamisk
  observe({
    df <- as.data.frame(spiller_statistik_passes)
    
    # SIKKERHEDSNET: Hvis navn mangler, brug ID
    if("SHORTNAME" %in% colnames(df)) {
      df$SHORTNAME <- ifelse(is.na(df$SHORTNAME) | trimws(df$SHORTNAME) == "", 
                             paste0("Ukendt (ID: ", df$PLAYER_WYID, ")"), 
                             df$SHORTNAME)
    }
    
    bif_spillere <- df %>% 
      filter(grepl("Brøndby|Brondby", TEAMNAME, ignore.case = TRUE)) %>% 
      pull(SHORTNAME)
    
    updateSelectInput(session, "bif_player_select", choices = sort(unique(bif_spillere)))
  })
    
  # 2. Datamotor: Udregner spillerens data vs. positions-specifik baseline
  player_baseline_calc <- reactive({
    req(input$bif_player_select, input$bif_player_baseline)
    df <- as.data.frame(spiller_statistik_passes)
    
    stats_cols <- c("gns_X_afleveringer", "gns_Y_afleveringer", "antal_afleveringer_total", 
                    "antal_succesfulde_afleveringer", "succes_procent_afleveringer", "gns_længde_succes_afleveringer")
    
    # Fjern KUN spillere, hvor selve tallene mangler (i stedet for na.omit)
    df <- df[complete.cases(df[, stats_cols]), ]
    
    # SIKKERHEDSNET: Håndter manglende navne og positioner
    df$SHORTNAME <- ifelse(is.na(df$SHORTNAME) | trimws(df$SHORTNAME) == "", paste0("Ukendt (ID: ", df$PLAYER_WYID, ")"), df$SHORTNAME)
    if(!"ROLENAME" %in% colnames(df)) df$ROLENAME <- "Ukendt"
    df$ROLENAME <- ifelse(is.na(df$ROLENAME), "Ukendt", df$ROLENAME)
    
    # Hent valgt spillers data
    player_data <- df %>% filter(SHORTNAME == input$bif_player_select)
    player_role <- if(nrow(player_data) > 0) player_data$ROLENAME[1] else "Ukendt"
    
    # Filtrer datasættet til kun at indeholde spillere med SAMME position
    df_role <- df %>% filter(ROLENAME == player_role)
    
    # Udregn baseline
    if (input$bif_player_baseline == "liga") {
      baseline_data <- df_role %>% summarise(across(all_of(stats_cols), mean, na.rm=TRUE))
      baseline_name <- paste0("Liga-snit (", player_role, ")")
    } else {
      baseline_data <- df_role %>% filter(grepl("Brøndby|Brondby", TEAMNAME, ignore.case = TRUE)) %>% 
        summarise(across(all_of(stats_cols), mean, na.rm=TRUE))
      baseline_name <- paste0("BIF-snit (", player_role, ")")
    }
    
    list(player = player_data, baseline = baseline_data, baseline_name = baseline_name, stats_cols = stats_cols, player_role = player_role)
  })
    
  # --- BILLED-ORDBOG: Her indsætter du billeder af spillerne ---
  get_player_image <- function(spiller_navn) {
    # Du kan erstatte disse URL'er med links direkte fra brondby.com eller lokale filer
    billede_urls <- list(
      "F. Alves" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/1024169.png?a=1775413630651",
      "R. Lauritsen" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/595154.png?a=1775413630650",
      "J. Vanlerberghe" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/401004.png?a=1775413630651",
      "L. Binks" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/962024.png?a=1775413630650",
      "N. Nartey" = "https://cdn.pixabay.com/photo/2015/10/05/22/37/blank-profile-picture-973460_960_720.png",
      "M. Köhlert" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/788379.png?a=1775413630651",
      "S. Spierings" = "https://cdn.pixabay.com/photo/2015/10/05/22/37/blank-profile-picture-973460_960_720.png",
      "D. Wass" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/74169.png?a=1775413630652",
      "B. Tahirović" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/1415104.png?a=1775413630652",
      "P. Pentz" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/630672.png?a=1775413630648",
      "N. Vallys" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/785052.png?a=1775413630651",
      "S. Klaiber" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/466861.png?a=1775413630651",
      "M. Jensen" = "https://cdn.pixabay.com/photo/2015/10/05/22/37/blank-profile-picture-973460_960_720.png",
      "O. Villadsen" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/1069402.png?a=1775413630649",
      "F. Bundgaard" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/1165444.png?a=1775413630653",
      "J. Ambæk" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/1717020.png?a=1775413630653",
      "M. Divković" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/861992.png?a=1775413630650",
      "C. Bischoff" = "https://cdn.pixabay.com/photo/2015/10/05/22/37/blank-profile-picture-973460_960_720.png",
      "S. Fukuda" = "https://driu3sl4x7vty.cloudfront.net/spdk/current/524x584/8595/1451137.png?a=1775413630653",
      "M. Gregoritsch" = "https://cdn.pixabay.com/photo/2015/10/05/22/37/blank-profile-picture-973460_960_720.png")
    
    # Hvis spilleren findes i listen, vis hans billede - ellers vis et standardbillede
    if (spiller_navn %in% names(billede_urls)) {
      return(billede_urls[[spiller_navn]])
    } else {
      # Standard "tom" profil fra nettet, hvis vi mangler et billede
      return("https://cdn.pixabay.com/photo/2015/10/05/22/37/blank-profile-picture-973460_960_720.png") 
    }
  }
  
  # --- SPILLERKORT: Billede, Navn og Position ---
  output$player_profile_card <- renderUI({
    res <- player_baseline_calc()
    if(nrow(res$player) == 0) return(NULL)
    
    spiller_navn <- res$player$SHORTNAME[1]
    
    # Vi henter positionen direkte fra dataen
    position <- "Ukendt position"
    if("ROLENAME" %in% colnames(res$player) && !is.na(res$player$ROLENAME[1])) {
      position <- res$player$ROLENAME[1]
    }
    
    img_url <- get_player_image(spiller_navn)
    
    # HTML layout for spillerkortet (rundt billede i Brøndby-farver)
    # NYT: 'object-position: center 10%;' sikrer at hovedet ikke bliver skåret af
    HTML(paste0(
      "<div style='background-color: #f8f9fa; border: 1px solid #e0e0e0; border-radius: 12px; padding: 20px; text-align: center; box-shadow: 0 4px 8px rgba(0,0,0,0.05); height: 100%;'>",
      "<img src='", img_url, "' style='width: 120px; height: 120px; object-fit: cover; object-position: center 10%; background-color: #ffffff; border-radius: 50%; border: 4px solid #FFC425; margin-bottom: 15px;'>",
      "<h4 style='color: #002B54; font-weight: 800; margin: 0;'>", spiller_navn, "</h4>",
      "<p style='color: #666; font-size: 0.95rem; font-weight: 500; text-transform: uppercase; letter-spacing: 1px; margin-top: 5px;'>", position, "</p>",
      "</div>"
    ))
  })  
    
  # --- KPI BOKSE: Tydelige tal vs. Gennemsnit ---
  output$player_kpi_boxes <- renderUI({
    res <- player_baseline_calc()
    if(nrow(res$player) == 0) return(NULL)
    
    kpi_metrics <- c("antal_afleveringer_total" = "Afleveringer (Total)", 
                     "succes_procent_afleveringer" = "Succesrate (%)", 
                     "gns_længde_succes_afleveringer" = "Gns. Længde (m)")
    
    # Hent navnet på den valgte baseline (fra 'player_baseline_calc')
    baseline_tekst <- res$baseline_name
    
    boxes <- lapply(names(kpi_metrics), function(col_name) {
      player_val <- res$player[[col_name]][1]
      base_val <- res$baseline[[col_name]][1]
      
      diff <- player_val - base_val
      farve <- ifelse(diff >= 0, "#2e7d32", "#d32f2f")
      ikon <- ifelse(diff >= 0, "arrow-up", "arrow-down")
      
      HTML(paste0(
        "<div style='background-color: white; border: 1px solid #e0e0e0; border-radius: 12px; padding: 20px; box-shadow: 0 4px 8px rgba(0,0,0,0.05); text-align: center;'>",
        "<h6 style='color: #666; font-weight: bold; text-transform: uppercase; font-size: 0.8rem;'>", kpi_metrics[col_name], "</h6>",
        "<h2 style='color: #002B54; font-weight: 900; margin: 10px 0;'>", round(player_val, 1), "</h2>",
        "<p style='color: ", farve, "; font-weight: 600; font-size: 0.9rem; margin: 0;'>",
        "<i class='fas fa-", ikon, "'></i> ", abs(round(diff, 1)), " ift. ", baseline_tekst, "</p>",
        "</div>"
      ))
    })
    
    # Sæt boksene op i et gitter
    fluidRow(
      column(4, HTML(boxes[[1]])),
      column(4, HTML(boxes[[2]])),
      column(4, HTML(boxes[[3]]))
    )
  })  
  # 4. Datatabel: Alle Brøndby-spillere med farvekoder og positioner
  output$bif_players_table <- renderDT({
    res <- player_baseline_calc()
    df <- as.data.frame(spiller_statistik_passes)
    
    # SIKKERHEDSNET: Samme oprydning som i datamotoren
    df <- df[complete.cases(df[, res$stats_cols]), ]
    df$SHORTNAME <- ifelse(is.na(df$SHORTNAME) | trimws(df$SHORTNAME) == "", paste0("Ukendt (ID: ", df$PLAYER_WYID, ")"), df$SHORTNAME)
    if(!"ROLENAME" %in% colnames(df)) df$ROLENAME <- "Ukendt"
    df$ROLENAME <- ifelse(is.na(df$ROLENAME), "Ukendt", df$ROLENAME)
    
    # Filtrer kun Brøndby-spillere og vælg kolonner
    bif_df <- df %>% 
      filter(grepl("Brøndby|Brondby", TEAMNAME, ignore.case = TRUE)) %>%
      filter(ROLENAME == res$player_role) %>%
      select(SHORTNAME, ROLENAME, all_of(res$stats_cols))
    
    colnames(bif_df) <- c("Spiller", "Position", "Gns. X-koordinat (meter)", "Gns. Y-koordinat (meter)", "Afleveringer total", "Succesfulde afleveringer", "Succes-procent (%)", "Gns. længde (meter)")
    
    baseline_vals <- as.numeric(res$baseline[1, ])
    
    # Opret tabellen (Behold resten som det var)
    dt <- datatable(bif_df, 
                    rownames = FALSE,
                    class = 'display nowrap',
                    options = list(
                      paging = FALSE,
                      autoWidth = FALSE,
                      dom = 'ft' 
                    )) %>%
      formatRound(columns = 3:8, digits = 1) 
    
    for (i in 1:length(res$stats_cols)) {
      col_idx <- i + 2 
      base_val <- baseline_vals[i]
      dt <- dt %>% formatStyle(
        col_idx,
        color = styleInterval(c(base_val - 0.001, base_val + 0.001), c('#d32f2f', '#002B54', '#2e7d32')),
        fontWeight = 'bold'
      )
    }
    
    return(dt)
  })    

  # =========================================================
  # SERVER LOGIK: RÅ KAMPDATA (ENKELT KAMPE)
  # =========================================================
  
  # 1. Opdater rullemenuen med alle sæsonens kampe (Dynamisk søgning)
  observe({
    df <- MATCHADVANCEDSTATS %>% na.omit() %>%
      left_join(filtered_teams %>% select(TEAM_WYID, TEAMNAME), by = "TEAM_WYID") %>%
      left_join(matches %>% select(MATCH_WYID, MATCHLABEL, DATE), by = "MATCH_WYID") %>%
      arrange(desc(DATE))
    
    # Skab en pæn tekststreng til rullemenuen
    df <- df %>% mutate(
      Dato_Str = format(DATE, "%d-%m-%Y"),
      Menu_Navn = paste0(Dato_Str, " | ", MATCHLABEL, " (Fokus: ", TEAMNAME, ")")
    )
    
    # En kamp har to hold, så vi laver et unikt ID baseret på Kamp + Hold
    df$UniqueID <- paste(df$MATCH_WYID, df$TEAM_WYID, sep = "_")
    
    valg <- setNames(df$UniqueID, df$Menu_Navn)
    updateSelectizeInput(session, "single_match_select", choices = valg)
  })
  
  # 2. Overskrift der fortæller, hvilken kamp vi kigger på
  output$single_match_header <- renderUI({
    req(input$single_match_select)
    df <- MATCHADVANCEDSTATS %>% na.omit() %>%
      left_join(filtered_teams %>% select(TEAM_WYID, TEAMNAME), by = "TEAM_WYID") %>%
      left_join(matches %>% select(MATCH_WYID, MATCHLABEL), by = "MATCH_WYID") %>%
      mutate(UniqueID = paste(MATCH_WYID, TEAM_WYID, sep = "_")) %>%
      filter(UniqueID == input$single_match_select)
    
    if(nrow(df) == 0) return("Kampdata")
    HTML(paste0("Kampdata: <span style='color: #FFC425;'>", df$MATCHLABEL[1], "</span> (Hold: ", df$TEAMNAME[1], ")"))
  })
  
  # 3. Tabellen med alle kampens stats
  output$single_match_table <- renderDT({
    req(input$single_match_select)
    
    # Hent data
    df <- MATCHADVANCEDSTATS %>% na.omit() %>%
      left_join(filtered_teams %>% select(TEAM_WYID, TEAMNAME), by = "TEAM_WYID") %>%
      mutate(UniqueID = paste(MATCH_WYID, TEAM_WYID, sep = "_"))
    
    selected_row <- df %>% filter(UniqueID == input$single_match_select)
    if(nrow(selected_row) == 0) return(NULL)
    
    stats_cols <- colnames(MATCHADVANCEDSTATS)[5:ncol(MATCHADVANCEDSTATS)]
    team_id <- selected_row$TEAM_WYID[1]
    
    # Udregn holdets sæsongennemsnit til sammenligning
    team_avg <- df %>% filter(TEAM_WYID == team_id) %>% summarise(across(all_of(stats_cols), mean, na.rm=TRUE))
    
    # Byg en data frame hvor hver kolonne er blevet til en række
    res_df <- data.frame(
      "Variabel_Original" = stats_cols,
      "Kamp_Vaerdi" = as.numeric(selected_row[1, stats_cols]),
      "Saeson_Snit" = as.numeric(team_avg[1, stats_cols])
    )
    
    # Oversæt de grimme variabelnavne til pæne danske navne via clean_variables
    match_idx <- match(res_df$Variabel_Original, clean_variables$original_variable_names)
    res_df$Variabel <- ifelse(!is.na(match_idx), as.character(clean_variables$new_clean_names[match_idx]), res_df$Variabel_Original)
    
    # Ryd op i tabellen og giv pæne kolonnenavne
    res_df <- res_df %>% select(Variabel, Kamp_Vaerdi, Saeson_Snit) %>% arrange(Variabel)
    colnames(res_df) <- c("Statistik", "Værdi i kampen", paste0("Sæsonsnit (", selected_row$TEAMNAME[1], ")"))
    
    # Skab tabellen
    datatable(res_df, 
              rownames = FALSE, 
              class = 'display nowrap', 
              options = list(
                paging = FALSE,       # NYT: Slår sider fra, så ALLE variabler vises
                scrollY = "500px",    # NYT: Laver en indbygget rullebar i tabellen
                autoWidth = FALSE,
                dom = 'ft'            # Kun søgefelt og tabel
              )) %>%
      formatRound(columns = 2:3, digits = 2) %>%
      # Farvekod "Værdi i kampen" så man kan se om tallet er over (grøn) eller under (rød) holdets snit
      formatStyle(
        'Værdi i kampen',
        paste0("Sæsonsnit (", selected_row$TEAMNAME[1], ")"),
        color = styleInterval(0, c('#d32f2f', '#2e7d32')),
        fontWeight = 'bold'
      )
  })  
  
  analyze_data <- eventReactive(input$run, {
    req(input$clusters)
    df_raw <- MATCHADVANCEDSTATS %>% na.omit()
    stats_cols <- colnames(df_raw)[5:ncol(df_raw)]
    
    # 1. Skalér og kør PCA FØRST (så skud ikke dominerer)
    scaled_matrix <- scale(df_raw[, stats_cols])
    pca <- prcomp(scaled_matrix)
    pca_for_kmeans <- scale(pca$x[, 1:5])
    df_scaled <- as.data.frame(pca_for_kmeans)
    
    # 2. Kør K-means på de afbalancerede PCA-dimensioner
    set.seed(123)
    km <- kmeans(df_scaled, centers = input$clusters, nstart = 25)
    
    # --- NYT: DYNAMISK NAVNGIVNING AF HOLD-PROFILER ---
    cluster_names <- paste("Cluster", 1:input$clusters)
    
    if (input$clusters == 5) {
      # Vi finder de præcise kolonnenavne fra din oversættelsesliste for at undgå slåfejl
      xg_col <- clean_variables$original_variable_names[clean_variables$new_clean_names == "Sum af forventet målchance i kampen for holdet (Xg-sum)"]
      pass_col <- clean_variables$original_variable_names[clean_variables$new_clean_names == "Antal afleveringer i kampen for holdet"]
      
      # Udregn gennemsnit af xG og Afleveringer for hver klynge for at gennemskue deres taktik
      df_temp <- df_raw
      df_temp$c_id <- km$cluster
      centers <- df_temp %>% group_by(c_id) %>% summarise(xg = mean(.data[[xg_col]]), passes = mean(.data[[pass_col]]))
      
      c_xg <- centers$xg
      c_pass <- centers$passes
      
      # Logikken til at finde profilerne:
      dom_idx <- which.max(c_xg) # Højest xG = Dominant Offensiv
      dir_idx <- which.min(c_pass) # Færrest afleveringer = Direkte / Omstillinger
      
      rem1 <- setdiff(1:5, c(dom_idx, dir_idx))
      poss_idx <- rem1[which.max(c_pass[rem1])] # Flest afleveringer af resten = Possession & Højt pres
      
      rem2 <- setdiff(rem1, poss_idx)
      def_idx <- rem2[which.min(c_xg[rem2])] # Lavest xG af de sidste to = Afventende Defensiv
      bold_idx <- setdiff(rem2, def_idx) # Den sidste = Possession og få chancer
      
      cluster_names[dom_idx] <- "Dominant offensiv"
      cluster_names[dir_idx] <- "Direkte spil & omstillinger"
      cluster_names[poss_idx] <- "Possession & højt pres"
      cluster_names[def_idx] <- "Afventende defensiv"
      cluster_names[bold_idx] <- "Possession og få chancer"
    }
    
    # 3. Gem klyngerne tilbage i det rå datasæt
    df_raw$Cluster <- factor(km$cluster, levels = 1:input$clusters, labels = cluster_names)
    df_raw$cluster_id <- km$cluster # Beholder ID for bagudkompatibilitet
    df_raw$PCA1 <- pca$x[, 1]
    df_raw$PCA2 <- pca$x[, 2]
    
    list(df_raw = df_raw, df_scaled = df_scaled, km = km, pca = pca, stats_cols = stats_cols, c_names = cluster_names)
  }, ignoreNULL = FALSE)
      
  output$cluster_summary <- renderDT({
    res <- analyze_data()
    df_raw <- res$df_raw
    n_cluster <- input$clusters
    stats_cols <- res$stats_cols
    cluster_means <- data.frame(row.names = 1:length(stats_cols))
    
    for (i in 1:n_cluster) {
      current_name <- res$c_names[i]
      cluster_subset <- df_raw[df_raw$Cluster == current_name, stats_cols, drop = FALSE]
      cluster_means1 <- data.frame(colMeans(cluster_subset, na.rm = TRUE))
      colnames(cluster_means1) <- current_name 
      cluster_means <- cbind(cluster_means, cluster_means1)
    }
    
    clean_names <- stats_cols
    match_idx <- match(clean_names, clean_variables$original_variable_names)
    clean_names[!is.na(match_idx)] <- as.character(clean_variables$new_clean_names[match_idx[!is.na(match_idx)]])
    
    cluster_means <- cbind("Variabel" = clean_names, cluster_means)
    
    # 1. Den normale Max/Min Ratio til skærmen
    cluster_means$"Max/Min Ratio" <- apply(cluster_means[, 2:(n_cluster + 1)], 1, function(x) {
      max(x, na.rm = TRUE) / min(x, na.rm = TRUE)
    })
    
    # --- 2. MAGIEN: EKSTREM-SORTERING (1 for Max/Min, 0 for Midten) ---
    row_means <- rowMeans(cluster_means[, 2:(n_cluster + 1)], na.rm = TRUE)
    row_mins <- apply(cluster_means[, 2:(n_cluster + 1)], 1, min, na.rm = TRUE)
    row_maxs <- apply(cluster_means[, 2:(n_cluster + 1)], 1, max, na.rm = TRUE)
    
    row_centers <- (row_maxs + row_mins) / 2
    row_max_dists <- (row_maxs - row_mins) / 2
    safe_max_dists <- ifelse(row_max_dists == 0, 1, row_max_dists) 
    
    col_defs <- list()
    for (i in 1:n_cluster) {
      rel_col_name <- paste0("rel_", i)
      color_col_name <- paste0("color_", i)
      
      cluster_means[[rel_col_name]] <- abs(cluster_means[, i + 1] - row_centers) / safe_max_dists
      
      # NYT: Tildeler 'Max', 'Min' eller 'Mid' baseret på om de har rekorden i rækken
      cluster_means[[color_col_name]] <- ifelse(
        row_maxs == row_mins, "Mid", # Hvis alle hold har præcis samme tal, gøres de sorte
        ifelse(cluster_means[, i + 1] == row_maxs, "Max",
               ifelse(cluster_means[, i + 1] == row_mins, "Min", "Mid"))
      )
      
      col_defs[[length(col_defs) + 1]] <- list(targets = i, orderData = n_cluster + 2 * i)
          }
    
    hidden_targets <- (n_cluster + 2):(ncol(cluster_means) - 1)
    col_defs[[length(col_defs) + 1]] <- list(targets = hidden_targets, visible = FALSE)
    
    # Byg selve tabellen
    dt <- datatable(cluster_means, 
                    rownames = FALSE,
                    class = 'display nowrap',
                    options = list(
                      paging = FALSE,         
                      autoWidth = FALSE, 
                      dom = 'ft',             
                      columnDefs = col_defs 
                    )) %>%
      formatRound(columns = 2:(n_cluster + 2), digits = 3)
    
    # --- NYT: ANVEND FARVERNE PÅ TALLENE (KUN MAX OG MIN) ---
    for (i in 1:n_cluster) {
      col_name <- colnames(cluster_means)[i + 1]
      color_col <- paste0("color_", i)
      
      dt <- dt %>% formatStyle(
        columns = col_name,
        valueColumns = color_col,
        # Grøn til Max, Rød til Min, og Appens mørkeblå (#002B54) til midten
        color = styleEqual(c("Max", "Min", "Mid"), c("#2e7d32", "#d32f2f", "#002B54")),
        # Gør kun ekstrem-tallene fede!
        fontWeight = styleEqual(c("Max", "Min", "Mid"), c("bold", "bold", "normal"))
      )
    }
    
    return(dt)
  })
      
  output$cluster_distances <- renderTable({
    res <- analyze_data()
    dists <- as.data.frame(as.matrix(dist(res$km$centers)))
    colnames(dists) <- res$c_names
    dists <- cbind("Spillestil" = res$c_names, dists)
    return(dists)
  }, digits = 2)
    
  output$pca_loadings <- renderTable({
    res <- analyze_data()
    
    rot <- as.data.frame(res$pca$rotation[, 1:5])
    originale_navne <- rownames(rot)
    paene_navne <- character(length(originale_navne))
    
    for (i in seq_along(originale_navne)) {
      paene_navne[i] <- get_clean_label(originale_navne[i])
    }
    
    final_table <- data.frame(
      "Variabel" = paene_navne, 
      "Dimension 1 (PC1)" = rot$PC1, 
      "Dimension 2 (PC2)" = rot$PC2, 
      "Dimension 3 (PC3)" = rot$PC3, 
      "Dimension 4 (PC4)" = rot$PC4,
      "Dimension 5 (PC5)" = rot$PC5,
      check.names = FALSE
    )
    
    # --- NY LÆSNING AF MENU OG SORTERING ---
    # Læs valget fra menuen (vi sætter den til PC1 som standard ved opstart)
    sort_col <- input$sort_pca
    if(is.null(sort_col)) sort_col <- "PC1"
    
    # Find ud af hvilken kolonne-plads der skal sorteres efter (PC1 er kolonne 2 osv.)
    col_index <- which(c("PC1", "PC2", "PC3", "PC4", "PC5") == sort_col) + 1
    
    # Sortér tabellen efter "absolutte" værdier. 
    # Dette sikrer, at f.eks. -0.40 og +0.40 begge kommer i toppen, 
    # da store minus-tal har lige så stor 'stemme' som store plus-tal.
    final_table <- final_table[order(abs(final_table[[col_index]]), decreasing = TRUE), ]
    
    return(final_table)
  }, digits = 3)
    
  output$cluster_plot <- renderPlotly({
    req(input$plot_view, input$var_x, input$var_y) 
    
    res <- analyze_data()
    df_plot <- res$df_raw
    
    v_x <- input$var_x
    v_y <- input$var_y
    clean_x <- get_clean_label(v_x)
    clean_y <- get_clean_label(v_y)
    
    df_plot <- df_plot %>%
      left_join(filtered_teams %>% select(TEAM_WYID, TEAMNAME), by = "TEAM_WYID") %>%
      left_join(matches %>% select(MATCH_WYID, MATCHLABEL, DATE), by = "MATCH_WYID") %>%
      mutate(MATCH_DATE_STR = format(DATE, "%d-%m-%Y"))
    
    if (isTRUE(input$plot_view == "means")) {
      df_means <- df_plot %>% group_by(Cluster) %>% summarise(plot_x = mean(.data[[v_x]], na.rm = TRUE), plot_y = mean(.data[[v_y]], na.rm = TRUE))
      df_means$hover <- paste0("<b>Cluster ", df_means$Cluster, " Gennemsnit</b><br><br>", "<b>", clean_x, ":</b> ", round(df_means$plot_x, 2), "<br>", "<b>", clean_y, ":</b> ", round(df_means$plot_y, 2))
      
      p <- ggplot(df_means, aes(x = plot_x, y = plot_y, color = Cluster)) + 
        geom_point(aes(text = hover), size = 8, shape = 18) + 
        geom_text(aes(label = paste("Cluster", Cluster)), vjust = -1.5, fontface = "bold", size = 4) + 
        labs(x = clean_x, y = clean_y, color = "Spillestil") +
        theme_minimal(base_family = "Montserrat") + 
        scale_color_brewer(palette = "Set1")
      
    } else {
      
      # --- LØSNINGEN PÅ IDENTISK TEKST ---
      # Vi bygger de to tekst-lister herude, så de tvinges til at være lige så lange som dataene
      if (v_x %in% c("PCA1", "PCA2")) {
        hover_x_str <- rep("N/A (PCA)", nrow(df_plot))
      } else {
        hover_x_str <- round(df_plot[[v_x]], 2)
      }
      
      if (v_y %in% c("PCA1", "PCA2")) {
        hover_y_str <- rep("N/A (PCA)", nrow(df_plot))
      } else {
        hover_y_str <- round(df_plot[[v_y]], 2)
      }
      
      df_plot$hover <- paste0(
        "<b>", df_plot$TEAMNAME, "</b><br>",
        "<b>Kamp:</b> ", df_plot$MATCHLABEL, "<br>",
        "<b>Dato:</b> ", df_plot$MATCH_DATE_STR, "<br>",
        "<b>Spillestil:</b> ", df_plot$Cluster, "<br><br>",
        "<b>", clean_x, ":</b> ", hover_x_str, "<br>",
        "<b>", clean_y, ":</b> ", hover_y_str
      )
      
      hl_teams <- input$highlight_teams
      has_highlights <- length(hl_teams) > 0 && any(hl_teams != "")
      
      # SÆTTER MANUELLE SKALAER (Forhindrer Plotly-crashet)
      if (has_highlights) {
        df_plot$is_h <- factor(ifelse(as.character(df_plot$TEAM_WYID) %in% as.character(hl_teams), "Valgt", "Sløret"), levels = c("Sløret", "Valgt"))
        
        # --- RETTELSE HER: Højere alpha og lidt større prikker i baggrunden ---
        val_alpha <- c("Valgt" = 1.0, "Sløret" = 0.35) 
        val_size <- c("Valgt" = 6, "Sløret" = 2.5)     
        
      } else {
        df_plot$is_h <- factor("Valgt", levels = c("Sløret", "Valgt"))
        val_alpha <- c("Valgt" = 0.8, "Sløret" = 0.8)
        val_size <- c("Valgt" = 3, "Sløret" = 3)
      }
      
      # Sorterer data så de fremhævede tegnes til sidst
      df_plot <- df_plot[order(df_plot$is_h), ]
      
      p <- ggplot(df_plot, aes(x = .data[[v_x]], y = .data[[v_y]], color = Cluster, text = hover, size = is_h, alpha = is_h)) + 
        geom_point(position = position_jitter(width = 0.15, height = 0.15, seed = 123)) + 
        labs(x = clean_x, y = clean_y, color = "Spillestil") + 
        theme_minimal(base_family = "Montserrat") + 
        scale_color_brewer(palette = "Set1") +
        scale_alpha_manual(values = val_alpha, guide = "none") +
        scale_size_manual(values = val_size, guide = "none")
    }
    
    ggplotly(p, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25), margin = list(b = 100), dragmode = "pan")
  })
              
  output$elbow <- renderPlot({
    res <- analyze_data()
    set.seed(123)
    # Vi fjerner [, res$stats_cols], da df_scaled nu er rene PCA-dimensioner
    fviz_nbclust(res$df_scaled, kmeans, method = "wss") + 
      theme_minimal() + 
      labs(title = "Elbow-plot (Kampe)", x = "Antal klynger (k)", y = "Sum af kvadratafvigelser")
  })  
  
  # =========================================================
  # SERVER LOGIK: TAB 2 (AFLEVERINGER)
  # =========================================================
  analyze_passes <- eventReactive(input$run_pass, {
    req(input$pass_clusters, input$pass_level)
    
    stats_cols <- c("gns_X_afleveringer", "gns_Y_afleveringer", "antal_afleveringer_total", 
                    "antal_succesfulde_afleveringer", "succes_procent_afleveringer", "gns_længde_succes_afleveringer")
    
    if(input$pass_level == "hold") {
      df_raw <- as.data.frame(hold_statistik_passes)
      labels <- df_raw$TEAMNAME
    } else {
      df_raw <- as.data.frame(spiller_statistik_passes)
      if("SHORTNAME" %in% colnames(df_raw)) {
        labels <- paste0(df_raw$SHORTNAME, " (", df_raw$TEAMNAME, ")")
      } else {
        labels <- paste0("Spiller ID: ", df_raw$PLAYER_WYID)
      }
    }
    
    valid_rows <- complete.cases(df_raw[, stats_cols])
    df_raw <- df_raw[valid_rows, ]
    labels <- labels[valid_rows]
    
    scaled_matrix <- scale(df_raw[, stats_cols])
    df_scaled <- as.data.frame(scaled_matrix)
    
    set.seed(123)
    km <- kmeans(df_scaled, centers = input$pass_clusters, nstart = 25)
    pca <- prcomp(scaled_matrix)
    
    # --- NYT: DYNAMISK NAVNGIVNING AF KLYNGER ---
    cluster_names <- paste("Cluster", 1:input$pass_clusters)
    
    # KUN hvis vi har valgt præcis 5 klynger (da vores logik bygger på 5 profiler)
    if (input$pass_clusters == 5) {
      centers <- as.data.frame(km$centers)
      
      # ==========================================
      # 1. NAVNGIVNING: SPILLERNIVEAU
      # ==========================================
      if (input$pass_level == "spiller") {
        gk_idx <- which.min(centers$gns_X_afleveringer)
        att_idx <- which.max(centers$gns_X_afleveringer)
        playmaker_idx <- which.max(centers$antal_afleveringer_total)
        
        remaining <- setdiff(1:5, c(gk_idx, att_idx, playmaker_idx))
        if (centers$gns_Y_afleveringer[remaining[1]] > centers$gns_Y_afleveringer[remaining[2]]) {
          left_idx <- remaining[1]; right_idx <- remaining[2]
        } else {
          left_idx <- remaining[2]; right_idx <- remaining[1]
        }
        
        cluster_names[gk_idx] <- "Målmænd & dybt forsvar"
        cluster_names[att_idx] <- "Angribere & gennembrud"
        cluster_names[playmaker_idx] <- "Spilfordelere (Central akse)"
        cluster_names[left_idx] <- "Breddespillere (Venstre)"
        cluster_names[right_idx] <- "Breddespillere (Højre)"
        
        # ==========================================
        # 2. NAVNGIVNING: HOLDNIVEAU
        # ==========================================
      } else if (input$pass_level == "hold") {
        
        # Finder hold med længste afleveringer (Direkte spil)
        dir_idx <- which.max(centers$gns_længde_succes_afleveringer)
        
        # Finder hold med flest afleveringer (Boldbesiddende)
        rem1 <- setdiff(1:5, dir_idx)
        poss_idx <- rem1[which.max(centers$antal_afleveringer_total[rem1])]
        
        # Finder hold der spiller højest på banen (Dominant)
        rem2 <- setdiff(rem1, poss_idx)
        dom_idx <- rem2[which.max(centers$gns_X_afleveringer[rem2])]
        
        # Finder hold der spiller dybest på banen (Afventende)
        rem3 <- setdiff(rem2, dom_idx)
        def_idx <- rem3[which.min(centers$gns_X_afleveringer[rem3])]
        
        # Den sidste klynge (typisk gennemsnitshold)
        bal_idx <- setdiff(rem3, def_idx)
        
        cluster_names[dir_idx] <- "Direkte spil (Lange bolde)"
        cluster_names[poss_idx] <- "Boldbesiddende (Kortpasning)"
        cluster_names[dom_idx] <- "Dominant offensiv (Højt på banen)"
        cluster_names[def_idx] <- "Afventende defensiv (Dybt på banen)"
        cluster_names[bal_idx] <- "Balanceret spillestil"
      }
    }
    
    # Omdan klynge-ID til de nye pæne navne
    df_raw$Cluster <- factor(km$cluster, levels = 1:input$pass_clusters, labels = cluster_names)
    df_raw$PCA1 <- pca$x[, 1]
    df_raw$PCA2 <- pca$x[, 2]
    df_raw$Label <- labels
    
    list(df_raw = df_raw, df_scaled = df_scaled, km = km, pca = pca, stats_cols = stats_cols, c_names = cluster_names)
  }, ignoreNULL = FALSE)
      
  output$pass_cluster_plot <- renderPlotly({
    req(input$pass_plot_view, input$pass_var_x, input$pass_var_y)
    
    res <- analyze_passes()
    df_plot <- res$df_raw
    
    v_x <- input$pass_var_x
    v_y <- input$pass_var_y
    clean_x <- names(pass_var_choices)[pass_var_choices == v_x]
    clean_y <- names(pass_var_choices)[pass_var_choices == v_y]
    
    if (isTRUE(input$pass_plot_view == "means")) {
      df_means <- df_plot %>% group_by(Cluster) %>% summarise(plot_x = mean(.data[[v_x]], na.rm = TRUE), plot_y = mean(.data[[v_y]], na.rm = TRUE))
      df_means$hover <- paste0("<b>", df_means$Cluster, "</b><br><br>", "<b>", clean_x, ":</b> ", round(df_means$plot_x, 2), "<br>", "<b>", clean_y, ":</b> ", round(df_means$plot_y, 2))
      
      p <- ggplot(df_means, aes(x = plot_x, y = plot_y, color = Cluster)) + 
        geom_point(aes(text = hover), size = 8, shape = 18) + 
        labs(x = clean_x, y = clean_y, color = "Spillertype") + 
        theme_minimal(base_family = "Montserrat") + 
        scale_color_brewer(palette = "Set1")
      
    } else {
      df_plot$hover <- paste0(
        "<b>", df_plot$Label, "</b><br>",
        "<b>Profil:</b> ", df_plot$Cluster, "<br><br>",
        "<b>Antal afleveringer:</b> ", df_plot$antal_afleveringer_total, "<br>",
        "<b>Antal succesfulde:</b> ", df_plot$antal_succesfulde_afleveringer, "<br>",
        "<b>Succes-procent (%):</b> ", round(df_plot$succes_procent_afleveringer, 1), "<br>",
        "<b>Gns. X-koordinat (meter):</b> ", round(df_plot$gns_X_afleveringer, 1), "<br>",
        "<b>Gns. Y-koordinat (meter):</b> ", round(df_plot$gns_Y_afleveringer, 1), "<br>",
        "<b>Gns. længde på succesfulde (m):</b> ", round(df_plot$gns_længde_succes_afleveringer, 1)
      )
      
      hl_teams_pass <- input$highlight_teams_pass
      has_highlights_pass <- length(hl_teams_pass) > 0 && any(hl_teams_pass != "")
      
      # MANUELLE SKALAER FOR AFLEVERINGER
      if (has_highlights_pass) {
        df_plot$is_h <- factor(ifelse(as.character(df_plot$TEAM_WYID) %in% as.character(hl_teams_pass), "Valgt", "Sløret"), levels = c("Sløret", "Valgt"))
        val_alpha <- c("Valgt" = 1.0, "Sløret" = 0.35)
        val_size <- c("Valgt" = 6, "Sløret" = 2.5) # Fast standardstørrelse (ligesom kampfanen)
      } else {
        df_plot$is_h <- factor("Valgt", levels = c("Sløret", "Valgt"))
        val_alpha <- c("Valgt" = 0.8, "Sløret" = 0.8)
        val_size <- c("Valgt" = 3, "Sløret" = 3) # Fast standardstørrelse (ligesom kampfanen)
      }
      
      df_plot <- df_plot[order(df_plot$is_h), ]
      
      p <- ggplot(df_plot, aes(x = .data[[v_x]], y = .data[[v_y]], color = Cluster, text = hover, size = is_h, alpha = is_h)) +
        geom_point() +
        labs(x = clean_x, y = clean_y, color = "Spillertype") +
        theme_minimal(base_family = "Montserrat") +
        scale_color_brewer(palette = "Set1") +
        scale_alpha_manual(values = val_alpha, guide = "none") +
        scale_size_manual(values = val_size, guide = "none")
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25), margin = list(b = 100), dragmode = "pan")
  })
      
  output$pass_cluster_summary <- renderDT({
    res <- analyze_passes()
    df_raw <- res$df_raw
    stats_cols <- res$stats_cols
    n_cluster <- input$pass_clusters
    
    cluster_means <- data.frame(row.names = 1:length(stats_cols))
    
    for (i in 1:n_cluster) {
      current_name <- res$c_names[i]
      cluster_subset <- df_raw[df_raw$Cluster == current_name, stats_cols, drop = FALSE]
      cluster_means1 <- data.frame(colMeans(cluster_subset, na.rm = TRUE))
      colnames(cluster_means1) <- current_name 
      cluster_means <- cbind(cluster_means, cluster_means1)
    }
    
    clean_names_passes <- c("Gns. X-koordinat (meter)", "Gns. Y-koordinat (meter)", "Antal afleveringer", 
                            "Antal succesfulde", "Succes-procent (%)", "Gns. længde på succesfulde (meter)")
    
    cluster_means <- cbind("Variabel" = clean_names_passes, cluster_means)
    
    cluster_means$"Max/Min Ratio" <- apply(cluster_means[, 2:(n_cluster + 1)], 1, function(x) {
      max(x, na.rm=TRUE) / min(x, na.rm=TRUE)
    })
    
    # --- 2. MAGIEN: EKSTREM-SORTERING ---
    row_means <- rowMeans(cluster_means[, 2:(n_cluster + 1)], na.rm = TRUE)
    row_mins <- apply(cluster_means[, 2:(n_cluster + 1)], 1, min, na.rm = TRUE)
    row_maxs <- apply(cluster_means[, 2:(n_cluster + 1)], 1, max, na.rm = TRUE)
    
    row_centers <- (row_maxs + row_mins) / 2
    row_max_dists <- (row_maxs - row_mins) / 2
    safe_max_dists <- ifelse(row_max_dists == 0, 1, row_max_dists) 
    
    col_defs <- list()
    for (i in 1:n_cluster) {
      rel_col_name <- paste0("rel_", i)
      color_col_name <- paste0("color_", i)
      
      cluster_means[[rel_col_name]] <- abs(cluster_means[, i + 1] - row_centers) / safe_max_dists
      
      # NYT: Tildeler 'Max', 'Min' eller 'Mid'
      cluster_means[[color_col_name]] <- ifelse(
        row_maxs == row_mins, "Mid",
        ifelse(cluster_means[, i + 1] == row_maxs, "Max",
               ifelse(cluster_means[, i + 1] == row_mins, "Min", "Mid"))
      )
      
      col_defs[[length(col_defs) + 1]] <- list(targets = i, orderData = n_cluster + 2 * i)
          }
    
    hidden_targets <- (n_cluster + 2):(ncol(cluster_means) - 1)
    col_defs[[length(col_defs) + 1]] <- list(targets = hidden_targets, visible = FALSE)
    
    # Byg selve tabellen
    dt <- datatable(cluster_means, 
                    rownames = FALSE,
                    class = 'display nowrap',
                    options = list(
                      paging = FALSE,         
                      autoWidth = FALSE, 
                      dom = 'ft',             
                      columnDefs = col_defs 
                    )) %>%
      formatRound(columns = 2:(n_cluster + 2), digits = 3)
    
    # --- FARVER TIL TAB 2 ---
    for (i in 1:n_cluster) {
      col_name <- colnames(cluster_means)[i + 1]
      color_col <- paste0("color_", i)
      
      dt <- dt %>% formatStyle(
        columns = col_name,
        valueColumns = color_col,
        color = styleEqual(c("Max", "Min", "Mid"), c("#2e7d32", "#d32f2f", "#002B54")),
        fontWeight = styleEqual(c("Max", "Min", "Mid"), c("bold", "bold", "normal"))
      )
    }
    
    return(dt)})
  
  output$pass_cluster_distances <- renderTable({
    res <- analyze_passes()
    dists <- as.data.frame(as.matrix(dist(res$km$centers)))
    
    # Her bruger vi res$c_names til at trække de dynamiske navne ind i rækker og kolonner!
    colnames(dists) <- res$c_names
    dists <- cbind("Profil" = res$c_names, dists)
    
    return(dists)
  }, digits = 2)
    
  output$pass_pca_loadings <- renderTable({
    res <- analyze_passes()
    rot <- as.data.frame(res$pca$rotation[, 1:2])
    rot$Variabel <- rownames(rot)
    
    # Oversæt til pæne navne
    for(i in 1:nrow(rot)) {
      match_name <- names(pass_var_choices)[pass_var_choices == rot$Variabel[i]]
      if(length(match_name) > 0) rot$Variabel[i] <- match_name
    }
    
    final_table <- data.frame(
      "Variabel" = rot$Variabel,
      "Vægt på X-aksen (PCA1)" = rot$PC1,
      "Vægt på Y-aksen (PCA2)" = rot$PC2,
      check.names = FALSE
    )
    
    # --- LÆS MENU OG SORTÉR ---
    sort_col <- input$sort_pass_pca
    if(is.null(sort_col)) sort_col <- "PC1" # Standard er X-aksen
    
    # Find ud af hvilken kolonne der skal sorteres efter
    col_index <- which(c("PC1", "PC2") == sort_col) + 1
    
    # Sortér tabellen efter absolutte værdier (største indflydelse øverst)
    final_table <- final_table[order(abs(final_table[[col_index]]), decreasing = TRUE), ]
    
    return(final_table)
  }, digits = 3)
  
  
  output$pass_cluster_members <- renderDT({
    res <- analyze_passes()
    df_raw <- res$df_raw
    
    if (input$pass_level == "hold") {
      out_df <- data.frame(
        "Hold" = df_raw$TEAMNAME,
        "Profil (Klynge)" = df_raw$Cluster,
        check.names = FALSE
      )
    } else {
      # --- FIX 1: Håndtering af spøgelses-spillere ---
      spiller_navn <- if("SHORTNAME" %in% colnames(df_raw)) {
        # Hvis navnet mangler (NA) eller bare er helt blankt (""), så vis spillerens ID
        ifelse(is.na(df_raw$SHORTNAME) | trimws(df_raw$SHORTNAME) == "", 
               paste0("Ukendt spiller (ID: ", df_raw$PLAYER_WYID, ")"), 
               df_raw$SHORTNAME)
      } else {
        as.character(df_raw$PLAYER_WYID)
      }
      
      out_df <- data.frame(
        "Spiller" = spiller_navn,
        "Hold" = df_raw$TEAMNAME,
        "Profil (Klynge)" = df_raw$Cluster,
        check.names = FALSE
      )
    }
    
    out_df <- out_df[order(out_df$"Profil (Klynge)", out_df$Hold), ]
    
    # --- FIX 2: Én lang side uden scrollbars ---
    datatable(out_df,
              rownames = FALSE,
              filter = 'top', 
              class = 'display nowrap',
              options = list(
                paging = FALSE,         # Slår sideinddeling (1, 2, 3...) helt fra
                autoWidth = FALSE,
                dom = 'ft'              # 'f' = globalt søgefelt, 't' = selve tabellen. Fjerner 'ip' (info og sider).
              ))
  })
    
  # --- SKJUL/VIS KLYNGE-FANEN DYNAMISK ---
  observe({
    if (input$pass_level == "hold") {
      nav_hide(id = "pass_tabs", target = "klynge_tab") # Skjuler fanen
    } else {
      nav_show(id = "pass_tabs", target = "klynge_tab") # Viser fanen
    }
  })  
    
  output$pass_elbow <- renderPlot({
    res <- analyze_passes()
    
    # Lås 'seed' for at matche den manuelle kode 100 %
    set.seed(123) 
    fviz_nbclust(res$df_scaled, kmeans, method = "wss") + 
      theme_minimal() + 
      labs(title = "Elbow-plot (Afleveringer)", x = "Antal klynger (k)", y = "Sum af kvadratafvigelser")
  })
    
  # --- FORKLARINGSGRAD: KAMPE ---
  output$kmeans_variance <- renderUI({
    res <- analyze_data()
    variance_explained <- (res$km$betweenss / res$km$totss) * 100
    
    HTML(paste0(
      "<div style='padding: 20px; font-family: Montserrat;'>",
      "<h4 style='color: #002B54; font-weight: 700;'>K-means forklaringsgrad (Variation)</h4>",
      "<p style='font-size: 2.5rem; font-weight: 800; color: #FFC425; margin-bottom: 5px;'>", 
      round(variance_explained, 1), " %</p>",
      "<p style='color: #666; font-size: 1.1rem;'>Denne procentdel beskriver, hvor stor en del af den samlede variation i dataene, klyngerne formår at fange (<em>Between SS / Total SS</em>). Jo tættere på 100 %, jo mere præcist er datapunkterne grupperet.</p>",
      "</div>"
    ))
  })
  
  
  # --- FORKLARINGSGRAD: AFLEVERINGER ---
  output$pass_kmeans_variance <- renderUI({
    res <- analyze_passes()
    variance_explained <- (res$km$betweenss / res$km$totss) * 100
    
    HTML(paste0(
      "<div style='padding: 20px; font-family: Montserrat;'>",
      "<h4 style='color: #002B54; font-weight: 700;'>K-means forklaringsgrad (Variation)</h4>",
      "<p style='font-size: 2.5rem; font-weight: 800; color: #FFC425; margin-bottom: 5px;'>", 
      round(variance_explained, 1), " %</p>",
      "<p style='color: #666; font-size: 1.1rem;'>Denne procentdel beskriver, hvor stor en del af den samlede variation i afleverings-dataene, klyngerne formår at fange (<em>Between SS / Total SS</em>). Jo tættere på 100 %, jo mere præcist er datapunkterne grupperet.</p>",
      "</div>"
    ))
  })
  
  # --- FORKLARINGSTEKST TIL PCA (KAMPE) ---
  output$pca_forklaring <- renderUI({
    HTML("
      <div style='background-color: #f8f9fa; padding: 15px; border-left: 5px solid #FFC425; margin-bottom: 20px; margin-top: 10px; font-family: Montserrat;'>
        <h5 style='color: #002B54; font-weight: bold;'>Hvad betyder denne tabel?</h5>
        <p style='color: #333; font-size: 0.95rem;'>Algoritmen har kogt de mange statistikker ned til <b>5 overordnede taktiske temaer (Dimensioner)</b>, som den bruger til at gruppere holdene efter. Tallene i tabellen (fra -1 til 1) viser, hvor meget 'stemmekraft' hver variabel har i den enkelte dimension.</p>
        <ul style='color: #333; font-size: 0.95rem;'>
          <li><b>Store tal (f.eks. over 0.20 eller under -0.20):</b> Variablen dominerer dimensionen. Hvis f.eks. 'Skud' og 'xG' begge har store tal i Dimension 1, handler Dimension 1 om offensivt tryk.</li>
          <li><b>Tal tæt på 0:</b> Variablen har nærmest ingen indflydelse på netop denne dimension.</li>
          <li><b>Fortegn (+ og -):</b> Viser bare retningen. To variabler med minus-fortegn følges for eksempel ad.</li>
        </ul>
      </div>
    ")
  })
  
  # --- FORKLARINGSTEKST TIL PCA (AFLEVERINGER) ---
  output$pass_pca_forklaring <- renderUI({
    HTML("
      <div style='background-color: #f8f9fa; padding: 15px; border-left: 5px solid #FFC425; margin-bottom: 20px; margin-top: 10px; font-family: Montserrat;'>
        <h5 style='color: #002B54; font-weight: bold;'>Hvad betyder denne tabel?</h5>
        <p style='color: #333; font-size: 0.95rem;'>Tabellen afslører, hvordan algoritmen ser på afleveringsdataene. Den har fundet <b>underliggende mønstre (Dimensioner)</b>. Tallene viser, hvor stor en 'stemme' hver statistik har i at definere spillertypen.</p>
        <ul style='color: #333; font-size: 0.95rem;'>
          <li>Jo længere tallet er fra nul (f.eks. over 0.40 eller under -0.40), jo vigtigere er den statistik for den pågældende dimension.</li>
          <li><b>Eksempel:</b> Hvis Dimension 1 styres kraftigt af X-koordinaten, adskiller denne dimension spillerne ud fra, hvor højt på banen de spiller. Er det i stedet antallet af afleveringer, skiller den spillerne ud fra deres grad af involvering.</li>
        </ul>
      </div>
    ")
  })
  
}

shinyApp(ui, server)
