
library(shiny)
library(readxl)
library(shiny)
library(bslib)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(leaflet)
library(scales)

fires <- read.csv("data/Fires_Short.csv")

fires$FIRE_YEAR  <- as.numeric(fires$FIRE_YEAR)
fires$FIRE_SIZE  <- as.numeric(fires$FIRE_SIZE)
fires$LATITUDE   <- as.numeric(fires$LATITUDE)
fires$LONGITUDE  <- as.numeric(fires$LONGITUDE)

fires <- fires %>%
  filter(!is.na(FIRE_YEAR), !is.na(FIRE_SIZE),
         !is.na(LATITUDE), !is.na(LONGITUDE))

fires$risk_index  <- log1p(fires$FIRE_SIZE)

# Fire size class labels (NWCG standard)
fires$SIZE_CLASS <- cut(
  fires$FIRE_SIZE,
  breaks = c(0, 0.25, 10, 100, 300, 1000, 5000, Inf),
  labels = c("A (<0.25)", "B (0.25–10)", "C (10–100)",
             "D (100–300)", "E (300–1k)", "F (1k–5k)", "G (>5k)"),
  include.lowest = TRUE
)

# Month labels (if DISCOVERY_DOY present, derive month)
if ("DISCOVERY_DOY" %in% names(fires)) {
  fires$MONTH <- as.integer((fires$DISCOVERY_DOY - 1) / 30.44) + 1
  fires$MONTH  <- pmin(fires$MONTH, 12)
  fires$MONTH_LABEL <- factor(month.abb[fires$MONTH], levels = month.abb)
}

# Palette & theme helpers
FIRE_ORANGE  <- "#FF6B35"
FIRE_RED     <- "#E63946"
FIRE_AMBER   <- "#FFBE0B"
SMOKE_DARK   <- "#0D0D0D"
SMOKE_MID    <- "#1A1A2E"
SMOKE_GREY   <- "#2E2E3A"
TEXT_LIGHT   <- "#E8E8E8"
TEXT_DIM     <- "#9E9EAE"

gg_fire_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background   = element_rect(fill = SMOKE_GREY, color = NA),
      panel.background  = element_rect(fill = SMOKE_GREY, color = NA),
      panel.grid.major  = element_line(color = "#3A3A4A", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      text              = element_text(color = TEXT_LIGHT, family = "sans"),
      axis.text         = element_text(color = TEXT_DIM, size = 10),
      axis.title        = element_text(color = TEXT_LIGHT, size = 11,
                                       face = "bold"),
      plot.title        = element_text(color = FIRE_ORANGE, size = 13,
                                       face = "bold"),
      legend.background = element_rect(fill = SMOKE_GREY, color = NA),
      legend.text       = element_text(color = TEXT_LIGHT),
      legend.title      = element_text(color = TEXT_DIM),
      strip.text        = element_text(color = TEXT_LIGHT, face = "bold")
    )
}

# CSS overrides
custom_css <- "
/* ── Google fonts ── */
@import url('https://fonts.googleapis.com/css2?family=Rajdhani:wght@400;600;700&family=DM+Sans:wght@300;400;500&display=swap');

:root {
  --fire-orange: #FF6B35;
  --fire-red:    #E63946;
  --fire-amber:  #FFBE0B;
  --smoke-dark:  #0D0D0D;
  --smoke-mid:   #1A1A2E;
  --smoke-grey:  #2E2E3A;
  --text-light:  #E8E8E8;
  --text-dim:    #9E9EAE;
}

/* page */
body, .bslib-page-sidebar {
  background: var(--smoke-dark) !important;
  font-family: 'DM Sans', sans-serif;
  color: var(--text-light);
}

/* title bar */
.navbar, .bslib-sidebar-layout > .main > .navbar {
  background: linear-gradient(90deg, #0D0D0D 0%, #1A1A2E 100%) !important;
  border-bottom: 2px solid var(--fire-orange) !important;
}
.navbar-brand {
  font-family: 'Rajdhani', sans-serif !important;
  font-size: 1.5rem !important;
  letter-spacing: 2px !important;
  color: var(--fire-orange) !important;
  text-transform: uppercase;
}

/* sidebar */
.bslib-sidebar-layout > .sidebar {
  background: #111120 !important;
  border-right: 1px solid #2E2E3A !important;
}
.sidebar .form-label, .sidebar label {
  color: var(--text-dim) !important;
  font-size: 0.75rem;
  text-transform: uppercase;
  letter-spacing: 1px;
  font-weight: 500;
}
.sidebar hr { border-color: #2E2E3A; }
.sidebar .help-block, .sidebar .shiny-input-container .helpText {
  color: #666 !important; font-size: 0.72rem;
}

/* cards */
.card {
  background: var(--smoke-grey) !important;
  border: 1px solid #3A3A4A !important;
  border-radius: 10px !important;
  box-shadow: 0 4px 24px rgba(0,0,0,0.5) !important;
  transition: border-color .2s;
}
.card:hover { border-color: var(--fire-orange) !important; }
.card-header {
  background: #23232F !important;
  border-bottom: 1px solid #3A3A4A !important;
  color: var(--text-light) !important;
  font-family: 'Rajdhani', sans-serif !important;
  font-size: 1.05rem;
  letter-spacing: 1px;
  font-weight: 600;
  padding: 0.65rem 1rem !important;
}

/* KPI value cards */
.kpi-card {
  background: linear-gradient(135deg, #23232F 0%, #1A1A2E 100%);
  border: 1px solid #3A3A4A;
  border-radius: 10px;
  padding: 1.1rem 1.4rem;
  text-align: center;
  transition: border-color .2s, transform .2s;
}
.kpi-card:hover { border-color: var(--fire-orange); transform: translateY(-2px); }
.kpi-value {
  font-family: 'Rajdhani', sans-serif;
  font-size: 2.2rem;
  font-weight: 700;
  color: var(--fire-orange);
  line-height: 1.1;
}
.kpi-label {
  font-size: 0.7rem;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  color: var(--text-dim);
  margin-top: 4px;
}
.kpi-sub {
  font-size: 0.75rem;
  color: #666;
  margin-top: 2px;
}

/* form controls */
.selectize-control .selectize-input,
.selectize-dropdown {
  background: #23232F !important;
  border-color: #3A3A4A !important;
  color: var(--text-light) !important;
}
.selectize-dropdown .option:hover { background: #3A3A4A !important; }
.irs--shiny .irs-bar { background: var(--fire-orange) !important; }
.irs--shiny .irs-handle { background: var(--fire-orange) !important;
                           border-color: var(--fire-amber) !important; }
.irs--shiny .irs-from, .irs--shiny .irs-to,
.irs--shiny .irs-single { background: var(--fire-orange) !important; }
.irs--shiny .irs-line { background: #3A3A4A !important; }
.irs--shiny .irs-min, .irs--shiny .irs-max { color: var(--text-dim) !important; }

/* tabs */
.nav-tabs { border-bottom: 1px solid #3A3A4A !important; }
.nav-tabs .nav-link {
  color: var(--text-dim) !important;
  border: none !important;
  font-size: 0.8rem;
  text-transform: uppercase;
  letter-spacing: 1px;
}
.nav-tabs .nav-link.active {
  color: var(--fire-orange) !important;
  background: transparent !important;
  border-bottom: 2px solid var(--fire-orange) !important;
}

/* table */
table.dataTable, .shiny-html-output table {
  color: var(--text-light) !important;
  border-collapse: collapse;
  width: 100%;
}
table.dataTable th {
  font-family: 'Rajdhani', sans-serif;
  text-transform: uppercase;
  letter-spacing: 1px;
  font-size: 0.78rem;
  color: var(--text-dim);
  border-bottom: 1px solid #3A3A4A;
  padding: 8px 12px;
}
table.dataTable td {
  padding: 7px 12px;
  font-size: 0.85rem;
  border-bottom: 1px solid #252530;
}
table.dataTable tbody tr:hover { background: #23232F !important; }

/* download button */
.btn-dl {
  background: transparent;
  border: 1px solid var(--fire-orange);
  color: var(--fire-orange);
  border-radius: 6px;
  font-size: 0.78rem;
  padding: 4px 14px;
  letter-spacing: 1px;
  text-transform: uppercase;
  cursor: pointer;
  transition: background .15s;
}
.btn-dl:hover { background: var(--fire-orange); color: #000; }

/* scrollbar */
::-webkit-scrollbar { width: 6px; }
::-webkit-scrollbar-track { background: #111; }
::-webkit-scrollbar-thumb { background: #3A3A4A; border-radius: 3px; }
"

# UI 
ui <- page_navbar(
  
  title = "🔥 WILDFIRE ANALYTICS",
  id    = "nav",
  
  theme = bs_theme(
    version    = 5,
    bootswatch = "darkly",
    base_font  = font_google("DM Sans")
  ),
  
  header = tags$head(
    tags$style(HTML(custom_css))
  ),
  
  # Tab 1: Overview 
  nav_panel(
    "Overview",
    icon = icon("fire"),
    
    layout_sidebar(
      
      sidebar = sidebar(
        width = 230,
        
        selectInput("state", "State",
                    choices  = c("All", sort(unique(fires$STATE))),
                    selected = "All"),
        
        sliderInput("year", "Year Range",
                    min   = min(fires$FIRE_YEAR),
                    max   = max(fires$FIRE_YEAR),
                    value = c(min(fires$FIRE_YEAR), max(fires$FIRE_YEAR)),
                    step  = 1, sep = ""),
        
        selectInput("cause", "Cause",
                    choices  = c("All", sort(unique(fires$NWCG_CAUSE_CLASSIFICATION))),
                    selected = "All"),
        
        selectInput("size_class", "Size Class",
                    choices  = c("All", levels(fires$SIZE_CLASS)),
                    selected = "All"),
        
        hr(),
        
        downloadButton("dl_csv", "⬇ Export CSV",
                       class = "btn-dl w-100"),
        
        hr(),
        
        helpText("1992–2015 US wildfire records. Filters apply to all panels.")
      ),
      
      # KPI row
      uiOutput("kpi_row"),
      
      br(),
      
      layout_columns(
        col_widths = c(5, 3, 4),
        
        card(
          full_screen = TRUE,
          card_header("Ignitions Over Time"),
          plotOutput("trend", height = 280)
        ),
        
        card(
          full_screen = TRUE,
          card_header("Cause Breakdown"),
          plotOutput("cause_bar", height = 280)
        ),
        
        card(
          full_screen = TRUE,
          card_header("Size Class Composition"),
          plotOutput("size_class_plot", height = 280)
        )
      ),
      
      layout_columns(
        col_widths = c(4, 8),
        
        card(
          full_screen = TRUE,
          card_header("Fire Size Distribution (log scale)"),
          plotOutput("size_hist", height = 280)
        ),
        
        card(
          full_screen = TRUE,
          card_header("\U0001f4cd Spatial Risk Map"),
          leafletOutput("overview_map", height = 280)
        )
      )
    )
  ),
  
  # Tab 2: Spatial Map 
  nav_panel(
    "Spatial Map",
    icon = icon("map"),
    
    layout_sidebar(
      
      sidebar = sidebar(
        width = 230,
        
        selectInput("map_state", "State",
                    choices  = c("All", sort(unique(fires$STATE))),
                    selected = "All"),
        
        sliderInput("map_year", "Year Range",
                    min   = min(fires$FIRE_YEAR),
                    max   = max(fires$FIRE_YEAR),
                    value = c(min(fires$FIRE_YEAR), max(fires$FIRE_YEAR)),
                    step  = 1, sep = ""),
        
        selectInput("map_cause", "Cause",
                    choices  = c("All", sort(unique(fires$NWCG_CAUSE_CLASSIFICATION))),
                    selected = "All"),
        
        radioButtons("map_color", "Color By",
                     choices  = c("Risk Index" = "risk_index",
                                  "Fire Size"  = "FIRE_SIZE"),
                     selected = "risk_index"),
        
        sliderInput("map_sample", "Max Points",
                    min = 500, max = 5000, value = 2000, step = 500),
        
        hr(),
        helpText("Points sampled for performance. Zoom to explore clusters.")
      ),
      
      card(
        full_screen = TRUE,
        card_header("📍 Interactive Fire Map"),
        leafletOutput("map", height = "70vh")
      )
    )
  ),
  
  # Tab 3: Seasonal / Monthly 
  nav_panel(
    "Seasonality",
    icon = icon("calendar"),
    
    layout_sidebar(
      
      sidebar = sidebar(
        width = 230,
        
        selectInput("s_state", "State",
                    choices  = c("All", sort(unique(fires$STATE))),
                    selected = "All"),
        
        sliderInput("s_year", "Year Range",
                    min   = min(fires$FIRE_YEAR),
                    max   = max(fires$FIRE_YEAR),
                    value = c(min(fires$FIRE_YEAR), max(fires$FIRE_YEAR)),
                    step  = 1, sep = ""),
        
        selectInput("s_cause", "Cause",
                    choices  = c("All", sort(unique(fires$NWCG_CAUSE_CLASSIFICATION))),
                    selected = "All")
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          full_screen = TRUE,
          card_header("Monthly Fire Count"),
          plotOutput("monthly_count", height = 280)
        ),
        
        card(
          full_screen = TRUE,
          card_header("Monthly Avg Fire Size"),
          plotOutput("monthly_size", height = 280)
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("Year × Month Heat Map  (fire count)"),
        plotOutput("heatmap", height = 320)
      )
    )
  ),
  
  # Tab 4: Top States 
  nav_panel(
    "State Ranking",
    icon = icon("ranking-star"),
    
    layout_sidebar(
      
      sidebar = sidebar(
        width = 230,
        
        sliderInput("rank_year", "Year Range",
                    min   = min(fires$FIRE_YEAR),
                    max   = max(fires$FIRE_YEAR),
                    value = c(min(fires$FIRE_YEAR), max(fires$FIRE_YEAR)),
                    step  = 1, sep = ""),
        
        selectInput("rank_metric", "Rank By",
                    choices  = c("Total Fires"    = "n",
                                 "Total Acres"    = "total_acres",
                                 "Avg Fire Size"  = "avg_size",
                                 "Largest Fire"   = "max_size"),
                    selected = "n"),
        
        sliderInput("rank_n", "Top N States",
                    min = 5, max = 20, value = 10, step = 1)
      ),
      
      layout_columns(
        col_widths = c(7, 5),
        
        card(
          full_screen = TRUE,
          card_header("State Ranking"),
          plotOutput("state_rank", height = 440)
        ),
        
        card(
          full_screen = TRUE,
          card_header("Cause Mix — Top States"),
          plotOutput("state_cause_mix", height = 440)
        )
      )
    )
  )
)

# SERVER 
server <- function(input, output, session) {
  
  # Shared reactive (Overview tab) 
  filtered <- reactive({
    df <- fires %>%
      filter(FIRE_YEAR >= input$year[1], FIRE_YEAR <= input$year[2])
    if (input$state  != "All") df <- df %>% filter(STATE == input$state)
    if (input$cause  != "All") df <- df %>% filter(NWCG_CAUSE_CLASSIFICATION == input$cause)
    if (input$size_class != "All") df <- df %>% filter(SIZE_CLASS == input$size_class)
    df
  })
  
  # Map reactive 
  map_data <- reactive({
    df <- fires %>%
      filter(FIRE_YEAR >= input$map_year[1], FIRE_YEAR <= input$map_year[2])
    if (input$map_state != "All") df <- df %>% filter(STATE == input$map_state)
    if (input$map_cause != "All") df <- df %>% filter(NWCG_CAUSE_CLASSIFICATION == input$map_cause)
    df %>% sample_n(min(input$map_sample, n()))
  })
  
  # Seasonal reactive 
  s_filtered <- reactive({
    df <- fires %>%
      filter(FIRE_YEAR >= input$s_year[1], FIRE_YEAR <= input$s_year[2],
             !is.na(MONTH_LABEL))
    if (input$s_state != "All") df <- df %>% filter(STATE == input$s_state)
    if (input$s_cause != "All") df <- df %>% filter(NWCG_CAUSE_CLASSIFICATION == input$s_cause)
    df
  })
  
  # Ranking reactive
  rank_data <- reactive({
    fires %>%
      filter(FIRE_YEAR >= input$rank_year[1], FIRE_YEAR <= input$rank_year[2]) %>%
      group_by(STATE) %>%
      summarise(
        n           = n(),
        total_acres = sum(FIRE_SIZE, na.rm = TRUE),
        avg_size    = mean(FIRE_SIZE, na.rm = TRUE),
        max_size    = max(FIRE_SIZE, na.rm = TRUE),
        .groups     = "drop"
      ) %>%
      arrange(desc(.data[[input$rank_metric]])) %>%
      slice_head(n = input$rank_n)
  })
  
  # KPI cards 
  output$kpi_row <- renderUI({
    df <- filtered()
    if (nrow(df) == 0) return(p("No data for selected filters.", style = "color:#999"))
    
    total   <- format(nrow(df), big.mark = ",")
    acres   <- format(round(sum(df$FIRE_SIZE) / 1e6, 2), big.mark = ",")
    avg_sz  <- format(round(mean(df$FIRE_SIZE), 1), big.mark = ",")
    hum_pct <- scales::percent(mean(df$NWCG_CAUSE_CLASSIFICATION == "Human", na.rm = TRUE), .1)
    big_fire <- format(max(df$FIRE_SIZE), big.mark = ",")
    n_states <- length(unique(df$STATE))
    
    kpi <- function(val, lbl, sub = "") {
      div(class = "kpi-card",
          div(class = "kpi-value", val),
          div(class = "kpi-label", lbl),
          if (nchar(sub) > 0) div(class = "kpi-sub", sub))
    }
    
    layout_columns(
      col_widths = c(2, 2, 2, 2, 2, 2),
      kpi(total,   "Total Fires"),
      kpi(paste0(acres, "M"), "Total Acres"),
      kpi(avg_sz,  "Avg Size (ac)"),
      kpi(big_fire,"Largest Fire", "acres"),
      kpi(hum_pct, "Human-Caused"),
      kpi(n_states,"States Affected")
    )
  })
  
  # Trend 
  output$trend <- renderPlot({
    df <- filtered() %>%
      group_by(FIRE_YEAR) %>%
      summarise(n = n(), total_acres = sum(FIRE_SIZE), .groups = "drop")
    
    ggplot(df, aes(FIRE_YEAR, n)) +
      geom_area(fill = FIRE_ORANGE, alpha = 0.18) +
      geom_line(color = FIRE_ORANGE, linewidth = 1.2) +
      geom_point(color = FIRE_AMBER, size = 2.5) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(breaks = pretty(df$FIRE_YEAR, 8)) +
      labs(x = NULL, y = "Fire Count") +
      gg_fire_theme()
  }, bg = SMOKE_GREY)
  
  # Cause bar 
  output$cause_bar <- renderPlot({
    df <- filtered() %>%
      count(NWCG_CAUSE_CLASSIFICATION) %>%
      arrange(n)
    
    ggplot(df, aes(reorder(NWCG_CAUSE_CLASSIFICATION, n), n,
                   fill = NWCG_CAUSE_CLASSIFICATION)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(
        values = setNames(
          colorRampPalette(c(FIRE_RED, FIRE_ORANGE, FIRE_AMBER))(nrow(df)),
          df$NWCG_CAUSE_CLASSIFICATION
        )
      ) +
      scale_y_continuous(labels = comma) +
      coord_flip() +
      labs(x = NULL, y = "Count") +
      gg_fire_theme()
  }, bg = SMOKE_GREY)
  
  # Size histogram 
  output$size_hist <- renderPlot({
    ggplot(filtered(), aes(FIRE_SIZE)) +
      geom_histogram(bins = 45, fill = FIRE_ORANGE, color = SMOKE_DARK,
                     linewidth = 0.2, alpha = 0.85) +
      scale_x_log10(labels = comma) +
      labs(x = "Fire Size — acres (log)", y = "Count") +
      gg_fire_theme()
  }, bg = SMOKE_GREY)
  
  # Size class 
  output$size_class_plot <- renderPlot({
    df <- filtered() %>%
      count(SIZE_CLASS) %>%
      filter(!is.na(SIZE_CLASS)) %>%
      mutate(pct = n / sum(n))
    
    ggplot(df, aes(SIZE_CLASS, pct, fill = SIZE_CLASS)) +
      geom_col(show.legend = FALSE, width = 0.7) +
      geom_text(aes(label = scales::percent(pct, 1)),
                hjust = -0.1, color = TEXT_LIGHT, size = 3.2) +
      scale_y_continuous(labels = percent, expand = expansion(mult = c(0, .2))) +
      scale_fill_manual(
        values = colorRampPalette(c("#FF6B35","#FFBE0B","#E63946","#C77DFF","#4CC9F0","#2EC4B6","#8338EC"))(7)
      ) +
      coord_flip() +
      labs(x = NULL, y = "% of fires") +
      gg_fire_theme()
  }, bg = SMOKE_GREY)
  
  # Download 
  output$dl_csv <- downloadHandler(
    filename = function() paste0("wildfires_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(filtered(), file, row.names = FALSE)
  )
  
  # Overview spatial risk map
  output$overview_map <- renderLeaflet({
    df  <- filtered() %>% sample_n(min(1500, n()))
    pal <- colorNumeric("YlOrRd", df$risk_index)
    
    leaflet(df) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addCircleMarkers(
        lng         = ~LONGITUDE,
        lat         = ~LATITUDE,
        radius      = ~pmin(7, 2 + log1p(FIRE_SIZE) * 0.45),
        color       = ~pal(risk_index),
        stroke      = FALSE,
        fillOpacity = 0.65,
        popup       = ~paste0(
          "<div style='font-family:DM Sans,sans-serif;color:#eee;background:#23232F;padding:8px;border-radius:6px'>",
          "<b style='color:#FF6B35'>", STATE, " — ", FIRE_YEAR, "</b><br>",
          "Size: <b>", format(FIRE_SIZE, big.mark = ","), " ac</b><br>",
          "Cause: ", NWCG_CAUSE_CLASSIFICATION, "</div>"
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~risk_index,
                title = "Risk Index", opacity = 0.85,
                labFormat = labelFormat(digits = 1))
  })
  
  # Map
  output$map <- renderLeaflet({
    df  <- map_data()
    col_var <- input$map_color
    pal <- colorNumeric("YlOrRd", df[[col_var]])
    
    leaflet(df) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addCircleMarkers(
        lng         = ~LONGITUDE,
        lat         = ~LATITUDE,
        radius      = ~pmin(8, 2 + log1p(FIRE_SIZE) * 0.5),
        color       = ~pal(get(col_var)),
        stroke      = FALSE,
        fillOpacity = 0.7,
        popup       = ~paste0(
          "<div style='font-family:DM Sans,sans-serif;color:#eee;background:#23232F;padding:8px;border-radius:6px'>",
          "<b style='color:#FF6B35'>", STATE, " — ", FIRE_YEAR, "</b><br>",
          "Size: <b>", format(FIRE_SIZE, big.mark = ","), " ac</b><br>",
          "Cause: ", NWCG_CAUSE_CLASSIFICATION, "</div>"
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = df[[col_var]],
                title    = ifelse(col_var == "risk_index", "Risk Index", "Fire Size"),
                opacity  = 0.9,
                labFormat = labelFormat(digits = 1))
  })
  
  # Monthly count 
  output$monthly_count <- renderPlot({
    req("MONTH_LABEL" %in% names(fires))
    
    df <- s_filtered() %>%
      count(MONTH_LABEL) %>%
      complete(MONTH_LABEL = factor(month.abb, levels = month.abb), fill = list(n = 0))
    
    ggplot(df, aes(MONTH_LABEL, n, group = 1)) +
      geom_area(fill = FIRE_ORANGE, alpha = 0.22) +
      geom_line(color = FIRE_ORANGE, linewidth = 1.3) +
      geom_point(color = FIRE_AMBER, size = 3) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Fire Count") +
      gg_fire_theme()
  }, bg = SMOKE_GREY)
  
  # Monthly avg size 
  output$monthly_size <- renderPlot({
    req("MONTH_LABEL" %in% names(fires))
    
    df <- s_filtered() %>%
      group_by(MONTH_LABEL) %>%
      summarise(avg = mean(FIRE_SIZE, na.rm = TRUE), .groups = "drop") %>%
      complete(MONTH_LABEL = factor(month.abb, levels = month.abb),
               fill = list(avg = 0))
    
    ggplot(df, aes(MONTH_LABEL, avg, group = 1)) +
      geom_col(fill = FIRE_RED, alpha = 0.75, width = 0.7) +
      geom_line(color = FIRE_AMBER, linewidth = 1.2) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Avg Size (acres)") +
      gg_fire_theme()
  }, bg = SMOKE_GREY)
  
  # Year × Month heatmap 
  output$heatmap <- renderPlot({
    req("MONTH_LABEL" %in% names(fires))
    
    df <- s_filtered() %>%
      group_by(FIRE_YEAR, MONTH_LABEL) %>%
      summarise(n = n(), .groups = "drop")
    
    ggplot(df, aes(MONTH_LABEL, factor(FIRE_YEAR), fill = n)) +
      geom_tile(color = SMOKE_DARK, linewidth = 0.4) +
      scale_fill_gradient(low = "#1A1A2E", high = FIRE_ORANGE,
                          labels = comma, name = "Fires") +
      labs(x = NULL, y = NULL) +
      gg_fire_theme(base_size = 11) +
      theme(axis.text.y = element_text(size = 8))
  }, bg = SMOKE_GREY)
  
  # State ranking bar 
  output$state_rank <- renderPlot({
    df  <- rank_data()
    met <- input$rank_metric
    
    label_fn <- switch(met,
                       n           = comma,
                       total_acres = function(x) paste0(round(x/1e6,1), "M"),
                       avg_size    = comma,
                       max_size    = comma
    )
    
    ggplot(df, aes(reorder(STATE, .data[[met]]), .data[[met]])) +
      geom_col(fill = FIRE_ORANGE, alpha = 0.85, width = 0.7) +
      geom_text(aes(label = label_fn(.data[[met]])),
                hjust = -0.1, color = TEXT_LIGHT, size = 3.3) +
      scale_y_continuous(labels = label_fn,
                         expand  = expansion(mult = c(0, .2))) +
      coord_flip() +
      labs(x = NULL, y = met) +
      gg_fire_theme()
  }, bg = SMOKE_GREY)
  
  # State × cause stacked bar 
  output$state_cause_mix <- renderPlot({
    states_keep <- rank_data()$STATE
    
    df <- fires %>%
      filter(FIRE_YEAR >= input$rank_year[1],
             FIRE_YEAR <= input$rank_year[2],
             STATE %in% states_keep) %>%
      count(STATE, NWCG_CAUSE_CLASSIFICATION) %>%
      group_by(STATE) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup()
    
    causes <- unique(df$NWCG_CAUSE_CLASSIFICATION)
    cols   <- setNames(
      colorRampPalette(c(FIRE_RED, FIRE_ORANGE, FIRE_AMBER, "#4CC9F0", "#8338EC"))(length(causes)),
      causes
    )
    
    ggplot(df, aes(pct, reorder(STATE, pct), fill = NWCG_CAUSE_CLASSIFICATION)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = cols, name = "Cause") +
      scale_x_continuous(labels = percent) +
      labs(x = "Share of fires", y = NULL) +
      gg_fire_theme() +
      theme(legend.position = "bottom",
            legend.key.size  = unit(0.5, "lines"),
            legend.text      = element_text(size = 8))
  }, bg = SMOKE_GREY)
}

shinyApp(ui, server)


  
  
  
  