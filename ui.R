ui <- page_navbar(
  title = tags$span(
    tags$a(
      href = "https://emcguyana.com/", target = "_blank",
      tags$img(
        src    = "https://i0.wp.com/www.emcguyana.com/wp-content/uploads/2025/09/icon.png?fit=500%2C500&ssl=1",
        height = "28px",
        style  = "margin-right:8px; vertical-align:middle;"
      )
    ),
    "Mahaica River Water Quality Dashboard"
  ),
  theme = bs_theme(
    bootswatch   = "flatly",
    primary      = "#2c7bb6",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  navbar_options = navbar_options(bg = "#1a3a5c", theme = "dark"),
  
  # Explorer tab ----
  nav_panel(
    "Explorer",
    icon = icon("map"),
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        bg    = "#f0f4f8",
        
        tags$h6("Filters", class = "text-muted fw-bold mt-1 mb-2"),
        
        selectInput("param", "Parameter",
                    choices  = PARAMETERS,
                    selected = "pH"),
        
        radioButtons("season", "Season",
                     choices  = SEASONS,
                     selected = "All",
                     inline   = TRUE),
        
        selectInput("site_filter", "Sites",
                    choices  = c("All sites", sort(unique(readings$SiteID))),
                    selected = "All sites",
                    multiple = FALSE),
        
        hr(),
        
        tags$h6("Date Range", class = "text-muted fw-bold"),
        uiOutput("date_slider_ui"),
        
        hr(),
        
        tags$h6("Map Style", class = "text-muted fw-bold"),
        radioButtons("map_base", NULL,
                     choices  = c("Satellite" = "Esri.WorldImagery",
                                  "Street"    = "OpenStreetMap",
                                  "Terrain"   = "Esri.WorldTopoMap"),
                     selected = "Esri.WorldImagery"),
        
        checkboxInput("show_labels",    "Show site labels",    value = FALSE),
        checkboxInput("show_otters",    "Show otter sightings", value = FALSE),
        checkboxInput("show_waterways", "Show river features",  value = TRUE),
        
        hr(),
        
        tags$small(
          class = "text-muted",
          icon("circle-info"), " Click a map marker to select a site."
        ),
        
        if (!is.null(otter_load_error))
          tags$small(
            class = "text-danger mt-2 d-block",
            icon("triangle-exclamation"),
            " Otter data unavailable: ", otter_load_error
          )
      ),
      
      # Main content panels
      div(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          uiOutput("vbox_mean"),
          uiOutput("vbox_min"),
          uiOutput("vbox_max"),
          uiOutput("vbox_sites")
        ),
        
        tags$br(),
        
        layout_columns(
          col_widths = c(6, 6),
          card(
            full_screen = TRUE,
            card_header(icon("map-location-dot"), " Sampling Locations"),
            leafletOutput("map", height = "400px")
          ),
          card(
            full_screen = TRUE,
            card_header(icon("chart-line"), " Longitudinal Profile (Mouth \u2192 Headwaters)"),
            plotlyOutput("longitudinal_plot", height = "400px")
          )
        ),
        
        tags$br(),
        
        layout_columns(
          col_widths = c(8, 4),
          card(
            full_screen = TRUE,
            card_header(icon("chart-area"), " Time Series by Site"),
            plotlyOutput("timeseries_plot", height = "380px")
          ),
          card(
            full_screen = TRUE,
            card_header(icon("box"), " Season Comparison"),
            plotlyOutput("boxplot_season", height = "380px")
          )
        ),
        
        tags$br(),
        
        layout_columns(
          col_widths = c(7, 5),
          card(
            full_screen = TRUE,
            card_header(icon("th"), " Parameter Heatmap Across Sites & Time"),
            plotlyOutput("heatmap_plot", height = "380px")
          ),
          card(
            full_screen = TRUE,
            card_header(icon("circle-nodes"), " Parameter Correlations"),
            uiOutput("corr_param_ui"),
            plotlyOutput("scatter_corr", height = "320px")
          )
        ),
        
        tags$br()
      )
    )
  ),
  
  # Site Detail tab ----
  nav_panel(
    "Site Detail",
    icon = icon("location-dot"),
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        bg    = "#f0f4f8",
        selectInput("detail_site", "Select Site",
                    choices  = sort(unique(readings$SiteID)),
                    selected = "M1"),
        radioButtons("detail_season", "Season",
                     choices  = SEASONS,
                     selected = "All",
                     inline   = TRUE),
        hr(),
        uiOutput("site_info_card")
      ),
      div(
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(icon("chart-line"), " All Parameters Over Time"),
            plotlyOutput("detail_timeseries", height = "380px")
          ),
          card(
            card_header(icon("chart-bar"), " Latest Sampling Summary"),
            plotlyOutput("detail_radar", height = "380px")
          )
        ),
        tags$br(),
        card(
          card_header(icon("table"), " Raw Data"),
          DTOutput("detail_table")
        )
      )
    )
  ),
  
  # Data tab ----
  nav_panel(
    "Data",
    icon = icon("table"),
    card(
      card_header("All Readings"),
      DTOutput("full_table")
    )
  )
)