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
  
  # Site Pairs tab ----
  nav_panel(
    "Site Pairs",
    icon = icon("code-compare"),
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        bg    = "#f0f4f8",
        
        tags$h6("Pair Selection", class = "text-muted fw-bold mt-1 mb-2"),
        
        selectInput("pair_id", "Site Pair",
                    choices  = setNames(
                      sapply(SITE_PAIRS, `[[`, "id"),
                      sapply(SITE_PAIRS, `[[`, "label")
                    ),
                    selected = "pair1"),
        
        uiOutput("pair_site_labels_ui"),
        
        hr(),
        
        selectInput("pair_param", "Parameter",
                    choices  = PARAMETERS,
                    selected = "pH"),
        
        radioButtons("pair_season", "Season",
                     choices  = SEASONS,
                     selected = "All",
                     inline   = TRUE),
        
        hr(),
        
        tags$h6("Date Range", class = "text-muted fw-bold"),
        uiOutput("pair_date_slider_ui"),
        
        hr(),
        
        uiOutput("pair_description_ui")
      ),
      
      div(
        # Value boxes: mean difference and per-site means
        layout_columns(
          col_widths = c(4, 4, 4),
          uiOutput("pair_vbox_a"),
          uiOutput("pair_vbox_b"),
          uiOutput("pair_vbox_diff")
        ),
        
        tags$br(),
        
        # Time series comparison and box plots side by side
        layout_columns(
          col_widths = c(7, 5),
          card(
            full_screen = TRUE,
            card_header(icon("chart-line"), " Time Series Comparison"),
            plotlyOutput("pair_timeseries", height = "380px")
          ),
          card(
            full_screen = TRUE,
            card_header(icon("box"), " Distribution Comparison"),
            plotlyOutput("pair_boxplot", height = "380px")
          )
        ),
        
        tags$br(),
        
        # Paired difference over time + scatter of A vs B
        layout_columns(
          col_widths = c(6, 6),
          card(
            full_screen = TRUE,
            card_header(icon("arrow-right-arrow-left"), " Paired Difference Over Time (A − B)"),
            plotlyOutput("pair_difference", height = "360px")
          ),
          card(
            full_screen = TRUE,
            card_header(icon("circle-nodes"), " Site A vs Site B")
            # plotlyOutput("pair_scatter", height = "360px")
          )
        ),
        
        tags$br(),
        
        # Per-parameter mean difference bar chart
        card(
          full_screen = TRUE,
          card_header(icon("chart-bar"), " Mean Difference by Parameter (A − B)"),
          plotlyOutput("pair_param_diff", height = "360px")
        ),
        
        tags$br()
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