server <- function(input, output, session) {
  # Date slider ----
  output$date_slider_ui <- renderUI({
    dates <- sort(unique(readings$Date_parsed))
    sliderInput(
      "date_range",
      NULL,
      min        = min(dates),
      max        = max(dates),
      value      = c(min(dates), max(dates)),
      timeFormat = "%b %Y",
      step       = 30
    )
  })
  
  # Correlation parameter picker ----
  output$corr_param_ui <- renderUI({
    selectInput(
      "corr_param2",
      "Correlate with:",
      choices  = setdiff(PARAMETERS, input$param),
      selected = if ("Salinity (PPT)" != input$param)
        "Salinity (PPT)"
      else
        "pH"
    )
  })
  
  ## Filtered Reactives ----
  
  # Debounce all inputs by 400ms so charts don't re-render on every
  # intermediate value during rapid slider drags or dropdown changes
  param_d         <- reactive(input$param)         %>% debounce(400)
  season_d        <- reactive(input$season)        %>% debounce(400)
  site_filter_d   <- reactive(input$site_filter)   %>% debounce(400)
  date_range_d    <- reactive(input$date_range)    %>% debounce(400)
  corr_param2_d   <- reactive(input$corr_param2)   %>% debounce(400)
  detail_site_d   <- reactive(input$detail_site)   %>% debounce(400)
  detail_season_d <- reactive(input$detail_season) %>% debounce(400)
  
  # Map bounds — updated on zoom/pan with 600ms debounce to avoid thrashing
  map_bounds <- reactive({
    input$map_bounds
  }) %>% debounce(600)
  
  # Sites whose coordinates fall within the current map extent
  visible_sites <- reactive({
    bounds <- map_bounds()
    if (is.null(bounds))
      return(unique(locations$SiteID))
    locations %>%
      filter(
        DDLat >= bounds$south,
        DDLat <= bounds$north,
        DDLon >= bounds$west,
        DDLon <= bounds$east
      ) %>%
      pull(SiteID)
  })
  
  fdata <- reactive({
    req(date_range_d())
    d <- readings %>%
      filter(
        Date_parsed >= date_range_d()[1],
        Date_parsed <= date_range_d()[2],
        SiteID %in% visible_sites()
      )
    if (season_d() != "All")
      d <- d %>% filter(Season == season_d())
    if (site_filter_d() != "All sites")
      d <- d %>% filter(SiteID == site_filter_d())
    d
  })
  
  param_data <- reactive({
    fdata() %>% filter(Attribute == param_d())
  })
  
  ## Map Observers ----
  
  # Swap basemap tile without resetting view or clearing markers
  observeEvent(input$map_base, {
    leafletProxy("map") %>% addProviderTiles(input$map_base)
  })
  
  # Click marker → select/deselect site in dropdown
  observeEvent(input$map_marker_click, {
    clicked <- input$map_marker_click$id
    if (!is.null(clicked)) {
      current <- isolate(input$site_filter)
      new_val <- if (identical(current, clicked))
        "All sites"
      else
        clicked
      updateSelectInput(session, "site_filter", selected = new_val)
    }
  })
  
  # Dropdown selection → zoom map to site (or reset to full extent)
  observeEvent(input$site_filter, {
    if (input$site_filter == "All sites") {
      leafletProxy("map") %>%
        setView(lng = MAP_CENTER$lng,
                lat = MAP_CENTER$lat,
                zoom = MAP_CENTER$zoom)
    } else {
      loc <- locations %>% filter(SiteID == input$site_filter)
      if (nrow(loc) > 0)
        leafletProxy("map") %>%
        setView(lng = loc$DDLon,
                lat = loc$DDLat,
                zoom = SITE_ZOOM)
    }
  }, ignoreInit = TRUE)
  
  ## Value Boxes ----
  
  make_vbox <- function(label, value, icon_name, color) {
    value_box(
      title    = label,
      value    = value,
      showcase = icon(icon_name, style = "font-size:1rem;"),
      theme    = color,
      height   = "90px"
    )
  }
  
  output$vbox_mean <- renderUI({
    v <- mean(param_data()$Value, na.rm = TRUE)
    make_vbox(paste("Mean", param_d()),
              paste0(round(v, 2), " ", param_unit(param_d())),
              "chart-bar",
              "primary")
  })
  output$vbox_min <- renderUI({
    v <- min(param_data()$Value, na.rm = TRUE)
    make_vbox("Minimum",
              paste0(round(v, 2), " ", param_unit(param_d())),
              "arrow-down",
              "info")
  })
  output$vbox_max <- renderUI({
    v <- max(param_data()$Value, na.rm = TRUE)
    make_vbox("Maximum",
              paste0(round(v, 2), " ", param_unit(param_d())),
              "arrow-up",
              "warning")
  })
  output$vbox_sites <- renderUI({
    n <- n_distinct(param_data()$SiteID)
    make_vbox("Active Sites", n, "map-marker-alt", "success")
  })
  
  ## Map ----
  
  # Full summary across ALL sites — used to fix colour scale domain so colours
  # don't shift when the map extent changes
  site_summary_full <- reactive({
    req(date_range_d())
    d <- readings %>%
      filter(
        Attribute == param_d(),
        Date_parsed >= date_range_d()[1],
        Date_parsed <= date_range_d()[2]
      )
    if (season_d() != "All")
      d <- d %>% filter(Season == season_d())
    if (site_filter_d() != "All sites")
      d <- d %>% filter(SiteID == site_filter_d())
    d %>%
      group_by(SiteID,
               DDLat,
               DDLon,
               Name,
               Habitat,
               Description,
               Distance_from_mouth) %>%
      summarise(
        mean_val = mean(Value, na.rm = TRUE),
        n_obs    = n(),
        .groups  = "drop"
      )
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addLayersControl(
        overlayGroups = c("Important Places"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lng = MAP_CENTER$lng,
              lat = MAP_CENTER$lat,
              zoom = MAP_CENTER$zoom) %>%
      htmlwidgets::onRender(
        "
        function(el, x) {
          var map = this;
          function reportBounds() {
            var b = map.getBounds();
            Shiny.setInputValue('map_bounds', {
              north: b.getNorth(), south: b.getSouth(),
              east:  b.getEast(),  west:  b.getWest()
            });
          }
          map.on('moveend', reportBounds);
          reportBounds();
        }
      "
      )
  })
  
  observe({
    ss <- site_summary_full()
    req(nrow(ss) > 0)
    
    pal <- colorNumeric("RdYlBu", domain = ss$mean_val, reverse = TRUE)
    
    proxy <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls() %>%
      clearGroup("waterways")
    
    # River features drawn first so they sit underneath all point markers
    if (isTRUE(input$show_waterways) &&
        !is.null(waterways_geojson)) {
      proxy %>%
        addGeoJSON(
          geojson    = waterways_geojson,
          group      = "waterways",
          color      = "#4A90D9",
          weight     = 2,
          opacity    = 0.7,
          fill       = FALSE
        )
    }
    
    proxy %>%
      addLabelOnlyMarkers(
        data = important_places,
        label = ~ Name,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "font-size" = "10px",
            "font-weight" = "400",
            "color" = "#D3D3D3",
            # "background-color" = "rgba(255,255,255,0.65)",
            "border" = "none",
            "padding" = "1px 3px",
            "border-radius" = "2px",
            "text-shadow" = "none"
          )
        ),
        group = "Important Places"
      )
    
    proxy %>%
      addCircleMarkers(
        data        = ss,
        lng         = ~ DDLon,
        lat         = ~ DDLat,
        layerId     = ~ SiteID,
        radius      = 8,
        fillColor   = ~ pal(mean_val),
        color       = "white",
        weight      = 1.5,
        fillOpacity = 0.85,
        label       = ~ lapply(
          paste0(
            "<b>",
            SiteID,
            " \u2013 ",
            Name,
            "</b><br>",
            "<i>",
            Habitat,
            "</i><br>",
            "<b>",
            param_d(),
            ":</b> ",
            round(mean_val, 2),
            " ",
            param_unit(param_d()),
            "<br>",
            "<b>Observations:</b> ",
            n_obs,
            "<br>",
            "<b>Distance from mouth:</b> ",
            round(Distance_from_mouth, 1),
            " km<br><br>",
            "<small>",
            Description,
            "</small>"
          ),
          htmltools::HTML
        ),
        labelOptions = labelOptions(
          style     = TOOLTIP_LABEL_STYLE,
          direction = "top",
          textsize  = "12px"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = ss$mean_val,
        title    = paste0(param_d(), "<br>(", param_unit(param_d()), ")"),
        opacity  = 0.85
      )
    
    # Otter sightings layer
    proxy %>% clearGroup("otters")
    if (isTRUE(input$show_otters) &&
        !is.null(otter_data) && nrow(otter_data) > 0) {
      od <- otter_data %>%
        mutate(
          fill_col = dplyr::recode(
            Species,
            "Giant River Otter" = "#A63600",
            "Neotropical Otter" = "#4C7300",
            .default = "#888888"
          )
        )
      
      icons     <- lapply(od$fill_col, function(col)
        makeIcon(
          iconUrl = make_diamond_icon(col),
          iconWidth = 18,
          iconHeight = 18,
          iconAnchorX = 9,
          iconAnchorY = 9
        ))
      icon_list <- do.call(iconList, icons)
      
      proxy %>%
        addMarkers(
          data         = od,
          lng          = ~ Long,
          lat          = ~ Lat,
          group        = "otters",
          icon         = icon_list,
          label        = ~ lapply(
            paste0(
              "<b>",
              Species,
              "</b><br>",
              "<b>Date:</b> ",
              Date,
              "<br>",
              "<b>Count:</b> ",
              No_OttersSighted,
              "<br>",
              "<b>Observer:</b> ",
              ObservedBy,
              "<br>",
              ifelse(
                !is.na(Notes) & Notes != "",
                paste0("<b>Notes:</b> ", Notes),
                ""
              )
            ),
            htmltools::HTML
          ),
          labelOptions = labelOptions(style     = TOOLTIP_LABEL_STYLE, direction = "top")
        ) %>%
        addLegend(
          position = "bottomleft",
          colors   = unname(OTTER_COLS),
          labels   = names(OTTER_COLS),
          title    = "Otter Species",
          opacity  = 0.9
        )
    }
    
    if (isTRUE(input$show_labels)) {
      proxy %>%
        addLabelOnlyMarkers(
          data         = ss,
          lng          = ~ DDLon,
          lat          = ~ DDLat,
          label        = ~ SiteID,
          labelOptions = labelOptions(
            noHide    = TRUE,
            direction = "top",
            textsize  = "11px",
            style     = list(
              "font-weight"      = "bold",
              "background-color" = "rgba(255,255,255,0.6)",
              "border"           = "none",
              "box-shadow"       = "none",
              "padding"          = "1px 4px"
            )
          )
        )
    }
  })
  
  ## Longitudinal Profile ----
  
  output$longitudinal_plot <- renderPlotly({
    if (season_d() == "All") {
      d_season <- param_data() %>%
        group_by(SiteID, Distance_from_mouth, Season) %>%
        summarise(mean_val = mean(Value, na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(Distance_from_mouth)
      
      d_all <- param_data() %>%
        group_by(SiteID, Distance_from_mouth) %>%
        summarise(
          mean_val = mean(Value, na.rm = TRUE),
          sd_val   = sd(Value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(Distance_from_mouth)
      
      p <- ggplot() +
        geom_ribbon(
          data = d_all,
          aes(
            x = Distance_from_mouth,
            ymin = mean_val - sd_val,
            ymax = mean_val + sd_val
          ),
          fill = "grey60",
          alpha = 0.2,
          color = NA
        ) +
        geom_line(
          data  = d_season,
          aes(
            x = Distance_from_mouth,
            y = mean_val,
            color = Season,
            group = Season
          ),
          linewidth = 0.9
        ) +
        geom_point(
          data = d_season,
          aes(x = Distance_from_mouth, y = mean_val, color = Season),
          size = 2.5
        ) +
        geom_line(
          data  = d_all,
          aes(x = Distance_from_mouth, y = mean_val, group = 1),
          color = "grey20",
          linewidth = 1,
          linetype = "dashed"
        ) +
        geom_point(
          data = d_all,
          aes(x = Distance_from_mouth, y = mean_val, group = 1),
          color = "grey20",
          size = 2
        ) +
        scale_color_manual(values = SEASON_COLS, na.value = "grey50") +
        labs(
          x = "Distance from River Mouth (km)",
          y = paste0(param_d(), " (", param_unit(param_d()), ")"),
          color = NULL
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "top")
    } else {
      d <- param_data() %>%
        group_by(SiteID, Distance_from_mouth) %>%
        summarise(
          mean_val = mean(Value, na.rm = TRUE),
          sd_val   = sd(Value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(Distance_from_mouth)
      
      season_col <- SEASON_COLS[season_d()]
      
      p <- ggplot(d, aes(x = Distance_from_mouth, y = mean_val)) +
        geom_ribbon(
          aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val),
          fill = season_col,
          alpha = 0.2,
          color = NA
        ) +
        geom_line(color = season_col, linewidth = 0.9) +
        geom_point(color = season_col, size = 2.5) +
        labs(x = "Distance from River Mouth (km)",
             y = paste0(param_d(), " (", param_unit(param_d()), ")")) +
        theme_minimal(base_size = 11)
    }
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(hovermode = "x unified")
  })
  
  ## Time Series ----
  
  output$timeseries_plot <- renderPlotly({
    show_all_sites   <- site_filter_d() == "All sites"
    show_all_seasons <- season_d() == "All"
    
    if (show_all_sites) {
      d_all <- param_data() %>%
        group_by(Date_parsed) %>%
        summarise(Value = mean(Value, na.rm = TRUE),
                  .groups = "drop")
      
      p <- ggplot(d_all, aes(x = Date_parsed, y = Value, group = 1)) +
        geom_line(
          color = "grey20",
          linewidth = 1.2,
          linetype = "dashed"
        ) +
        geom_point(color = "grey20", size = 2.5) +
        labs(x = NULL,
             y = paste0(param_d(), " (", param_unit(param_d()), ")")) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "none")
    } else {
      d <- param_data() %>%
        group_by(SiteID, Date_parsed, Season) %>%
        summarise(Value = mean(Value, na.rm = TRUE),
                  .groups = "drop")
      
      if (show_all_seasons) {
        # Per-season points from raw data, overall mean as dashed black line
        d_raw <- param_data() %>%
          filter(SiteID == site_filter_d(), !is.na(Season))
        d_overall <- d_raw %>%
          group_by(Date_parsed) %>%
          summarise(Value = mean(Value, na.rm = TRUE),
                    .groups = "drop")
        
        p <- ggplot() +
          geom_line(
            data = d_raw,
            aes(
              x = Date_parsed,
              y = Value,
              color = Season,
              group = Season
            ),
            linewidth = 0.6,
            alpha = 0.8
          ) +
          geom_point(
            data = d_raw,
            aes(
              x = Date_parsed,
              y = Value,
              color = Season
            ),
            size = 2,
            alpha = 0.9
          ) +
          geom_line(
            data = d_overall,
            aes(
              x = Date_parsed,
              y = Value,
              group = 1
            ),
            color = "grey20",
            linewidth = 1.2,
            linetype = "dashed"
          ) +
          geom_point(
            data = d_overall,
            aes(
              x = Date_parsed,
              y = Value,
              group = 1
            ),
            color = "grey20",
            size = 2.5
          ) +
          scale_color_manual(values = SEASON_COLS, na.value = "grey50") +
          labs(
            x = NULL,
            y = paste0(param_d(), " (", param_unit(param_d()), ")"),
            color = NULL
          ) +
          theme_minimal(base_size = 11) +
          theme(legend.position = "top")
      } else {
        line_col <- SEASON_COLS[season_d()]
        p <- ggplot(d, aes(
          x = Date_parsed,
          y = Value,
          group = SiteID
        )) +
          geom_line(alpha = 0.7,
                    linewidth = 0.7,
                    color = line_col) +
          geom_point(size = 2,
                     alpha = 0.8,
                     color = line_col) +
          labs(x = NULL,
               y = paste0(param_d(), " (", param_unit(param_d()), ")")) +
          theme_minimal(base_size = 11) +
          theme(legend.position = "none")
      }
    }
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(hovermode = "x unified")
  })
  
  ## Season Box Plot ----
  
  output$boxplot_season <- renderPlotly({
    req(date_range_d())
    d <- readings %>%
      filter(
        Attribute == param_d(),
        !is.na(Season),
        Date_parsed >= date_range_d()[1],
        Date_parsed <= date_range_d()[2]
      )
    if (site_filter_d() != "All sites")
      d <- d %>% filter(SiteID == site_filter_d())
    
    p <- ggplot(d, aes(x = Season, y = Value, fill = Season)) +
      geom_boxplot(
        alpha = 0.7,
        outlier.shape = 21,
        outlier.size = 2
      ) +
      geom_jitter(width = 0.15,
                  alpha = 0.3,
                  size = 1) +
      scale_fill_manual(values = SEASON_COLS) +
      labs(x = NULL,
           y = paste0(param_d(), " (", param_unit(param_d()), ")")) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  ## Heatmap ----
  
  output$heatmap_plot <- renderPlotly({
    # Intentionally ignores map extent so all sites remain visible for context
    req(date_range_d())
    d <- readings %>%
      filter(
        Attribute == param_d(),
        Date_parsed >= date_range_d()[1],
        Date_parsed <= date_range_d()[2]
      )
    if (season_d() != "All")
      d <- d %>% filter(Season == season_d())
    if (site_filter_d() != "All sites")
      d <- d %>% filter(SiteID == site_filter_d())
    
    d <- d %>%
      group_by(SiteID, YearMon) %>%
      summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(SiteID = factor(SiteID, levels = paste0("M", 1:25)))
    
    p <- ggplot(d, aes(x = YearMon, y = SiteID, fill = Value)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_distiller(
        palette = "RdYlBu",
        direction = 1,
        name = param_unit(param_d())
      ) +
      labs(x = NULL, y = "Site") +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right")
    
    ggplotly(p) %>% layout(xaxis = list(tickangle = -45))
  })
  
  ## Correlation Scatter ----
  
  output$scatter_corr <- renderPlotly({
    req(corr_param2_d())
    
    d_wide <- fdata() %>%
      filter(Attribute %in% c(param_d(), corr_param2_d())) %>%
      select(SiteID, Date_parsed, Season, Attribute, Value) %>%
      pivot_wider(
        names_from = Attribute,
        values_from = Value,
        values_fn = mean
      ) %>%
      rename(x_val = all_of(param_d()),
             y_val = all_of(corr_param2_d())) %>%
      filter(!is.na(x_val), !is.na(y_val)) %>%
      mutate(tooltip = paste0("Site: ", SiteID, "<br>Date: ", Date_parsed))
    
    # Calculate Pearson correlation
    r <- cor(d_wide$x_val, d_wide$y_val, method = "pearson")
    
    # Format correlation coefficient
    r_label <- paste0("r = ", round(r, 2))
    
    p <- ggplot(d_wide, aes(x = x_val, y = y_val, color = Season)) +
      geom_point(alpha = 0.65, size = 2.5) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        linewidth = 0.8,
        aes(group = Season),
        alpha = 0.1
      ) +
      scale_color_manual(values = SEASON_COLS, na.value = "grey60") +
      labs(
        x = paste0(param_d(), " (", param_unit(param_d()), ")"),
        y = paste0(corr_param2_d(), " (", param_unit(corr_param2_d()), ")"),
        color = NULL
      ) +
      theme_minimal(base_size = 11)
    
    if (season_d() == "All") {
      p <- p +
        geom_smooth(
          data = d_wide,
          aes(x = x_val, y = y_val, group = 1),
          method = "lm",
          se = TRUE,
          color = "grey20",
          fill = "grey70",
          linewidth = 1,
          linetype = "dashed",
          alpha = 0.15,
          inherit.aes = FALSE
        )
    }
    
    plot <- ggplotly(p, tooltip = c("colour", "x", "y", "text")) %>%
      style(text = d_wide$tooltip, traces = 1)
    
    # add correlation annotation
    plot <- plot %>%
      layout(annotations = list(
        list(
          x = 0.98,
          y = 0.98,
          xref = "paper",
          yref = "paper",
          text = paste0("<b>r = ", round(r, 2), "</b>"),
          showarrow = FALSE,
          xanchor = "right",
          yanchor = "top",
          font = list(size = 14)
        )
      ))
    
    plot
  })
  
  ## Site Detail Tab ----
  
  detail_data <- reactive({
    d <- readings %>% filter(SiteID == detail_site_d())
    if (detail_season_d() != "All")
      d <- d %>% filter(Season == detail_season_d())
    d
  })
  
  output$site_info_card <- renderUI({
    loc <- locations %>% filter(SiteID == detail_site_d())
    req(nrow(loc) > 0)
    tagList(
      tags$h6(loc$Name, class = "fw-bold"),
      tags$p(tags$small(loc$Description)),
      tags$hr(),
      tags$table(class = "table table-sm table-borderless", tags$tbody(
        tags$tr(tags$td(icon("tree"), " Habitat"), tags$td(tags$small(loc$Habitat))),
        tags$tr(tags$td(icon("ruler"), " Dist. from mouth"), tags$td(tags$small(
          round(loc$Distance_from_mouth, 1), " km"
        ))),
        tags$tr(tags$td(icon("map-pin"), " Coordinates"), tags$td(tags$small(
          round(loc$DDLat, 4), "N,", round(loc$DDLon, 4), "W"
        )))
      ))
    )
  })
  
  output$detail_timeseries <- renderPlotly({
    d <- detail_data() %>%
      filter(!Attribute %in% c("Cond (uS/cm)", "TDS (PPM)"))
    
    p <- ggplot(d,
                aes(
                  x = Date_parsed,
                  y = Value,
                  color = Attribute,
                  group = Attribute
                )) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2, aes(shape = Season)) +
      scale_shape_manual(values = c("Dry" = 17, "Wet" = 16)) +
      facet_wrap(~ Attribute, scales = "free_y", ncol = 2) +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_minimal(base_size = 10) +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold", size = 9))
    
    ggplotly(p)
  })
  
  output$detail_radar <- renderPlotly({
    d <- detail_data()
    latest <- d %>%
      filter(!is.na(Value)) %>%
      arrange(desc(Date_parsed)) %>%
      slice(1) %>%
      pull(Date_parsed)
    
    d_latest <- d %>%
      filter(Date_parsed == latest) %>%
      group_by(Attribute) %>%
      summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(pct_rank = percent_rank(Value))
    
    p <- ggplot(d_latest, aes(
      x = reorder(Attribute, Value),
      y = Value,
      fill = pct_rank
    )) +
      geom_col() +
      scale_fill_distiller(palette = "RdYlGn",
                           direction = 1,
                           name = "Relative\nrank") +
      coord_flip() +
      labs(
        x = NULL,
        y = "Measured value",
        title = paste("Latest sample:", format(latest, "%d %b %Y"))
      ) +
      theme_minimal(base_size = 11)
    
    ggplotly(p)
  })
  
  output$detail_table <- renderDT({
    detail_data() %>%
      select(
        Date = Date_parsed,
        Season,
        Attribute,
        Value,
        Weather = CurrentWeather,
        Tide,
        Colour,
        Clarity,
        Odour
      ) %>%
      arrange(desc(Date), Attribute) %>%
      datatable(
        filter = "top",
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
  })
  
  ## Site Pairs Tab ----
  
  # Debounced pair inputs
  pair_id_d     <- reactive(input$pair_id)     %>% debounce(400)
  pair_param_d  <- reactive(input$pair_param)  %>% debounce(400)
  pair_season_d <- reactive(input$pair_season) %>% debounce(400)
  pair_date_d   <- reactive(input$pair_date_range) %>% debounce(400)
  
  # Current pair metadata
  current_pair <- reactive({
    SITE_PAIRS[[which(sapply(SITE_PAIRS, `[[`, "id") == pair_id_d())]]
  })
  
  output$pair_description_ui <- renderUI({
    p <- current_pair()
    tagList(tags$small(class = "text-muted", p$description))
  })
  
  output$pair_site_labels_ui <- renderUI({
    p <- current_pair()
    tagList(
      tags$div(
        style = "display:flex; gap:12px; margin-bottom:4px;",
        tags$span(
          style = paste0("color:", PAIR_COLS[1], "; font-weight:bold;"),
          icon("circle"),
          " ",
          p$sites[1]
        ),
        tags$span(
          style = paste0("color:", PAIR_COLS[2], "; font-weight:bold;"),
          icon("circle"),
          " ",
          p$sites[2]
        )
      )
    )
  })
  
  output$pair_date_slider_ui <- renderUI({
    dates <- sort(unique(readings$Date_parsed))
    sliderInput(
      "pair_date_range",
      NULL,
      min        = min(dates),
      max        = max(dates),
      value      = c(min(dates), max(dates)),
      timeFormat = "%b %Y",
      step       = 30
    )
  })
  
  # Filtered pair data — wide format with one column per site
  pair_data_long <- reactive({
    req(pair_date_d())
    p  <- current_pair()
    d  <- readings %>%
      filter(
        SiteID %in% p$sites,
        Attribute == pair_param_d(),
        Date_parsed >= pair_date_d()[1],
        Date_parsed <= pair_date_d()[2]
      )
    if (pair_season_d() != "All")
      d <- d %>% filter(Season == pair_season_d())
    d
  })
  
  pair_data_wide <- reactive({
    p <- current_pair()
    pair_data_long() %>%
      select(Date_parsed, Season, SiteID, Value) %>%
      pivot_wider(
        names_from = SiteID,
        values_from = Value,
        values_fn = mean
      ) %>%
      rename(site_a = all_of(p$sites[1]),
             site_b = all_of(p$sites[2])) %>%
      filter(!is.na(site_a) | !is.na(site_b)) %>%
      mutate(difference = site_a - site_b)
  })
  
  # Value boxes
  output$pair_vbox_a <- renderUI({
    p <- current_pair()
    v <- mean(pair_data_long()$Value[pair_data_long()$SiteID == p$sites[1]], na.rm = TRUE)
    value_box(
      title    = paste("Mean", p$sites[1]),
      value    = paste0(round(v, 2), " ", param_unit(pair_param_d())),
      showcase = icon("circle", style = paste0("color:", PAIR_COLS[1])),
      height   = "90px"
    )
  })
  
  output$pair_vbox_b <- renderUI({
    p <- current_pair()
    v <- mean(pair_data_long()$Value[pair_data_long()$SiteID == p$sites[2]], na.rm = TRUE)
    value_box(
      title    = paste("Mean", p$sites[2]),
      value    = paste0(round(v, 2), " ", param_unit(pair_param_d())),
      showcase = icon("circle", style = paste0("color:", PAIR_COLS[2])),
      height   = "90px"
    )
  })
  
  output$pair_vbox_diff <- renderUI({
    v    <- mean(pair_data_wide()$difference, na.rm = TRUE)
    sign <- if (v > 0)
      "+"
    else
      ""
    value_box(
      title    = "Mean Difference (A − B)",
      value    = paste0(sign, round(v, 2), " ", param_unit(pair_param_d())),
      showcase = icon("arrow-right-arrow-left"),
      theme    = if (abs(v) < 0.01)
        "secondary"
      else if (v > 0)
        "warning"
      else
        "info",
      height   = "90px"
    )
  })
  
  # Time series comparison
  output$pair_timeseries <- renderPlotly({
    p <- current_pair()
    d <- pair_data_long() %>%
      group_by(SiteID, Date_parsed) %>%
      summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
    
    cols <- setNames(PAIR_COLS, p$sites)
    
    gg <- ggplot(d, aes(
      x = Date_parsed,
      y = Value,
      color = SiteID,
      group = SiteID
    )) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2.5) +
      scale_color_manual(values = cols) +
      labs(
        x = NULL,
        y = paste0(pair_param_d(), " (", param_unit(pair_param_d()), ")"),
        color = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top")
    
    ggplotly(gg, tooltip = c("x", "y", "colour")) %>%
      layout(hovermode = "x unified")
  })
  
  # Distribution box plots
  output$pair_boxplot <- renderPlotly({
    p    <- current_pair()
    d    <- pair_data_long()
    cols <- setNames(PAIR_COLS, p$sites)
    
    gg <- ggplot(d, aes(x = SiteID, y = Value, fill = SiteID)) +
      geom_boxplot(
        alpha = 0.7,
        outlier.shape = 21,
        outlier.size = 2
      ) +
      geom_jitter(width = 0.15,
                  alpha = 0.3,
                  size = 1) +
      scale_fill_manual(values = cols) +
      labs(x = NULL,
           y = paste0(pair_param_d(), " (", param_unit(pair_param_d()), ")")) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none")
    
    ggplotly(gg)
  })
  
  # Paired difference over time
  output$pair_difference <- renderPlotly({
    d <- pair_data_wide() %>% filter(!is.na(difference))
    
    gg <- ggplot(d, aes(x = Date_parsed, y = difference)) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "grey50"
      ) +
      geom_line(color = "grey30", linewidth = 0.8) +
      geom_point(aes(color = difference > 0),
                 size = 2.5,
                 show.legend = FALSE) +
      scale_color_manual(values = c("TRUE" = PAIR_COLS[1], "FALSE" = PAIR_COLS[2])) +
      labs(x = NULL,
           y = paste0("Δ ", pair_param_d(), " (", param_unit(pair_param_d()), ")")) +
      theme_minimal(base_size = 11)
    
    ggplotly(gg, tooltip = c("x", "y")) %>%
      layout(hovermode = "x unified")
  })
  
  # Site A vs Site B scatter
  output$pair_scatter <- renderPlotly({
    p <- current_pair()
    
    d <- pair_data_wide() %>%
      filter(!is.na(site_a), !is.na(site_b))
    
    # Calculate Pearson correlation
    r <- cor(d$site_a, d$site_b, method = "pearson")
    
    # Format correlation coefficient
    r_label <- paste0("r = ", round(r, 2))
    
    gg <- ggplot(d, aes(x = site_a, y = site_b, color = Season)) +
      
      geom_abline(
        slope = 1,
        intercept = 0,
        linetype = "dashed",
        color = "grey50"
      ) +
      
      geom_point(size = 2.5, alpha = 0.8) +
      
      geom_smooth(
        aes(x = site_a, y = site_b),
        method = "lm",
        se = TRUE,
        linewidth = 0.8,
        color = "grey20",
        fill = "grey70",
        alpha = 0.15,
        inherit.aes = FALSE
      ) +
      
      # annotate(
      #   "text",
      #   x = Inf,
      #   y = Inf,
      #   label = r_label,
      #   hjust = 1.1,
      #   vjust = 1.5,
      #   size = 4
      # ) +
      
      scale_color_manual(values = SEASON_COLS, na.value = "grey60") +
      
      labs(
        x = paste0(
          p$sites[1],
          " — ",
          pair_param_d(),
          " (",
          param_unit(pair_param_d()),
          ")"
        ),
        y = paste0(
          p$sites[2],
          " — ",
          pair_param_d(),
          " (",
          param_unit(pair_param_d()),
          ")"
        ),
        color = NULL
      ) +
      
      theme_minimal(base_size = 11) +
      theme(legend.position = "top")
    
    plot <- ggplotly(gg, tooltip = c("x", "y", "colour"))
    
    # add correlation annotation
    plot <- plot %>%
      layout(annotations = list(
        list(
          x = 0.98,
          y = 0.98,
          xref = "paper",
          yref = "paper",
          text = paste0("<b>r = ", round(r, 2), "</b>"),
          showarrow = FALSE,
          xanchor = "right",
          yanchor = "top",
          font = list(size = 14)
        )
      ))
    
    
  })
  
  # Mean difference by parameter bar chart
  output$pair_param_diff <- renderPlotly({
    req(pair_date_d())
    p <- current_pair()
    
    d <- readings %>%
      filter(SiteID %in% p$sites,
             Date_parsed >= pair_date_d()[1],
             Date_parsed <= pair_date_d()[2]) %>%
      {
        if (pair_season_d() != "All")
          filter(., Season == pair_season_d())
        else
          .
      } %>%
      group_by(SiteID, Attribute) %>%
      summarise(mean_val = mean(Value, na.rm = TRUE),
                .groups = "drop") %>%
      pivot_wider(names_from = SiteID, values_from = mean_val) %>%
      rename(site_a = all_of(p$sites[1]),
             site_b = all_of(p$sites[2])) %>%
      filter(!is.na(site_a), !is.na(site_b)) %>%
      mutate(diff      = site_a - site_b,
             direction = if_else(diff >= 0, p$sites[1], p$sites[2]))
    
    cols <- setNames(PAIR_COLS, p$sites)
    
    gg <- ggplot(d, aes(
      x = reorder(Attribute, diff),
      y = diff,
      fill = direction
    )) +
      geom_col() +
      geom_hline(yintercept = 0, color = "grey30") +
      scale_fill_manual(values = cols) +
      coord_flip() +
      labs(x = NULL, y = "Mean difference (A − B)", fill = "Higher at") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top")
    
    ggplotly(gg, tooltip = c("x", "y", "fill"))
  })
  
  ## Full Data Table ----
  
  output$full_table <- renderDT({
    readings %>%
      select(
        SiteID,
        Name,
        Date = Date_parsed,
        Season,
        Attribute,
        Value,
        Habitat,
        Distance_from_mouth,
        Weather = CurrentWeather,
        Tide
      ) %>%
      arrange(desc(Date)) %>%
      datatable(
        filter = "top",
        rownames = FALSE,
        options = list(pageLength = 20, scrollX = TRUE)
      )
  })
}