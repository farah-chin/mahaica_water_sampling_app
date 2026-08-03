library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(scales)
library(DT)
library(RColorBrewer)

# Data Loading ----

SUPABASE_URL <- Sys.getenv("SUPABASE_URL")
SUPABASE_KEY <- Sys.getenv("SUPABASE_KEY")

supabase_get <- function(table, page_size = 1000) {
  headers <- add_headers(
    "apikey"        = SUPABASE_KEY,
    "Authorization" = paste("Bearer", SUPABASE_KEY)
  )
  pages  <- list()
  offset <- 0
  repeat {
    url  <- paste0(SUPABASE_URL, "/rest/v1/", table,
                   "?select=*&limit=", page_size, "&offset=", offset)
    resp <- GET(url, headers, timeout(10))
    if (http_error(resp))
      stop("Supabase fetch failed for '", table, "': HTTP ", status_code(resp))
    page <- as.data.frame(
      fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
    )
    pages  <- c(pages, list(page))
    if (nrow(page) < page_size) break
    offset <- offset + page_size
  }
  bind_rows(pages)
}

readings_raw <- supabase_get("water_sample_observations")
locations    <- supabase_get("mahaica_water_sample_locations")

# Clean locations
locations <- locations %>%
  mutate(
    Name        = if_else(is.na(Name) | Name == "", SiteID, Name),
    Habitat     = if_else(is.na(Habitat_type) | Habitat_type == "", "Unknown", Habitat_type),
    Description = if_else(is.na(Description) | Description == "", "No description available.", Description)
  )

# Exclude bank-assessment parameters
EXCLUDE_PARAMS <- c("LeftBank", "RightBank")

readings <- readings_raw %>%
  filter(!Attribute %in% EXCLUDE_PARAMS) %>%
  mutate(
    Value       = as.numeric(Value),
    Date_parsed = dmy(Date),
    YearMon     = format(Date_parsed, "%Y-%m"),
    MonthLabel  = format(Date_parsed, "%b %Y")
  ) %>%
  left_join(
    locations %>% select(SiteID, Name, Habitat, Description, Order),
    by = "SiteID"
  ) %>%
  filter(!is.na(Value))

# Otter Sightings ----
# Fetched once at startup from ArcGIS Online; if the request fails the app
# continues without otter data and shows a warning in the map sidebar.

OTTER_URL <- paste0(
  "https://services7.arcgis.com/3jO2fRV12whRxmim/ArcGIS/rest/services/",
  "Otter_sighting/FeatureServer/0/query",
  "?where=1%3D1&outFields=*&f=json"
)

otter_data        <- NULL
otter_load_error  <- NULL

tryCatch({
  resp  <- GET(OTTER_URL, timeout(10))
  if (http_error(resp)) stop("HTTP ", status_code(resp))
  js    <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
  attrs <- js$features$attributes
  attrs$Lat  <- as.numeric(attrs$Lat)
  attrs$Long <- as.numeric(attrs$Long)
  otter_data <<- attrs[!is.na(attrs$Lat) & !is.na(attrs$Long), ]
}, error = function(e) {
  otter_load_error <<- e$message
  message("Could not load otter sightings: ", e$message)
})

# Waterways ----
# River line features fetched as GeoJSON using the PostgREST Accept header,
# which tells Supabase to return geometry as GeoJSON rather than WKB hex

waterways_geojson    <- NULL
waterways_load_error <- NULL

tryCatch({
  url  <- paste0(SUPABASE_URL, "/rest/v1/mahaica_waterways?select=*")
  resp <- GET(url, add_headers(
    "apikey"        = SUPABASE_KEY,
    "Authorization" = paste("Bearer", SUPABASE_KEY),
    "Accept"        = "application/geo+json"
  ))
  if (http_error(resp))
    stop("HTTP ", status_code(resp))
  waterways_geojson <<- content(resp, as = "text", encoding = "UTF-8")
}, error = function(e) {
  waterways_load_error <<- e$message
  message("Could not load waterways: ", e$message)
})

# Constants ----

MAP_CENTER <- list(lng = -57.95, lat = 6.55, zoom = 10)
SITE_ZOOM  <- 14

PARAMETERS  <- sort(unique(readings$Attribute))
SEASONS     <- c("All", "Dry", "Wet")

PARAM_UNITS <- c(
  "Temp (C)"        = "\u00b0C",
  "pH"              = "pH",
  "DO (mg/L)"       = "mg/L",
  "Turbidity (NTU)" = "NTU",
  "Cond (uS/cm)"    = "\u00b5S/cm",
  "Salinity (PPT)"  = "PPT",
  "TDS (PPM)"       = "ppm",
  "TSS (mg/L)"      = "mg/L",
  "Ammonia"         = "mg/L",
  "Nitrate"         = "mg/L",
  "Nitrite"         = "mg/L",
  "Orthophosphate"  = "mg/L"
)

SEASON_COLS <- c("Dry" = "#E8A838", "Wet" = "#4A90D9")
OTTER_COLS  <- c("Giant River Otter" = "#A63600", "Neotropical Otter" = "#4C7300")

# Helpers ----

param_unit <- function(p) {
  if (p %in% names(PARAM_UNITS)) PARAM_UNITS[[p]] else ""
}

make_diamond_icon <- function(colour) {
  svg <- paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 18 18">',
    '<polygon points="9,1 17,9 9,17 1,9" ',
    'fill="', colour, '" stroke="white" stroke-width="1.5"/>',
    '</svg>'
  )
  paste0("data:image/svg+xml,", utils::URLencode(svg, reserved = TRUE))
}

TOOLTIP_LABEL_STYLE <- list(
  "font-size"        = "12px",
  "background-color" = "rgba(255,255,255,0.92)",
  "border"           = "1px solid #ccc",
  "border-radius"    = "4px",
  "padding"          = "6px 10px",
  "box-shadow"       = "2px 2px 4px rgba(0,0,0,0.15)"
)