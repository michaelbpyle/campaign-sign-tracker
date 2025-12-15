# Campaign Sign Delivery Tracker - Shiny App
# Interactive map showing yard sign requests and delivery status
#
# This is a demonstration app using FAKE DATA.
# Reconstructed from the Gaudin Campaign sign tracker for portfolio purposes.

library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(tidygeocoder)

# =============================================================================
# Data File Path
# =============================================================================
DATA_FILE <- "data/FAKE_DATA_volunteers.csv"

# =============================================================================
# Helper Functions
# =============================================================================
get_sign_requests <- function() {
  # Skip the comment header lines
  data <- read_csv(DATA_FILE, comment = "#", show_col_types = FALSE)

  data %>%
    filter(pref_yard_sign == 1) %>%
    mutate(
      has_sign = replace_na(has_sign, 0),
      sign_delivered_date = as.Date(sign_delivered_date)
    ) %>%
    arrange(name)
}

save_data <- function(data) {
  # Preserve the header comment when saving
  header <- c(
    "############################################################################################",
    "# FAKE DATA - FOR DEMONSTRATION PURPOSES ONLY",
    "# This file contains FICTIONAL names with REAL New Orleans addresses for demo purposes.",
    "# All volunteer information (names, phone numbers, emails) is completely fabricated.",
    "# DO NOT use this data for any real-world purposes.",
    "############################################################################################"
  )

  # Write header
  writeLines(header, DATA_FILE)

  # Append CSV data
  write_csv(data, DATA_FILE, append = TRUE)
}

update_sign_status <- function(data, volunteer_id, has_sign) {
  data <- data %>%
    mutate(
      has_sign = if_else(id == volunteer_id, as.integer(has_sign), has_sign),
      sign_delivered_date = if_else(
        id == volunteer_id,
        if (has_sign == 1) Sys.Date() else as.Date(NA),
        sign_delivered_date
      )
    )
  save_data(data)
  data
}

update_coordinates <- function(data, volunteer_id, lat, lon) {
  data <- data %>%
    mutate(
      latitude = if_else(id == volunteer_id, lat, latitude),
      longitude = if_else(id == volunteer_id, lon, longitude)
    )
  save_data(data)
  data
}

geocode_address <- function(street, city, state, zip) {
  full_address <- paste(street, city, state, zip, "USA", sep = ", ")
  result <- tryCatch({
    geo(full_address, method = "osm", quiet = TRUE)
  }, error = function(e) {
    data.frame(lat = NA, long = NA)
  })

  if (nrow(result) > 0 && !is.na(result$lat[1])) {
    return(c(lat = result$lat[1], lon = result$long[1]))
  }
  return(c(lat = NA, lon = NA))
}

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      }
      .demo-banner {
        background: #fff3cd;
        border: 1px solid #ffc107;
        padding: 10px 15px;
        margin-bottom: 15px;
        border-radius: 5px;
        text-align: center;
      }
      .info-box {
        background: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 15px;
      }
      .stat-box {
        text-align: center;
        padding: 20px;
        border-radius: 5px;
        margin: 5px;
      }
      .stat-red { background: #ffebee; border: 2px solid #f44336; }
      .stat-green { background: #e8f5e9; border: 2px solid #4caf50; }
      .stat-total { background: #e3f2fd; border: 2px solid #2196f3; }

      /* Pulsing marker animations */
      @keyframes pulse-red {
        0% { transform: scale(1); opacity: 0.9; }
        50% { transform: scale(1.3); opacity: 0.6; }
        100% { transform: scale(1); opacity: 0.9; }
      }
      @keyframes pulse-green {
        0% { transform: scale(1); opacity: 0.9; }
        50% { transform: scale(1.2); opacity: 0.7; }
        100% { transform: scale(1); opacity: 0.9; }
      }
      .pulse-red {
        animation: pulse-red 0.5s ease-in-out infinite;
      }
      .pulse-green {
        animation: pulse-green 0.5s ease-in-out infinite;
      }
      .leaflet-marker-icon.pulse-red,
      path.pulse-red {
        animation: pulse-red 0.5s ease-in-out infinite;
        transform-origin: center;
      }
      .leaflet-marker-icon.pulse-green,
      path.pulse-green {
        animation: pulse-green 0.5s ease-in-out infinite;
        transform-origin: center;
      }
    "))
  ),

  titlePanel("Yard Sign Delivery Tracker"),

  div(class = "demo-banner",
      tags$b("DEMONSTRATION APP"), " - Using fake data with fictional names and real New Orleans addresses"
  ),

  fluidRow(
    column(4,
           div(class = "stat-box stat-total",
               h3(textOutput("total_requests")),
               p("Total Requests")
           )
    ),
    column(4,
           div(class = "stat-box stat-red",
               h3(textOutput("pending_count")),
               p("Pending Delivery")
           )
    ),
    column(4,
           div(class = "stat-box stat-green",
               h3(textOutput("delivered_count")),
               p("Delivered")
           )
    )
  ),

  hr(),

  fluidRow(
    column(8,
           leafletOutput("map", height = 500)
    ),
    column(4,
           div(class = "info-box",
               h4("Selected Location"),
               uiOutput("selected_info"),
               hr(),
               actionButton("toggle_status", "Toggle Delivery Status",
                            class = "btn-primary btn-block"),
               actionButton("geocode_btn", "Geocode Missing Addresses",
                            class = "btn-warning btn-block")
           ),
           hr(),
           h4("Delivery List"),
           tableOutput("delivery_table")
    )
  ),

  hr(),
  p("Red = Needs Sign | Green = Delivered", style = "text-align: center; color: #666;")
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  # Reactive data - refreshes when invalidated
  sign_data <- reactiveVal(NULL)
  selected_id <- reactiveVal(NULL)

  # Load initial data
  observe({
    sign_data(get_sign_requests())
  })

  # Stats
  output$total_requests <- renderText({
    req(sign_data())
    nrow(sign_data())
  })

  output$pending_count <- renderText({
    req(sign_data())
    sum(sign_data()$has_sign == 0 | is.na(sign_data()$has_sign))
  })

  output$delivered_count <- renderText({
    req(sign_data())
    sum(sign_data()$has_sign == 1, na.rm = TRUE)
  })

  # Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -90.07, lat = 29.95, zoom = 11)  # New Orleans default
  })

  # Update map markers when data changes
  observe({
    req(sign_data())
    data <- sign_data()

    # Filter to only geocoded addresses
    map_data <- data %>%
      filter(!is.na(latitude) & !is.na(longitude))

    # Clear existing markers
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes()

    if (nrow(map_data) == 0) {
      return()
    }

    # Create popups
    map_data <- map_data %>%
      mutate(
        popup = paste0(
          "<b>", name, "</b><br>",
          street_address, "<br>",
          city, ", ", state, " ", zip, "<br>",
          "<br>Phone: ", phone, "<br>",
          "Status: ", ifelse(has_sign == 1, "DELIVERED", "PENDING")
        )
      )

    # Split by status for different icons
    pending <- map_data %>% filter(has_sign == 0 | is.na(has_sign))
    delivered <- map_data %>% filter(has_sign == 1)

    # Add pending markers (red, pulsing)
    if (nrow(pending) > 0) {
      for (i in 1:nrow(pending)) {
        row <- pending[i, ]
        leafletProxy("map") %>%
          addMarkers(
            lng = row$longitude,
            lat = row$latitude,
            popup = row$popup,
            layerId = as.character(row$id),
            icon = divIcon(
              html = '<div class="pulse-red" style="
                width: 22px;
                height: 22px;
                background-color: #f44336;
                border: 3px solid #b71c1c;
                border-radius: 50%;
                box-shadow: 0 0 12px #f44336;
              "></div>',
              className = "",
              iconSize = c(22, 22),
              iconAnchor = c(11, 11)
            )
          )
      }
    }

    # Add delivered markers (green, pulsing)
    if (nrow(delivered) > 0) {
      for (i in 1:nrow(delivered)) {
        row <- delivered[i, ]
        leafletProxy("map") %>%
          addMarkers(
            lng = row$longitude,
            lat = row$latitude,
            popup = row$popup,
            layerId = as.character(row$id),
            icon = divIcon(
              html = '<div class="pulse-green" style="
                width: 22px;
                height: 22px;
                background-color: #4caf50;
                border: 3px solid #1b5e20;
                border-radius: 50%;
                box-shadow: 0 0 12px #4caf50;
              "></div>',
              className = "",
              iconSize = c(22, 22),
              iconAnchor = c(11, 11)
            )
          )
      }
    }
  })

  # Handle marker clicks
  observeEvent(input$map_marker_click, {
    selected_id(as.integer(input$map_marker_click$id))
  })

  # Show selected info
  output$selected_info <- renderUI({
    req(selected_id(), sign_data())

    person <- sign_data() %>% filter(id == selected_id())
    if (nrow(person) == 0) return(p("Click a marker on the map"))

    tags$div(
      tags$p(tags$b(person$name)),
      tags$p(person$street_address),
      tags$p(paste(person$city, person$state, person$zip, sep = ", ")),
      tags$p(paste("Phone:", person$phone)),
      tags$p(
        tags$b("Status: "),
        if (person$has_sign == 1) {
          tags$span("DELIVERED", style = "color: green; font-weight: bold;")
        } else {
          tags$span("PENDING", style = "color: red; font-weight: bold;")
        }
      )
    )
  })

  # Toggle delivery status
  observeEvent(input$toggle_status, {
    req(selected_id())

    person <- sign_data() %>% filter(id == selected_id())
    if (nrow(person) == 0) return()

    new_status <- if (person$has_sign == 1) 0 else 1
    updated_data <- update_sign_status(sign_data(), selected_id(), new_status)
    sign_data(updated_data)

    showNotification(
      paste(person$name, "-", if (new_status == 1) "Marked as DELIVERED" else "Marked as PENDING"),
      type = if (new_status == 1) "message" else "warning"
    )
  })

  # Geocode missing addresses
  observeEvent(input$geocode_btn, {
    req(sign_data())

    to_geocode <- sign_data() %>%
      filter(is.na(latitude) | is.na(longitude)) %>%
      filter(!is.na(street_address) & street_address != "")

    if (nrow(to_geocode) == 0) {
      showNotification("All addresses already geocoded!", type = "message")
      return()
    }

    showNotification(paste("Geocoding", nrow(to_geocode), "addresses..."), type = "message")

    current_data <- sign_data()

    withProgress(message = "Geocoding addresses", value = 0, {
      for (i in 1:nrow(to_geocode)) {
        row <- to_geocode[i, ]
        coords <- geocode_address(row$street_address, row$city, row$state, row$zip)

        if (!is.na(coords["lat"])) {
          current_data <- update_coordinates(current_data, row$id, coords["lat"], coords["lon"])
        }

        incProgress(1/nrow(to_geocode), detail = paste("Address", i, "of", nrow(to_geocode)))
        Sys.sleep(1)  # Rate limiting for OSM
      }
    })

    sign_data(current_data)
    showNotification("Geocoding complete!", type = "message")
  })

  # Delivery list table
  output$delivery_table <- renderTable({
    req(sign_data())
    sign_data() %>%
      select(name, city, has_sign) %>%
      mutate(
        Status = ifelse(has_sign == 1, "\u2713", ""),
        has_sign = NULL
      ) %>%
      rename(Name = name, City = city)
  })
}

# =============================================================================
# Run App
# =============================================================================
shinyApp(ui = ui, server = server)
