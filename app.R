# Sign Delivery Tracker - Shiny App
# Interactive map showing yard sign requests and delivery status

library(shiny)
library(leaflet)
library(dplyr)
library(readr)

# =============================================================================
# Data File Path
# =============================================================================
DATA_FILE <- "data/FAKE_DATA_volunteers.csv"

# =============================================================================
# Helper Functions
# =============================================================================
get_sign_requests <- function() {
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
  header <- c(
    "############################################################################################",
    "# FAKE DATA - FOR DEMONSTRATION PURPOSES ONLY",
    "# This file contains FICTIONAL names with REAL New Orleans addresses for demo purposes.",
    "# All volunteer information (names, phone numbers, emails) is completely fabricated.",
    "# DO NOT use this data for any real-world purposes.",
    "############################################################################################"
  )
  writeLines(header, DATA_FILE)
  write_csv(data, DATA_FILE, append = TRUE)
}

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .info-box { background: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px; }
    .stat-box { text-align: center; padding: 20px; border-radius: 5px; margin: 5px; }
    .stat-red { background: #ffebee; border: 2px solid #f44336; }
    .stat-green { background: #e8f5e9; border: 2px solid #4caf50; }
    .stat-total { background: #e3f2fd; border: 2px solid #2196f3; }
    .demo-banner { background: #fff3cd; border: 1px solid #ffc107; padding: 10px; margin-bottom: 15px; border-radius: 5px; text-align: center; }
  "))),

  titlePanel("Yard Sign Delivery Tracker"),

  div(class = "demo-banner", tags$b("DEMO"), " - Fictional names with real New Orleans addresses"),

  fluidRow(
    column(4, div(class = "stat-box stat-total", h3(textOutput("total_requests")), p("Total Requests"))),
    column(4, div(class = "stat-box stat-red", h3(textOutput("pending_count")), p("Pending Delivery"))),
    column(4, div(class = "stat-box stat-green", h3(textOutput("delivered_count")), p("Delivered")))
  ),

  hr(),

  fluidRow(
    column(8, leafletOutput("map", height = 500)),
    column(4,
      div(class = "info-box",
        h4("Selected Location"),
        uiOutput("selected_info"),
        hr(),
        actionButton("toggle_status", "Toggle Delivery Status", class = "btn-primary btn-block")
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
  sign_data <- reactiveVal(NULL)
  selected_id <- reactiveVal(NULL)

  observe({ sign_data(get_sign_requests()) })

  output$total_requests <- renderText({ req(sign_data()); nrow(sign_data()) })
  output$pending_count <- renderText({ req(sign_data()); sum(sign_data()$has_sign == 0 | is.na(sign_data()$has_sign)) })
  output$delivered_count <- renderText({ req(sign_data()); sum(sign_data()$has_sign == 1, na.rm = TRUE) })

  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = -90.07, lat = 29.95, zoom = 11)
  })

  observe({
    req(sign_data())
    data <- sign_data()
    map_data <- data %>% filter(!is.na(latitude) & !is.na(longitude))

    leafletProxy("map") %>% clearMarkers()

    if (nrow(map_data) == 0) return()

    map_data <- map_data %>% mutate(
      popup = paste0("<b>", name, "</b><br>", street_address, "<br>", city, ", ", state, " ", zip, "<br><br>Phone: ", phone, "<br>Status: ", ifelse(has_sign == 1, "DELIVERED", "PENDING"))
    )

    pending <- map_data %>% filter(has_sign == 0 | is.na(has_sign))
    delivered <- map_data %>% filter(has_sign == 1)

    red_icon <- makeIcon(iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png", iconWidth = 25, iconHeight = 41, iconAnchorX = 12, iconAnchorY = 41)
    green_icon <- makeIcon(iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png", iconWidth = 25, iconHeight = 41, iconAnchorX = 12, iconAnchorY = 41)

    if (nrow(pending) > 0) {
      leafletProxy("map") %>% addMarkers(data = pending, lng = ~longitude, lat = ~latitude, popup = ~popup, layerId = ~as.character(id), icon = red_icon)
    }
    if (nrow(delivered) > 0) {
      leafletProxy("map") %>% addMarkers(data = delivered, lng = ~longitude, lat = ~latitude, popup = ~popup, layerId = ~as.character(id), icon = green_icon)
    }
  })

  observeEvent(input$map_marker_click, { selected_id(as.integer(input$map_marker_click$id)) })

  output$selected_info <- renderUI({
    req(selected_id(), sign_data())
    person <- sign_data() %>% filter(id == selected_id())
    if (nrow(person) == 0) return(p("Click a marker on the map"))
    tags$div(
      tags$p(tags$b(person$name)),
      tags$p(person$street_address),
      tags$p(paste(person$city, person$state, person$zip, sep = ", ")),
      tags$p(paste("Phone:", person$phone)),
      tags$p(tags$b("Status: "), if (person$has_sign == 1) tags$span("DELIVERED", style = "color: green;") else tags$span("PENDING", style = "color: red;"))
    )
  })

  observeEvent(input$toggle_status, {
    req(selected_id())
    person <- sign_data() %>% filter(id == selected_id())
    if (nrow(person) == 0) return()

    new_status <- if (person$has_sign == 1) 0 else 1
    updated_data <- sign_data() %>%
      mutate(
        has_sign = if_else(id == selected_id(), as.integer(new_status), has_sign),
        sign_delivered_date = if_else(id == selected_id(), if (new_status == 1) Sys.Date() else as.Date(NA), sign_delivered_date)
      )
    save_data(updated_data)
    sign_data(updated_data)

    showNotification(paste(person$name, "-", if (new_status == 1) "Marked as DELIVERED" else "Marked as PENDING"), type = if (new_status == 1) "message" else "warning")
  })

  output$delivery_table <- renderTable({
    req(sign_data())
    sign_data() %>% select(name, city, has_sign) %>% mutate(Status = ifelse(has_sign == 1, "Done", ""), has_sign = NULL) %>% rename(Name = name, City = city)
  })
}

shinyApp(ui = ui, server = server)
