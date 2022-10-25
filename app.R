library(shiny)
library(shinyTime)
library(astroFns)
library(dplyr)
library(ggplot2)
library(magrittr)
library(plotly)

source("functions.R")
stars <- readRDS("data\\stars.RDS")
constellations <- readRDS("data\\Constell_lines.RDS")
catalog <- readRDS("data\\Deep_objects.RDS")
object_types <- c("Galaxy", "Galactic Nebula", "Planetary Nebula", "Open Cluster", "Globular Cluster")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(4, dateInput("date", "Observing date")),
        column(5, timeInput("time", "Observing time", value = strptime("20:00:00", "%T"))),
        column(3, selectInput("timezone", "Time zone", selected = "UTC", timezones))
      ),
      fluidRow(
        column(6, numericInput("long", "Observing longitude", 0)),
        column(6, numericInput("lat", "Observing latitude", 0))
      ),
      fluidRow(
        column(4, h4("Show Me")),
        column(8, h4("With Magnitude Between"))
      ),
      fluidRow(
        column(4, checkboxInput("gal_view", "Galaxies", TRUE)),
        column(8, sliderInput("gal_mag", label = NULL, min = 2, max = 17.9, value = c(2,6), step = 0.1))
      ),
      fluidRow(
        column(4, checkboxInput("oc_view", "Open Clusters", TRUE)),
        column(8, sliderInput("oc_mag", label = NULL, min = 1.6, max = 14.5, value = c(1.6,6), step = 0.1))
      ), 
      fluidRow(
        column(4, checkboxInput("gc_view", "Globular Clusters", TRUE)),
        column(8, sliderInput("gc_mag", label = NULL, min = 4, max = 14.1, value = c(4,6), step = 0.1))
      ), 
      fluidRow(
        column(4, checkboxInput("eneb_view", "Galactic Nebulae", TRUE)),
        column(8, sliderInput("eneb_mag", label = NULL, min = 4, max = 11.6, value = c(4,6), step = 0.1))
      ),
      fluidRow(
        column(4, checkboxInput("pneb_view", "Planetary Nebulae", TRUE)),
        column(8, sliderInput("pneb_mag", label = NULL, min = 7.3, max = 15, value = c(7.3,7.3), step = 0.1))
      )
    ),
    mainPanel(
      plotlyOutput("projection", width = "900px", height = "900px"),
      tableOutput("test")
    )
  )
)

server <- function(input, output, session) {
  time <- reactive(as.POSIXct(paste(input$date, format(input$time, "%T")), tz = to_OlsonName(input$timezone)))
  local_sidereal_time <- reactive(to_local_sidereal_time(time(), input$long))
  constell_lines <- reactive({
      mutate(constellations, ha_start = local_sidereal_time() / 15 - rascension_start,
                             ha_end = local_sidereal_time() / 15 - rascension_end) %>%
      mutate(elevation_start = elev(rad2dms(degree_to_rad(declination_start)), ha_start, rad2dms(degree_to_rad(input$lat))),
             elevation_end = elev(rad2dms(degree_to_rad(declination_end)), ha_end, rad2dms(degree_to_rad(input$lat)))) %>%
      filter(elevation_start >= 0, elevation_end >= 0) %>%
      mutate(azimuth_start = azimuth(rad2dms(degree_to_rad(declination_start)), ha_start, rad2dms(degree_to_rad(input$lat))),
             azimuth_end = azimuth(rad2dms(degree_to_rad(declination_end)), ha_end, rad2dms(degree_to_rad(input$lat)))) %>%
      mutate(x_start = project_x(elevation_start, azimuth_start), 
             y_start = project_y(elevation_start, azimuth_start),
             x_end = project_x(elevation_end, azimuth_end),
             y_end = project_y(elevation_end, azimuth_end))
    })
  object_type <- reactive({object_types[which(c(input$gal_view, input$eneb_view, input$pneb_view, input$oc_view, input$gc_view))]})
  deep_objects <- reactive({
    filter(catalog, type %in% object_type()) %>%
      filter(case_when(type == "Galaxy" ~ between(VMAG, input$gal_mag[1], input$gal_mag[2]),
                       type == "Galactic Nebula" ~ between(VMAG, input$eneb_mag[1], input$eneb_mag[2]),
                       type == "Planetary Nebula" ~ between(VMAG, input$pneb_mag[1], input$pneb_mag[2]),
                       type == "Open Cluster" ~ between(VMAG, input$oc_mag[1], input$oc_mag[2]),
                       type == "Globular Cluster" ~ between(VMAG, input$gc_mag[1], input$gc_mag[2]))) %>%
      mutate(ha = local_sidereal_time() / 15 - rascension) %>%
      mutate(elevation = elev(rad2dms(degree_to_rad(declination)), ha, rad2dms(degree_to_rad(input$lat)))) %>%
      filter(elevation >= 0) %>%
      mutate(azimuth = azimuth(rad2dms(degree_to_rad(declination)), ha, rad2dms(degree_to_rad(input$lat)))) %>%
      mutate(x = project_x(elevation, azimuth), 
             y = project_y(elevation, azimuth))
  })
  projection_plot <- reactive({ggplotly(tooltip = c("name"),
    ggplot(constell_lines()) +
      geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, name = constell_names)) +
      geom_point(aes(x = x, y = y, shape = type, name = name), data = deep_objects()) +
      geom_text(x = 0, y = 1, label = "North") +
      geom_text(x = 1, y = 0, label = "West") +
      geom_text(x = 0, y = -1, label = "South") +
      geom_text(x = -1, y = 0, label = "East") +
      theme_minimal() +
      coord_cartesian(xlim = c(-1,1), ylim = c(-1,1)) +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()))
  })
  output$projection <- renderPlotly(projection_plot())
  output$test <- renderTable(deep_objects())
}
  

shinyApp(ui, server)
