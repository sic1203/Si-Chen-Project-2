library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(rgdal)
library(leaflet)
library(shinythemes)
library(shinydashboard)
library(reshape2)
library(plotly)
library(tibble)
require(leaflet.extras)
require(readxl)
require(stringr)
require(RColorBrewer)
library(rgeos)

ob_load <- readOGR("C:/Users/csvan/Documents/GitHub/SC_Project2/ScGH_project2/obesity_rates/obesity_rates.shp",layer="obesity_rates",GDAL1_integer64_policy = TRUE)
plot(ob_load)
head(ob_load)


ui <- navbarPage("Allegheny oBESITY Rate",
                 theme = shinytheme("cosmo"),
                 tabPanel("Allegheny Map",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("NgbhSelect",
                                          "Neighborhood",
                                          levels(ob_load$City_Neigh),
                                          selected = c("Allegheny Center","Banksville"),
                                          selectize = T,
                                          multiple = T)),
                            mainPanel(
                              # Using Shiny JS
                              shinyjs::useShinyjs(),
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                              # Map Output
                              leafletOutput("leaflet")
                              )
                          )
                 ),
                 tabPanel("Data",
                          fluidPage(
                            wellPanel(DT::dataTableOutput("table"))
                          )
                 )
)


server <- function(input, output) {
  
  output$leaflet <- renderLeaflet({
    # Load green infrastructure filtered data
    #greenInf <- greenInfInputs()
    Ngbh<-NgbhInputs()
    leaflet() %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite", group =" Street",options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles("Stamen.Toner", group="Toner",options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-80,40.45,zoom=12) %>%
      #add polygon layer
      addPolygons(data = ob_load$AREA) %>%
      # addCircleMarkers(data=Ball.load,radius=6,color="blue",stroke=FALSE,fillOpacity = 0.5,group="")%>%
      #add layers control
      addLayersControl(
        baseGroups = c("Toner", "Street"),
        options = layersControlOptions(collapsed = FALSE))
  })
  NgbhInputs <- reactive({
    Ngbh <- ob_load 
    
    if (length(input$NgbhSelect) > 0) {
      Ngbh<- subset(Ngbh, City_Neigh %in% input$NgbhSelect)
    }
    return(Ngbh)
  })
  
  # # Replace layer with filtered greenInfrastructure
  # observe({
  #   Ngbh <- NgbhInputs()
  #   # Data is greenInf
  #   leafletProxy("leaflet", data = Ngbh) 
  # })
  # 
  # output$table <- DT::renderDataTable(NgbhInputs()@data, options = list(scrollX = T))
  # # Print Inputs
  # observe({
  #   print(reactiveValuesToList(input))
  # })
  # # Enable button once a marker has been selected
  # observeEvent(input$leaflet_marker_click$id, {
  #   enable("delete")
  # })
  # # Add layerID to list of removed projects
  # observeEvent(input$delete, {
  #   enable("restore")
  #   isolate({
  #     values$removed <- c(values$removed, input$leaflet_marker_click$id)
  #   })
  # })
  # # Reset removed Projects
  # observeEvent(input$restore, {
  #   values$removed <- c()
  #   disable("restore")
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)


