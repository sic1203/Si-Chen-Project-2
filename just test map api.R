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


Ngbh.load <-readOGR('https://opendata.arcgis.com/datasets/dbd133a206cc4a3aa915cb28baa60fd4_0.geojson')
Ball.load <-readOGR('https://opendata.arcgis.com/datasets/87f3b2bdfa6646319a56e86521480efd_0.geojson')
# https://services1.arcgis.com/YZCmUqbcsUpOKfj7/arcgis/rest/services/Ballfields_PGH/FeatureServer/0/query?where=FIELD_NAME+LIKE+%27M%25%27&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnDistinctValues=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=geojson&token=



#retrieve all data from API 
#test: select all data from API(it works)
#test<-ckanSQL(URLencode("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%227f438bd0-71c7-4997-a5b8-f12894599215%22"))

#retrieve unique value for factor coulmn, neighborhood
# Ngh <- sort(ckanUniques("7f438bd0-71c7-4997-a5b8-f12894599215", "Neighborhood")$Neighborhood)
# 
# 
# FetchGeo <- function(url){
#   
#   x <- URLencode(url)
#   y <- readOGR(x)
# }

# Unique values for Resource Field
# FetchGeoUniques <- function(id, field) {
#   url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
#   c(ckanSQL(URLencode(url)))
# }





# Define UI for application
ui <- navbarPage("Allegheny Ballfileds Location",
                 theme = shinytheme("cosmo"),
                 tabPanel("Allegheny Map",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("NgbhSelect",
                                          "Neighborhood",
                                          levels(Ngbh.load$hood),
                                          selected = c("Allegheny Center","Banksville"),
                                          selectize = T,
                                          multiple = T),
                              checkboxGroupInput("TypeSelect",
                                               "Active or not:",
                                               choices = unique(Ball.load$active),
                                               selected = c("Customer","Subscriber"))),
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
      addPolygons(data = Ngbh) %>%
      addCircleMarkers(data=Ball.load,radius=6,color="blue",stroke=FALSE,fillOpacity = 0.5,group="")%>%
      #add layers control
      addLayersControl(
        baseGroups = c("Toner", "Street"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  NgbhInputs <- reactive({
    map_input <-FetchGeojson()
    Ngbh <- Ngbh.load 
    
    
    if (length(input$NgbhSelect) > 0) {
      Ngbh<- subset(Ngbh, hood %in% input$NgbhSelect)
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

