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
                                          multiple = T)
                              ),
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
                 )
)


server <- function(input, output) {
  pal <- colorFactor(c("navy", "red"), domain = Ball.load$active)
  
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
      addCircleMarkers(data=Ball.load,radius=6,color=~pal(Ball.load$active),stroke=FALSE,fillOpacity = 0.5)%>%
      #add layers control
      addLayersControl(
        baseGroups = c("Toner", "Street"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  NgbhInputs <- reactive({
    Ngbh <- Ngbh.load 
    
    
    if (length(input$NgbhSelect) > 0) {
      Ngbh<- subset(Ngbh, hood %in% input$NgbhSelect)
    }
    return(Ngbh)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)



