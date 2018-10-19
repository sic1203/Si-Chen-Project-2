library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(rgdal)
library(shinythemes)
library(shinydashboard)
library(reshape2)
library(plotly)
library(tibble)
require(readxl)
require(stringr)

ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

#retrieve all data from API 
#test: select all data from API(it works)
#test<-ckanSQL(URLencode("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%227f438bd0-71c7-4997-a5b8-f12894599215%22"))

#retrieve unique value for factor coulmn, neighborhood
Ngh <- sort(ckanUniques("7f438bd0-71c7-4997-a5b8-f12894599215", "Neighborhood")$Neighborhood)
#retrieve total income value
Ssi<- sort(ckanUniques("7f438bd0-71c7-4997-a5b8-f12894599215","Estimate%3B%20Total%3A")$`Estimate..Total.`)  
#retrieve total income withou social security supplements
Ic<- sort(ckanUniques("7f438bd0-71c7-4997-a5b8-f12894599215","Estimate%3B%20Total%3A%20-%20No%20Social%20Security%20income")$`Estimate..Total....No.Social.Security.income`)  

Ssinum<- as.numeric(Ssi)
Icnum<-as.numeric(Ic)

Ngbh.load <-readOGR('https://opendata.arcgis.com/datasets/dbd133a206cc4a3aa915cb28baa60fd4_0.geojson')
Ball.load <-readOGR('https://opendata.arcgis.com/datasets/87f3b2bdfa6646319a56e86521480efd_0.geojson')

#create ui
ui <- navbarPage("Pitssburgh Neighborhood Income",
                 theme = shinytheme("cosmo"),
                 #first page, intruduction about the project
                 tabPanel("Introduction",
                          mainPanel(img(src ="ba.jpg",
                                        width="1600", 
                                        height="1000"))),
                 #create map panel 
                 tabPanel("Ball fileds in Allegheny",
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
                                         body {background-color: #e2dede;}"),
                              # Map Output
                              leafletOutput("leaflet")
                              )
                          )),
                 tabPanel("Income Distribution",
                          sidebarLayout(
                            sidebarPanel(
                              # Different neighborhood select
                              selectInput("NghSelect",
                                          "Choose the Nehborhood:",
                                          choices = Ngh,
                                          multiple = TRUE,
                                          selected=c("Allentown", "Arlington","Banksville","East Hills", "East Liberty", "Beechview", "Bedford Dwellings", "Beltzhoover", "Bluff")),
                              #Total Income without ssi Select
                              sliderInput("InSelect",
                                          "Estimate Income without SSI:",
                                          min = min(Icnum, na.rm = T),
                                          max = max(Icnum, na.rm = T),
                                          value = c("140", "6000"),
                                          step = 1),
                              #total income select
                              sliderInput("SsiSelect",
                                          "Social Security Income:",
                                          min=min(Ssinum,na.rm=T),
                                          max=max(Ssinum,na.rm=T),
                                          value=c("200","6000"),
                                          step=1),
                              #reset button
                              actionButton("reset", "Reset Your Selection", icon = icon("refresh")
                                           )
                            ),
                            # Output plot
                            mainPanel(
                              plotlyOutput("plot1"),
                              plotlyOutput("plot2"),
                              plotlyOutput("plot3")
                            )
                          )
                          ),
                 #tab panel
                 tabPanel("Table and Download",
                          inputPanel(
                            downloadButton("downloadData","Download the Income Data in Pittsburgh")),
                          fluidPage(DT::dataTableOutput("table")
                                    )
                          )
                 )
# Define server logic
server <- function(input, output, session) {
  #customize color pattern
  pal <- colorFactor(c("navy", "red"), domain = Ball.load$active)
  #output leafflet
  output$leaflet <- renderLeaflet({
    # Load neighborhood  filtered data
    Ngbh<-NgbhInputs()
    leaflet() %>%
      #addbasemaps 
      addProviderTiles("OpenStreetMap.BlackAndWhite", group =" Street",options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles("Stamen.Toner", group="Toner",options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-80,40.45,zoom=12) %>%
      #add polygon layer
      addPolygons(data = Ngbh,group="Neighborhood") %>%
      addCircleMarkers(data=Ball.load,radius=6,color=~pal(Ball.load$active),stroke=FALSE,fillOpacity = 0.5,group="Ballfields")%>%
      #add layers control
      addLayersControl(
        baseGroups = c("Toner", "Street"),
        overlayGroups = c("Ballfields","Neighborhood"),
        options = layersControlOptions(collapsed = FALSE))
  })
  NgbhInputs <- reactive({
    Ngbh <- Ngbh.load 
    if (length(input$NgbhSelect) > 0) {
      Ngbh<- subset(Ngbh, hood %in% input$NgbhSelect)
    }
    return(Ngbh)
  })
  AllInput <- reactive({
    Ngh_filter <- ifelse(length(input$NghSelect) > 0, 
                         paste0("%20AND%20%22Neighborhood%22%20IN%20(%27", 
                                paste0(gsub(" " ,"%20", input$NghSelect),
                                       collapse = "%27,%27"), "%27)"), "")
    Ssi_filter <- ifelse(length(input$SsiSelect) > 0,
                                paste0("%20AND%20%22Estimate%3B%20Total%3A%22%20BETWEEN%20%27", 
                                       paste(input$SsiSelect, collapse = "%27AND%27"),"%27"),"")
    # Build API Query with proper encodes
    url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%227f438bd0-71c7-4997-a5b8-f12894599215%22%20WHERE%20%22Estimate%3B%20Total%3A%20-%20No%20Social%20Security%20income%22%20%3E=%20%27", input$InSelect[1], "%27%20AND%20%22Estimate%3B%20Total%3A%20-%20No%20Social%20Security%20income%22%20%3C=%20%27", input$InSelect[2], "%27", Ngh_filter,Ssi_filter)
    # Load and clean data
    No_filter <- ckanSQL(url) %>%
      mutate(Estimate..Total....No.Social.Security.income= as.numeric(Estimate..Total....No.Social.Security.income),
             Estimate..Total. = as.numeric(Estimate..Total.))
    return(No_filter)
  })
  #plot1 , use plotly compare two income in each neighborhood
  output$plot1<-renderPlotly({
    dat1<- AllInput()
    dat2<- AllInput()
    dat1$Neighborhood<-factor(dat1$Neighborhood)
    dat2$Neighborhood<-factor(dat2$Neighborhood)
    p1<-plot_ly(dat1,x=~Neighborhood,y=~Estimate..Total....No.Social.Security.income,name="With SSI",type="bar")
    p2<-plot_ly(dat2,x=~Neighborhood,y=~Estimate..Total.,name = "Without SSI",type="bar")
    subplot(p1,p2)
  })
  #create plot about the distribution of without SSI
  output$plot2 <- renderPlotly(({
    dat3<-AllInput()
    ggplotly(
      ggplot(data =dat3, aes(x=Estimate..Total.))+
        geom_density(fill="blue",alpha=.6)+
        xlab("Total Income") +
        ylab("Density") +
        ggtitle("Distribution of Total Income with social Security Income")
    )
  })
  )
  #create plot about the distribton of SSI
  output$plot3 <- renderPlotly(({
    dat4<-AllInput()
    ggplotly(
      ggplot(data =dat4, aes(x=Estimate..Total....No.Social.Security.income))+
        geom_density(fill="pink",alpha=.6)+
        xlab("SSI Income") +
        ylab("Density") +
        ggtitle("Distribution of Total Income without Social Security Income")
    )
  })
  )
  #table oupt
  output$table <- DT::renderDataTable({
    No_filter <- AllInput()
    subset(No_filter, select = c(Neighborhood, Estimate..Total.,Estimate..Total....No.Social.Security.income))
  })
  # Updating the URL Bar
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("income in Pittsburgh", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(obInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session,"NghSelect",selected = c("Allentown", "Arlington","Banksville","East Hills", "East Liberty", "Beechview", "Bedford Dwellings", "Beltzhoover", "Bluff"))
    updateSliderInput(session, "InSelect", value = c("140", "6000"))
    updateSliderInput(session, "SsiSelect", value = c("200", "6000"))
    showNotification("Hey! You did it! You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
