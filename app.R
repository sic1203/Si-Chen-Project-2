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

income<- read.csv('C:/Users/csvan/Documents/GitHub/SC_Project2/ScGH_project2/ssi.csv',head=TRUE)


ui <- navbarPage("The relationship between obesity and income in Pitssburgh",
                 theme = shinytheme("cosmo"),
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Different neighborhood select
                              selectInput("NghSelect",
                                          "Choose the Nehborhood:",
                                          choices = sort(unique(income$Neighborhood)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected=c("Arlington","East Hills","East Liberty")),
                              #Income Select
                              sliderInput("InSelect",
                                          "Estimate Income without SSI:",
                                          min = min(income$Estimate..Total....No.Social.Security.income, na.rm = T),
                                          max = max(income$Estimate..Total....No.Social.Security.income, na.rm = T),
                                          value = c("200", "6000"),
                                          step = 1),
                              sliderInput("SsiSelect",
                                          "Social Security Income:",
                                          min=min(income$Estimate..Total....With.Social.Security.income,na.rm=T),
                                          max=max(income$Estimate..Total....With.Social.Security.income,na.rm=T),
                                          value=c("200","6000"),
                                          step=1),
                              actionButton("reset", "Reset Your Selection", icon = icon("refresh"))
                            ),
                            # Output plot
                            mainPanel(
                              plotlyOutput("plot1"),
                              plotlyOutput("plot2")
                            )
                          )
                          ),
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download the Social Security Income Data in Pittsburgh")),
                          fluidPage(DT::dataTableOutput("table"))
                          )
                 )
# Define server logic
server <- function(input, output, session = session) {
  icInput <- reactive({
    income.filter <- income %>%
      # Slider income without social security Filter
      filter(Estimate..Total....No.Social.Security.income >= input$InSelect[1] & Estimate..Total....No.Social.Security.income <= input$InSelect[2])
    #selectinput neighborhood filter
    if (length(input$NghSelect) > 0 ) {
      income.filter <- subset(income.filter, Neighborhood%in% input$NghSelect)
    }
    #slider income with social security filter
    income.filter <- income %>%
      # Slider income without social security Filter
      filter(Estimate..Total....With.Social.Security.income >= input$SsiSelect[1] & Estimate..Total....With.Social.Security.income <= input$SsiSelect[2])
    
    return(income.filter)
  })
  
  output$plot1 <- renderPlotly({
    dat <- icInput()
    ggplotly(
      ggplot(data = dat, aes(x = Estimate..Total....No.Social.Security.income)) + 
        geom_density(fill="gray",alpha=.6)
      ) 
  })
  output$plot2 <- renderPlotly(({
    dat<- icInput()
    ggplotly(
      ggplot(data =dat, aes(x=Estimate..Total....With.Social.Security.income))+
        geom_density(fill="blue",alpha=.6)
    )
  }))
  output$table <- DT::renderDataTable({
    income.filter <- icInput()
    subset(income.filter, select = c(Neighborhood, Estimate..Total....With.Social.Security.income,Estimate..Total....No.Social.Security.income))
  })
  
}
#   # Updating the URL Bar
#   observe({
#     print(reactiveValuesToList(input))
#     session$doBookmark()
#   })
#   onBookmarked(function(url) {
#     updateQueryString(url)
#   })
#   # Download data in the datatable
#   output$downloadData <- downloadHandler(
#     filename = function() {
#       paste("obesity income", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#       write.csv(obInput(), file)
#     }
#   )
# }
#   # Reset Filter Data
#   observeEvent(input$reset, {
#     updateSelectInput(session,"SpeedSelect",selected = c("1~10","10~20","20~30"))
#     updateSliderInput(session, "TripdurationSelect", value = c("80", "151106"))
#     updateCheckboxGroupInput(session, "UserSelect", selected = c("Customer", "Subscriber"))
#     showNotification("Hey! You did it! You have successfully reset the filters", type = "message")
#   })
# }

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
