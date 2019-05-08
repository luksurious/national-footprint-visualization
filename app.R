#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

rawData <- read.csv("data/NFA 2018.csv")

totalBiocapPerCountry <- rawData[rawData$record == "BiocapTotGHA", ]

CapitaBiocapPerCountry <- rawData[rawData$record == "BiocapPerCap", ]

totalBiocapContinent <- aggregate(
  cbind(crop_land, grazing_land, forest_land, fishing_ground, built_up_land, population, total) ~ year + UN_region,
  totalBiocapPerCountry, sum)

CapitaBiocapContinent <- aggregate(
  cbind(crop_land, grazing_land, forest_land, fishing_ground, built_up_land, population, total) ~ year + UN_region,
  CapitaBiocapPerCountry, sum)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("National Footprint Visualization"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        helpText("Are there different trends in the evolution of the different resources visible?"),
        radioButtons("regionType", label = "Type of region", choices = c("Continents", "Countries"),
                     selected = "Continents"),
        conditionalPanel(
          condition = "input.regionType == 'Continents'",
          
          selectInput(
            "region",
            label = "Choose the region to show in the chart",
            choices = levels(totalBiocapContinent$UN_region),
            selected = "World"
          )
        ),
        conditionalPanel(
          condition = "input.regionType == 'Countries'",
          
          selectInput(
            "country",
            label = "Choose the country to show in the chart",
            choices = levels(totalBiocapPerCountry$country),
            selected = "Spain"
          )
        ),
        
        radioButtons("dataType", label = "Type of data", choices = c("Per person", "Total"), 
                     selected = "Per person"),
        
        sliderInput("years",
                    "Years",
                    min(totalBiocapContinent$year),
                    max(totalBiocapContinent$year),
                    value = c(max(totalBiocapContinent$year)-20, max(totalBiocapContinent$year)),
                    sep = "",
                    step = 1),
        
        checkboxInput("show_total", label = "Show totals")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("plot"),
         h4("Changes in the resource types over the selected period", align = "center"),
         fluidRow(
           splitLayout(cellWidths = c("50%", "50%"), 
                       plotlyOutput("absoluteChange"), plotlyOutput("relativeChange"))
         ),
         plotlyOutput("distribution")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  curData <- reactive({
    if (input$regionType == 'Countries') {
      
      if (input$dataType == 'Per person') {
        data <- CapitaBiocapPerCountry[CapitaBiocapPerCountry$country == input$country
                                      & CapitaBiocapPerCountry$year >= input$years[1]
                                      & CapitaBiocapPerCountry$year <= input$years[2], ]
      } else {
        data <- totalBiocapPerCountry[totalBiocapPerCountry$country == input$country
                                      & totalBiocapPerCountry$year >= input$years[1]
                                      & totalBiocapPerCountry$year <= input$years[2], ]
      }
    } else {
      data <- totalBiocapContinent[totalBiocapContinent$UN_region == input$region
                           & totalBiocapContinent$year >= input$years[1]
                           & totalBiocapContinent$year <= input$years[2], ]
      
      # per continent data per capita is not provided, we need to roughly calculate it
      # because of the large numbers, the precision is not perfect
      if (input$dataType == 'Per person') {
        data$crop_land <- data$crop_land / data$population
        data$grazing_land = data$grazing_land / data$population
        data$forest_land = data$forest_land / data$population
        data$fishing_ground = data$fishing_ground / data$population
        data$built_up_land = data$built_up_land / data$population
        data$total = data$total / data$population
      }
    }
    
    data
  })  
   
   output$plot <- renderPlotly({
     p <- plot_ly(curData(), x = ~year, name = 'crop land', y = ~crop_land, type = 'scatter', mode = 'lines') %>%
       add_trace(y = ~grazing_land, name = 'grazing land', mode = 'lines') %>%
       add_trace(y = ~forest_land, name = 'forest land', mode = 'lines') %>%
       add_trace(y = ~fishing_ground, name = 'fishing ground', mode = 'lines') %>%
       add_trace(y = ~built_up_land, name = 'built up land', mode = 'lines') %>%
       layout(title = "Total Biocapacity Development",
              xaxis = list(title = "Year"),
              yaxis = list (title = "Biocapacity in global hectares"))
     
     if (input$show_total == TRUE) {
       p <- add_trace(y = ~total, name = 'total', mode = 'lines', p = p)
     }
     
     p
   })
   
   output$changeTitle <- renderText({
     "Changes in the resource types over the selected period"
   })
   
   output$distribution <- renderPlotly({
     #cur_data <- totalBiocapContinent[totalBiocapContinent$UN_region == input$region
      #                                & totalBiocapContinent$year >= input$years[1]
       #                               & totalBiocapContinent$year <= input$years[2], ]
     
     plot_ly(curData(), x = ~grazing_land, name = "Grazing land", type = "box") %>%
       add_trace(x = ~forest_land, name = "Forest land") %>%
       add_trace(x = ~fishing_ground, name = "Fishing ground") %>%
       add_trace(x = ~crop_land, name = "Crop land") %>%
       add_trace(x = ~built_up_land, name = "Built-up land") %>%
       layout(xaxis = list(title = "Biocapacity in global hectares"), 
              title = "Comparison of data ranges for the different resource types")
   })
   
   changeData <- reactive({
     cur_data <- curData()
     cur_data <- cur_data[cur_data$year == input$years[1] | cur_data$year == input$years[2], ]
     
     crop_change <- cur_data$crop_land[2] - cur_data$crop_land[1]
     forest_change <- cur_data$forest_land[2] - cur_data$forest_land[1]
     grazing_change <- cur_data$grazing_land[2] - cur_data$grazing_land[1]
     fishing_change <- cur_data$fishing_ground[2] - cur_data$fishing_ground[1]
     built_change <- cur_data$built_up_land[2] - cur_data$built_up_land[1]
     
     crop_relative <- crop_change / cur_data$crop_land[1]
     forest_relative <- forest_change / cur_data$forest_land[1]
     grazing_relative <- grazing_change / cur_data$grazing_land[1]
     fishing_relative <- fishing_change / cur_data$fishing_ground[1]
     built_relative <- built_change / cur_data$built_up_land[1]
     
     type <- c("Crop", "Forest", "Grazing", "Fishing", "Built up")
     absolute <- c(crop_change, forest_change, grazing_change, fishing_change, built_change)
     relative <- c(crop_relative, forest_relative, grazing_relative, fishing_relative, built_relative)
     
     
     data.frame(type, absolute, relative)
   })
   
   output$absoluteChange <- renderPlotly({
     data <- changeData()
     text <- sprintf("%.0f", data$absolute)
     if (input$dataType == 'Per person') {
       factor <- 1
       suffix <- ""
     } else {
       factor <- 1000000
       suffix <- "M"
     }
     
     text <- paste(formatC(data$absolute/factor, format = "f", big.mark = ",", digits = 2, flag = '+'), suffix)
     plot_ly(data, x = ~type, y = ~absolute, type = 'bar', text = text, textposition = 'auto'
             ) %>%
       layout(yaxis = list(title = "Absolute change of biocapacity in GHA"),
              xaxis = list(title = "Type of biocapacity"))
   })
   output$relativeChange <- renderPlotly({
     data <- changeData()
     text <- paste(formatC(data$relative*100, format = "f", digits = 2, flag = '+'), "%")
     plot_ly(data, x = ~type, y = ~relative*100, type = 'bar', text = ~text,
             textposition = 'auto',
             marker = list(color = 'rgb(158,202,225)',
             line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(yaxis = list(title = "Relative change of biocapacity"),
             xaxis = list(title = "Type of biocapacity"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

