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

data <- read.csv("data/NFA 2018.csv")

totalBiocapPerCountry <- data[data$record == "BiocapTotGHA", ]

totalBiocapContinent <- aggregate(
  cbind(crop_land, grazing_land, forest_land, fishing_ground, built_up_land, population, total) ~ year + UN_region,
  totalBiocapPerCountry, sum)

totalBiocapWorld <- totalBiocapPerCountry[totalBiocapPerCountry$country == "World", ]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("National Footprint Visualization"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        helpText("Visualize the development of the biocapacity"),
        
        selectInput(
          "region",
          label = "Choose the region to show in the chart",
          choices = levels(totalBiocapContinent$UN_region)
        ),
        
        checkboxInput("show_total", label = "Show totals")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("plot"),
         verbatimTextOutput("event")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlotly({
     cur_data <- totalBiocapContinent[totalBiocapContinent$UN_region == input$region, ]
     
     p <- plot_ly(cur_data, x = ~year, name = 'crop land', y = ~crop_land, type = 'scatter', mode = 'lines') %>%
       add_trace(y = ~grazing_land, name = 'grazing land', mode = 'lines') %>%
       add_trace(y = ~forest_land, name = 'forest land', mode = 'lines') %>%
       add_trace(y = ~fishing_ground, name = 'fishing ground', mode = 'lines') %>%
       add_trace(y = ~built_up_land, name = 'built up land', mode = 'lines') %>%
       layout(title = "Total Biocapacity Development",
              xaxis = list(title = "Year"),
              yaxis = list (title = "Biocapacity in hectares"))
     
     if (input$show_total == TRUE) {
       p <- add_trace(y = ~total, name = 'total', mode = 'lines', p = p)
     }
     
     p
   })
   
   output$event <- renderPrint({
     d <- event_data("plotly_hover")
     if (is.null(d)) "Hover on a point!" else d
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

