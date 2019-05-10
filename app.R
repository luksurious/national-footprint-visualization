library(shiny)
library(plotly)
library(googleVis)

# Prepare data
source("./load-data.R", local = TRUE)
source("./biocapacity-data.R", local = TRUE)

# Load modules
source("./biocapacity-trend.R", local = TRUE)
source("./biocapacity-comparison.R", local = TRUE)
source("./biocapacity-map.R", local = TRUE)


# UI Definition
ui <- navbarPage("National Footprint Visualization",
                 
  #####################
  # Biocapacity
  #####################
  tabPanel("Biocapacity", fluidPage(

    tabsetPanel(type = "tabs",
                
      #####################
      # Biocapacity trends
      #####################
      tabPanel("Trend",
       br(),
       biocapacityTrendUI("bcTrend", totalBiocapPerCountry)
      ),
  
      #####################
      # Biocapacity comparison
      #####################
      tabPanel("Comparison", 
       br(),
       biocapacityComparisonUI("bcComp", totalBiocapPerCountry)
      ),
    
      #####################
      # Spatial comparison
      #####################
      tabPanel("Spatial comparison", 
        br(),
        biocapacityMapUI("bcMap", totalBiocapPerCountry)
      )
    ),
    br()
  )),
  tabPanel("...")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #####################
  # Biocapacity
  #####################
  # Biocapacity Trends
  callModule(biocapacityTrend, "bcTrend", selectBiocapData)
  
  # Biocapacity comparison
  callModule(biocapacityComparison, "bcComp", selectBiocapData)
   
  # Spatial comparison
  callModule(biocapacityMap, "bcMap")
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

