library(shiny)

# Prepare data
source("./load-data.R", local = TRUE)
source("./biocapacity-data.R", local = TRUE)

# Load modules
source("./biocapacity-trend.R", local = TRUE)
source("./biocapacity-comparison.R", local = TRUE)

source("./map.R", local = TRUE)

source("./carbon.R", local = TRUE)

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
       biocapacityTrendUI("bcTrend")
      ),
  
      #####################
      # Biocapacity comparison
      #####################
      tabPanel("Comparison", 
       br(),
       biocapacityComparisonUI("bcComp")
      )
    ),
    br()
  )),
  tabPanel("Carbon emissions",
    carbonEmissionsUI("carbon")
  ),
  tabPanel("Map",
    mapVisualizationUI("map")
  ),
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
  
  
  #####################
  # Carbon Emissions
  #####################
  callModule(carbonEmissions, "carbon")
  
  #####################
  # Map Visualization
  #####################
  callModule(mapVisualization, "map")
  
}

# Run the application 
shinyApp(ui = ui, server = server)

