library(shiny)

# Prepare data
source("./load-data.R", local = TRUE)
source("./prepare-data.R", local = TRUE)

# Load modules
source("./resource-trend.R", local = TRUE)
source("./resource-comparison.R", local = TRUE)

source("./map.R", local = TRUE)

source("./carbon.R", local = TRUE)

source("./gdp-vs-ef.R", local = TRUE)

source("./deficit.R", local = TRUE)

source("./country-clusters.R", local = TRUE)

# UI Definition
ui <- navbarPage(
  "National Footprint Visualization",
  
  #####################
  # World map
  #####################
  tabPanel("Map",
    mapVisualizationUI("map")
  ),               
                 
  #####################
  # Biocapacity & Footprint
  #####################
  tabPanel("Biocapacity & Footprint", fluidPage(

    tabsetPanel(type = "tabs",
      tabPanel("History",
       br(),
       resourceTrendUI("bcTrend")
      ),
      
      tabPanel("Comparison", 
       br(),
       resourceComparisonUI("bcComp")
      ),
      
      tabPanel("Ecological Deficit",
        deficitTrendUI("deficit")
      )
    ),
    br()
  )),
  
  tabPanel("Carbon emissions",
    carbonEmissionsUI("carbon")
  ),
  tabPanel("Footprint clusters",
    countryClusterUI("cluster")
  ),
  tabPanel("GDP vs Footprint",
    gdpVsEFUI("gdpVsEf")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #####################
  # Biocapacity & Footprint
  #####################
  # Trends
  callModule(resourceTrend, "bcTrend")
  
  # comparison
  callModule(resourceComparison, "bcComp")
  
  #####################
  # Carbon Emissions
  #####################
  callModule(carbonEmissions, "carbon")
  
  #####################
  # Map Visualization
  #####################
  callModule(mapVisualization, "map")
  
  
  #####################
  # GDP vs Footprint Visualization
  #####################
  callModule(gdpVsEF, "gdpVsEf")
  
  #####################
  # Deficit Visualization
  #####################
  callModule(deficitTrend, "deficit")
  
  
  callModule(countryCluster, "cluster")
}

# Run the application 
shinyApp(ui = ui, server = server)

