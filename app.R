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
       resourceTrendUI("bcTrend", "biocapacity")
      ),
  
      #####################
      # Biocapacity comparison
      #####################
      tabPanel("Comparison", 
       br(),
       resourceComparisonUI("bcComp", "Biocapacity")
      )
    ),
    br()
  )),
  
  #####################
  # Footprint
  #####################
  tabPanel("Footprint", fluidPage(
    
    tabsetPanel(type = "tabs",
                
      #####################
      # Footprint trends
      #####################
      tabPanel("Trend",
         br(),
         resourceTrendUI("efTrend", "footprint")
      ),
      
      #####################
      # Footprint comparison
      #####################
      tabPanel("Comparison",
         br(),
         resourceComparisonUI("efComp", "Footprint")
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
  tabPanel("GDP vs Footprint",
    gdpVsEFUI("gdpVsEf")
  ),
  tabPanel("Ecological Deficit",
    deficitTrendUI("deficit")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #####################
  # Biocapacity
  #####################
  # Biocapacity Trends
  callModule(resourceTrend, "bcTrend", selectBiocapData, 'Biocapacity')
  
  # Biocapacity comparison
  callModule(resourceComparison, "bcComp", selectBiocapData, "Biocapacity")
  
  #####################
  # Footprint
  #####################
  # Footprint Trends
  callModule(resourceTrend, "efTrend", selectFootprintData, 'Footprint')
  
  # Footprint comparison
  callModule(resourceComparison, "efComp", selectFootprintData, 'Footprint')
  
  
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)

