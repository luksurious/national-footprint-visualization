library(shiny)
library(plotly)
library(googleVis)

# Load data
rawData <- read.csv("data/NFA 2018.csv")

# Add ISO-2 country codes to data set
countryCodes <- read.csv("data/country-codes.csv", na.strings = "-----")
countryCodes <- countryCodes[, c("alpha.2", "alpha.3")]
rawData <- merge(rawData, countryCodes, by.x = "ISO.alpha.3.code", by.y = "alpha.3",
                  sort = FALSE, all.x = TRUE)
rawData <- rawData[order(rawData$country, rawData$year, rawData$record), ]

# UI Definition
ui <- navbarPage("National Footprint Visualization",
                 
  #####################
  # Biocapacity
  #####################
  tabPanel("Biocapacity", fluidPage(
    
    #####################
    # Biocapacity trends
    #####################
    tabsetPanel(type = "tabs",
      tabPanel("Trend",
       br(),
       h2("Are there different trends in the evolution of the different biocapacity resources?"),
                             
        sidebarLayout(
          sidebarPanel(
            radioButtons("btRegionType", label = "Type of region", choices = c("Continents", "Countries"),
                         selected = "Continents"),
            conditionalPanel(
              condition = "input.btRegionType == 'Continents'",
              
              selectInput(
                "btRegion",
                label = "Choose the region to show in the chart",
                choices = levels(totalBiocapContinent$UN_region),
                selected = "World"
              )
            ),
            conditionalPanel(
              condition = "input.btRegionType == 'Countries'",
              
              selectInput(
                "btCountry",
                label = "Choose the country to show in the chart",
                choices = levels(totalBiocapPerCountry$country),
                selected = "Spain"
              )
            ),
            
            radioButtons("btDataType", label = "Type of data", choices = c("Per person", "Total"), 
                         selected = "Per person"),
            
            sliderInput("btYears",
                        "Years",
                        min(totalBiocapContinent$year),
                        max(totalBiocapContinent$year),
                        value = c(max(totalBiocapContinent$year)-20, max(totalBiocapContinent$year)),
                        sep = "",
                        step = 1),
            
            checkboxInput("btShow_total", label = "Show totals")
          ),
          
          mainPanel(
            plotlyOutput("plot"),
            h4("Changes in the resource types over the selected period", align = "center"),
            fluidRow(
              splitLayout(cellWidths = c("50%", "50%"), 
                          plotlyOutput("absoluteChange"), plotlyOutput("relativeChange"))
            )
          )
        )
  ),
  
  #####################
  # Biocapacity comparison
  #####################
  tabPanel("Comparison", 
   br(),
   h2("How do different regions compare in biocapacity?"),
   
   
   sidebarLayout(
     sidebarPanel(
       radioButtons("bcRegionType", label = "Type of region", choices = c("Continents", "Countries"),
                    selected = "Continents"),
       conditionalPanel(
         condition = "input.bcRegionType == 'Continents'",
         
         selectInput(
           "bcRegion1",
           label = "Choose the first region to show in the chart",
           choices = levels(totalBiocapContinent$UN_region),
           selected = "Europe"
         ),
         selectInput(
           "bcRegion2",
           label = "Choose the first region to show in the chart",
           choices = levels(totalBiocapContinent$UN_region),
           selected = "Asia"
         )
       ),
       conditionalPanel(
         condition = "input.bcRegionType == 'Countries'",
         
         selectInput(
           "bcCountry1",
           label = "Choose the first country to show in the chart",
           choices = levels(totalBiocapPerCountry$country),
           selected = "Spain"
         ),
         selectInput(
           "bcCountry2",
           label = "Choose the country to show in the chart",
           choices = levels(totalBiocapPerCountry$country),
           selected = "Germany"
         )
       ),
       
       selectInput("bcResourceType", label = "Biocapacity to compare",
                   choices = c("crop_land", "forest_land", "fishing_ground", "built_up_land", "grazing_land", "total"),
                   selected = "crop_land"),
       
       radioButtons("bcDataType", label = "Type of data", choices = c("Per person", "Total"), 
                    selected = "Per person"),
       
       sliderInput("bcYears",
                   "Years",
                   min(totalBiocapContinent$year),
                   max(totalBiocapContinent$year),
                   value = c(max(totalBiocapContinent$year)-20, max(totalBiocapContinent$year)),
                   sep = "",
                   step = 1)
     ),
     
     mainPanel(
       br(), plotlyOutput("biocapComparison"),
       h4("Magnitude comparison of different regions"),
       plotlyOutput("biocapDistribution1")
     )
   )
    
  ),
  
  #####################
  # Spatial comparison
  #####################
  tabPanel("Spatial comparison", 
           
           br(),
           h2("Are there regional patterns of biocapacity?"),
           
           
           sidebarLayout(
             sidebarPanel(
               selectInput("bmResourceType", label = "Biocapacity to compare",
                           choices = c("crop_land", "forest_land", "fishing_ground", "built_up_land", "grazing_land", "total"),
                           selected = "crop_land"),
               
               radioButtons("bmDataType", label = "Type of data", choices = c("Per person", "Total"), 
                            selected = "Per person"),
               
               sliderInput("bmYear",
                           "Year",
                           min(totalBiocapContinent$year),
                           max(totalBiocapContinent$year),
                           value = max(totalBiocapContinent$year),
                           sep = "",
                           step = 1, animate = animationOptions(interval = 1000))
             ),
             
             mainPanel(
              htmlOutput("gvis")
             )
           )
  )
  
  
  
  ), br()
  )),
  tabPanel("...")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #####################
  # Biocapacity
  #####################
  
  # Do some pre-filtering for the biocapacity
  totalBiocapPerCountry <- rawData[rawData$record == "BiocapTotGHA", ]
  CapitaBiocapPerCountry <- rawData[rawData$record == "BiocapPerCap", ]
  
  totalBiocapContinent <- aggregate(
    cbind(crop_land, grazing_land, forest_land, fishing_ground, built_up_land, population, total) ~ year + UN_region,
    totalBiocapPerCountry, sum)
  
  CapitaBiocapContinent <- aggregate(
    cbind(crop_land, grazing_land, forest_land, fishing_ground, built_up_land, population, total) ~ year + UN_region,
    CapitaBiocapPerCountry, sum)
  
  selectBiocapData <- function(regionType, country, region, dataType, years) {
    if (regionType == 'Countries') {
      
      if (dataType == 'Per person') {
        data <- CapitaBiocapPerCountry[CapitaBiocapPerCountry$country == country
                                       & CapitaBiocapPerCountry$year >= years[1]
                                       & CapitaBiocapPerCountry$year <= years[2], ]
      } else {
        data <- totalBiocapPerCountry[totalBiocapPerCountry$country == country
                                      & totalBiocapPerCountry$year >= years[1]
                                      & totalBiocapPerCountry$year <= years[2], ]
      }
    } else {
      data <- totalBiocapContinent[totalBiocapContinent$UN_region == region
                                   & totalBiocapContinent$year >= years[1]
                                   & totalBiocapContinent$year <= years[2], ]
      
      # per continent data per capita is not provided, we need to roughly calculate it
      # because of the large numbers, the precision is not perfect
      if (dataType == 'Per person') {
        data$crop_land <- data$crop_land / data$population
        data$grazing_land = data$grazing_land / data$population
        data$forest_land = data$forest_land / data$population
        data$fishing_ground = data$fishing_ground / data$population
        data$built_up_land = data$built_up_land / data$population
        data$total = data$total / data$population
      }
    }
    
    names(data)[names(data) == "UN_region"] <- "region"
    names(data)[names(data) == "country"] <- "region"
    
    return(data)
  }
  
  #####################
  # Biocapacity Trends
  #####################
  bioCapTrendData <- reactive({
    selectBiocapData(input$btRegionType, input$btCountry, input$btRegion, input$btDataType, input$btYears)
  })  
  
  
  output$plot <- renderPlotly({
    p <-
      plot_ly(
        bioCapTrendData(),
        x = ~ year,
        name = 'crop land',
        y = ~ crop_land,
        type = 'scatter',
        mode = 'lines'
      ) %>%
      add_trace(y = ~ grazing_land,
                name = 'grazing land',
                mode = 'lines') %>%
      add_trace(y = ~ forest_land,
                name = 'forest land',
                mode = 'lines') %>%
      add_trace(y = ~ fishing_ground,
                name = 'fishing ground',
                mode = 'lines') %>%
      add_trace(y = ~ built_up_land,
                name = 'built up land',
                mode = 'lines') %>%
      layout(
        title = "Total Biocapacity Development",
        xaxis = list(title = "Year"),
        yaxis = list (title = "Biocapacity in global hectares")
      )
    
    if (input$btShow_total == TRUE) {
      p <- add_trace(
        y = ~ total,
        name = 'total',
        mode = 'lines',
        p = p
      )
    }
    
    p
  })
   
   #########
   # Biocapacity trend change
   #########
   bioCapChangeData <- reactive({
     cur_data <- bioCapTrendData()
     cur_data <- cur_data[cur_data$year == input$btYears[1] | cur_data$year == input$btYears[2], ]
     
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
     data <- bioCapChangeData()
     text <- sprintf("%.0f", data$absolute)
     if (input$btDataType == 'Per person') {
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
     data <- bioCapChangeData()
     text <- paste(formatC(data$relative*100, format = "f", digits = 2, flag = '+'), "%")
     plot_ly(data, x = ~type, y = ~relative*100, type = 'bar', text = ~text,
             textposition = 'auto',
             marker = list(color = 'rgb(158,202,225)',
             line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(yaxis = list(title = "Relative change of biocapacity"),
             xaxis = list(title = "Type of biocapacity"))
   })
   
   
   #####################
   # Biocapacity comparison
   #####################
   bioCapComparisonData1 <- reactive({
     selectBiocapData(input$bcRegionType, input$bcCountry1, input$bcRegion1, input$bcDataType, input$bcYears)
   })
   bioCapComparisonData2 <- reactive({
     selectBiocapData(input$bcRegionType, input$bcCountry2, input$bcRegion2, input$bcDataType, input$bcYears)
   })
   bioCapComparisonRegion1 <- reactive({
     if (input$bcRegionType == 'Continents') {
       input$bcRegion1
     } else {
       input$bcCountry1
     }
   })
   bioCapComparisonRegion2 <- reactive({
     if (input$bcRegionType == 'Continents') {
       input$bcRegion2
     } else {
       input$bcCountry2
     }
   })
   
   
   output$biocapComparison <- renderPlotly({
     comparison_data <- merge(bioCapComparisonData1()[, c("year", input$bcResourceType)], 
                              bioCapComparisonData2()[, c("year", input$bcResourceType)],
                              by = "year", sort = TRUE, all = TRUE)
     plot_ly(
       comparison_data,
       x = ~ year,
       name = bioCapComparisonRegion1(),
       y = as.formula(sprintf("~ %s.x", input$bcResourceType)),
       type = 'scatter',
       mode = 'lines'
     ) %>%
       add_trace(name = bioCapComparisonRegion2(),
                 y = as.formula(sprintf("~ %s.y", input$bcResourceType)),
                 mode = 'lines') %>%
       layout(
         title = sprintf("Biocapacity Evolution of a %s", input$bcResourceType),
         xaxis = list(title = "Year"),
         yaxis = list (title = "Biocapacity in global hectares")
       )
   })
   
   output$biocapDistribution1 <- renderPlotly({
     data1 <- bioCapComparisonData1()
     data2 <- bioCapComparisonData2()
     
     # restructure data for grouped box plot
     combiData <- rbind(data1, data2)
     
     cdata <- data.frame(region = combiData$region, value = combiData$crop_land)
     cdata$type <- "crop_land"
     gdata <- data.frame(region = combiData$region, value = combiData$grazing_land)
     gdata$type <- "grazing_land"
     fdata <- data.frame(region = combiData$region, value = combiData$forest_land)
     fdata$type <- "forest_land"
     fgdata <- data.frame(region = combiData$region, value = combiData$fishing_ground)
     fgdata$type <- "fishing_ground"
     bdata <- data.frame(region = combiData$region, value = combiData$built_up_land)
     bdata$type <- "built_up_land"
     
     combiData <- rbind(cdata, gdata, fdata, fgdata, bdata)
     
     plot_ly(combiData, x = ~value, y = ~type, color = ~region, type = "box") %>%
       layout(xaxis = list(title = "Biocapacity in GHA"), 
              boxmode = "group")
   })
   
   #####################
   # Spatial comparison
   #####################
   output$gvis <- renderGvis({
     
     if (input$bmDataType == 'Total') {
       cur_data <- totalBiocapPerCountry
     } else {
       cur_data <- CapitaBiocapPerCountry
     }
     cur_data <- cur_data[cur_data$year == input$bmYear, ]
     
     cur_data <- na.omit(cur_data)
     
     gvisGeoChart(cur_data, locationvar = "alpha.2", colorvar = input$bmResourceType,
                  options = list(width="100%", colorAxis = "{colors: ['#F5FDF5', '#267114']}"))
   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

