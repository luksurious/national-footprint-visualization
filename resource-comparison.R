library(plotly)

resourceComparisonUI <- function (id) {
  ns <- NS(id)
  
  resources <- c(
    "Crop land" = "crop_land",
    "Forest land" = "forest_land",
    "Fishing ground" = "fishing_ground",
    "Built up land" = "built_up_land",
    "Grazing land" = "grazing_land",
    "Carbon emissions (Footprint only)" = "carbon",
    "Total resources" = "total"
  )
  
  tagList(sidebarLayout(
    sidebarPanel(
      radioButtons(
        ns("recordType"),
        label = "Data",
        choices = c("Biocapacity", "Footprint"),
        selected = "Biocapacity"
      ),
      
      radioButtons(
        ns("regionType"),
        label = "Type of region",
        choices = c("Continents", "Countries"),
        selected = "Continents"
      ),
      conditionalPanel(
        condition = paste0("input['", ns("regionType"), "'] == 'Continents'"),
        
        selectInput(
          ns("region1"),
          label = "Choose the first region to show in the chart",
          choices = dataRegions,
          selected = "Europe"
        ),
        selectInput(
          ns("region2"),
          label = "Choose the first region to show in the chart",
          choices = dataRegions,
          selected = "Asia"
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("regionType"), "'] == 'Countries'"),
        
        selectInput(
          ns("country1"),
          label = "Choose the first country to show in the chart",
          choices = dataCountries,
          selected = "Spain"
        ),
        selectInput(
          ns("country2"),
          label = "Choose the country to show in the chart",
          choices = dataCountries,
          selected = "Germany"
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("resCompTab"), "'] == 'Trend'"),
        selectInput(
          ns("resourceType"),
          label = "Resource to compare",
          choices = resources,
          selected = "crop_land"
        )
      ),
      
      radioButtons(
        ns("dataType"),
        label = "Type of data",
        choices = c("Per person", "Total"),
        selected = "Per person"
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("resCompTab"), "'] == 'Resource characteristics'"),
        
        sliderInput(
          ns("year"),
          "Year",
          dataYears[1],
          dataYears[2],
          value = dataYears[2],
          sep = "",
          step = 1,
          animate = animationOptions(interval = 300)
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("resCompTab"), "'] != 'Resource characteristics'"),
        
        sliderInput(
          ns("years"),
          "Years",
          dataYears[1],
          dataYears[2],
          value = c(dataYears[1] - 20,
                    dataYears[2]),
          sep = "",
          step = 1
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          'Trend',
                 
           h2("How do different regions compare in their resources?"),
           br(),
           plotlyOutput(ns("resourceComparison"))
        ),
        tabPanel(
          'Resource characteristics',
          
          h2("How do regions compare in their resources?"),
          plotlyOutput(ns("resourceRadar"))
        ),
        tabPanel(
          'Distribution',
          
          h2("How do regions compare in magnitude and distribution?"),
          plotlyOutput(ns("resourceDistribution"))
        ),
        id = ns('resCompTab'),
        type = 'pills'
      )
    )
  ))
}


resourceComparison <- function (input, output, session) {
  resourceComparisonData1 <- reactive({
    if (input$recordType == 'Footprint') {
      selectFootprintData(input$regionType,
                          input$country1,
                          input$region1,
                          input$dataType,
                          input$years)
    } else {
      selectBiocapData(input$regionType,
                       input$country1,
                       input$region1,
                       input$dataType,
                       input$years)
    }
  })
  
  resourceComparisonData2 <- reactive({
    if (input$recordType == 'Footprint') {
      selectFootprintData(input$regionType,
                          input$country2,
                          input$region2,
                          input$dataType,
                          input$years)
    } else {
      selectBiocapData(input$regionType,
                       input$country2,
                       input$region2,
                       input$dataType,
                       input$years)
    }
  })
  
  resourceComparisonRegion1 <- reactive({
    if (input$regionType == 'Continents') {
      input$region1
    } else {
      input$country1
    }
  })
  
  resourceComparisonRegion2 <- reactive({
    if (input$regionType == 'Continents') {
      input$region2
    } else {
      input$country2
    }
  })
  
  
  mergedComparisonData <- reactive({
    data1 <- resourceComparisonData1()
    data2 <- resourceComparisonData2()
    
    merge(
      data1[, c("year", input$resourceType)],
      data2[, c("year", input$resourceType)],
      by = "year",
      sort = TRUE,
      all = TRUE
    )
  })
  
  output$resourceComparison <- renderPlotly({
    comparison_data <- mergedComparisonData()
    
    plot_ly(
      comparison_data,
      x = ~ year,
      name = resourceComparisonRegion1(),
      y = as.formula(sprintf("~ %s.x", input$resourceType)),
      type = 'scatter',
      mode = 'lines'
    ) %>%
      add_trace(name = resourceComparisonRegion2(),
                y = as.formula(sprintf("~ %s.y", input$resourceType)),
                mode = 'lines') %>%
      layout(
        title = sprintf("%s Evolution of %s", input$recordType, input$resourceType),
        xaxis = list(title = "Year"),
        yaxis = list (title = paste0(
          input$recordType, " in global hectares"
        ))
      )
  })
  
  distributionData <- reactive({
    data1 <- resourceComparisonData1()
    data2 <- resourceComparisonData2()
    
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
    
    if (input$recordType == 'Footprint') {
      codata <- data.frame(region = combiData$region, value = combiData$carbon)
      codata$type <- "carbon"
      
      combiData <- rbind(cdata, gdata, fdata, fgdata, bdata, codata)
    } else {
      combiData <- rbind(cdata, gdata, fdata, fgdata, bdata)
    }
    
    combiData
  })
  
  output$resourceDistribution <- renderPlotly({
    plot_ly(
      distributionData(),
      x = ~ value,
      y = ~ type,
      color = ~ region,
      type = "box"
    ) %>%
      layout(xaxis = list(title = paste0(input$recordType, " in GHA")),
             boxmode = "group")
  })
  
  radarData <- function (data, year, record) {
    data <- data[data$year == year, ]
    dataR <-
      c(
        data$crop_land,
        data$forest_land,
        data$fishing_ground,
        data$grazing_land,
        data$built_up_land,
        data$crop_land
      )
    if (record == 'Footprint') {
      dataR <-
        c(
          data$crop_land,
          data$forest_land,
          data$fishing_ground,
          data$grazing_land,
          data$built_up_land,
          data$carbon,
          data$crop_land
        )
    }
    
    return(dataR)
  }
  
  radarData1 <- reactive({
    radarData(resourceComparisonData1(), input$year, input$recordType)
  })
  radarData2 <- reactive({
    radarData(resourceComparisonData2(), input$year, input$recordType)
  })
  
  output$resourceRadar <- renderPlotly({
    
    theta <-
      c(
        'crop_land',
        'forest_land',
        'fishing_ground',
        'grazing_land',
        'built_up_land',
        'crop_land'
      )
    if (input$recordType == 'Footprint') {
      theta <-
        c(
          'crop_land',
          'forest_land',
          'fishing_ground',
          'grazing_land',
          'built_up_land',
          'carbon',
          'crop_land'
        )
    }
    
    plot_ly(
      type = 'scatterpolar',
      r = radarData1(),
      theta = theta,
      mode = 'lines+markers',
      name = resourceComparisonRegion1()
    ) %>% add_trace(r = radarData2(),
                    theta = theta,
                    name = resourceComparisonRegion2())
  })
}