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
        condition = "input.regionType == 'Continents'",
        ns = ns,
        selectInput(
          ns("region1"),
          label = "First region",
          choices = dataRegions,
          selected = "Europe"
        ),
        selectInput(
          ns("region2"),
          label = "Second region",
          choices = dataRegions,
          selected = "Asia"
        )
      ),
      conditionalPanel(
        condition = "input.regionType == 'Countries'",
        ns = ns,
        selectInput(
          ns("country1"),
          label = "First country",
          choices = dataCountries,
          selected = "Spain"
        ),
        selectInput(
          ns("country2"),
          label = "Second country",
          choices = dataCountries,
          selected = "Germany"
        )
      ),
      
      conditionalPanel(
        condition = "input.resCompTab == 'Trend'",
        ns = ns,
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
        condition = "input.resCompTab == 'Resource values'",
        ns = ns,
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
        condition = "input.resCompTab != 'Resource values'",
        ns = ns,
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
      ),
      
      radioButtons(
        ns("colors"),
        "Color set",
        choices = c("Colorbrewer Set2" = "Set2", "Colorbrewer Dark2" = "Dark2", "Plotly defaults" = "plotly")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          'Trend',
                 
           h2("How do different regions compare in their resource trends?"),
           br(),
           plotlyOutput(ns("resourceComparison"))
        ),
        tabPanel(
          'Resource values',
          
          h2("How do regions compare in their resource values?"),
          fluidRow(
            column(
              5,
              plotlyOutput(ns("resourceRadar"))
            ),
            column(
              7,
              plotlyOutput(ns("resourceGroupBar"))
            )
          )
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
  
  colors <- reactive({
    switch (input$colors,
            "Set2" = categoricalSet2Colors9[1:2],
            "Dark2" = categoricalDark2Colors8[1:2],
            "plotly" = plotlyColors[1:2]
    )
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
    colors <- colors()
    
    plot_ly(
      comparison_data,
      x = ~ year,
      name = resourceComparisonRegion1(),
      y = as.formula(sprintf("~ %s.x", input$resourceType)),
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = colors[1]),
      marker = list(size = 4, color = colors[1])
    ) %>%
      add_trace(name = resourceComparisonRegion2(),
                y = as.formula(sprintf("~ %s.y", input$resourceType)),
                line = list(color = colors[2]),
                marker = list(color = colors[2])) %>%
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
    
    combiData$region <- droplevels(combiData$region)
    
    combiData
  })
  
  output$resourceDistribution <- renderPlotly({
    colors <- colors()
    plot_ly(
      distributionData(),
      x = ~ value,
      y = ~ type,
      color = ~ region,
      colors = setNames(c(colors[1], colors[2]), c(resourceComparisonRegion1(), resourceComparisonRegion2())),
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
    colors <- colors()
    
    plot_ly(
      type = 'scatterpolar',
      r = radarData1(),
      theta = theta,
      showlegend = FALSE,
      line = list(color = colors[1]),
      marker = list(color = colors[1]),
      mode = 'lines+markers',
      name = resourceComparisonRegion1()
    ) %>% add_trace(r = radarData2(),
                    theta = theta,
                    line = list(color = colors[2]),
                    marker = list(color = colors[2]),
                    name = resourceComparisonRegion2())
  })
  
  output$resourceGroupBar <- renderPlotly({
    cols <- c(
      'crop_land',
      'forest_land',
      'fishing_ground',
      'grazing_land',
      'built_up_land',
      'crop_land'
    )
    if (input$recordType == 'Footprint') {
      cols <-
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
    dataF <- data.frame(cols, first = radarData1(), second = radarData2())
    colors <- colors()
    
    dataF$cols = with(dataF, factor(cols, levels = rev(levels(cols))))
    
    plot_ly(
      type = 'bar',
      orientation = 'h',
      data = dataF,
      y = ~ cols,
      x = ~ first,
      marker = list(color = colors[1]),
      name = resourceComparisonRegion1()
    ) %>% add_trace(
      x = ~ second,
      marker = list(color = colors[2]),
      name = resourceComparisonRegion2()
    ) %>% layout(
      xaxis = list(title = "Resource usage/availability in GHA"),
      yaxis = list(title = "Resource type"),
      legend = list(orientation = 'h', y = 100, xanchor = 'left')
    )
  })
}