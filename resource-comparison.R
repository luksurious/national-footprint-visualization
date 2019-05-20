library(plotly)

resourceComparisonUI <- function (id, record) {
  ns <- NS(id)
  
  resources <- c(
    "crop_land",
    "forest_land",
    "fishing_ground",
    "built_up_land",
    "grazing_land",
    "total"
  )
  if (record == 'Footprint') {
    resources[6] <- 'carbon'
    resources[7] <- 'total'
  }
  
  tagList(
    h2(paste0("How do different regions compare in their ", record, "?")),
    
    sidebarLayout(
      sidebarPanel(
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
        
        selectInput(
          ns("resourceType"),
          label = "Resource to compare",
          choices = resources,
          selected = "crop_land"
        ),
        
        radioButtons(
          ns("dataType"),
          label = "Type of data",
          choices = c("Per person", "Total"),
          selected = "Per person"
        ),
        
        sliderInput(
          ns("years"),
          "Years",
          dataYears[1],
          dataYears[2],
          value = c(
            dataYears[1] - 20,
            dataYears[2]
          ),
          sep = "",
          step = 1
        )
      ),
      
      mainPanel(
        br(),
        plotlyOutput(ns("resourceComparison")),
        h4("Magnitude comparison of different regions"),
        plotlyOutput(ns("resourceDistribution"))
      )
    )
  )
}


resourceComparison <- function (input, output, session, selectResourceData, record) {
  
  resourceComparisonData1 <- reactive({
    selectResourceData(input$regionType, input$country1, input$region1, input$dataType, input$years)
  })
  resourceComparisonData2 <- reactive({
    selectResourceData(input$regionType, input$country2, input$region2, input$dataType, input$years)
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
  
  
  output$resourceComparison <- renderPlotly({
    comparison_data <- merge(resourceComparisonData1()[, c("year", input$resourceType)], 
                             resourceComparisonData2()[, c("year", input$resourceType)],
                             by = "year", sort = TRUE, all = TRUE)
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
        title = sprintf("%s Evolution of %s", record, input$resourceType),
        xaxis = list(title = "Year"),
        yaxis = list (title = paste0(record, " in global hectares"))
      )
  })
  
  output$resourceDistribution <- renderPlotly({
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
    
    if (record == 'Footprint') {
      codata <- data.frame(region = combiData$region, value = combiData$carbon)
      codata$type <- "carbon"
      
      combiData <- rbind(cdata, gdata, fdata, fgdata, bdata, codata)
    } else {
      combiData <- rbind(cdata, gdata, fdata, fgdata, bdata)
    }
    
    plot_ly(combiData, x = ~value, y = ~type, color = ~region, type = "box") %>%
      layout(xaxis = list(title = paste0(record, " in GHA")), 
             boxmode = "group")
  })
}