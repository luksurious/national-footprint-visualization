

biocapacityComparisonUI <- function (id, biocapData) {
  ns <- NS(id)
  
  tagList(
    h2("How do different regions compare in biocapacity?"),
    
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
            choices = levels(biocapData$UN_region),
            selected = "Europe"
          ),
          selectInput(
            ns("region2"),
            label = "Choose the first region to show in the chart",
            choices = levels(biocapData$UN_region),
            selected = "Asia"
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("regionType"), "'] == 'Countries'"),
          
          selectInput(
            ns("country1"),
            label = "Choose the first country to show in the chart",
            choices = levels(biocapData$country),
            selected = "Spain"
          ),
          selectInput(
            ns("country2"),
            label = "Choose the country to show in the chart",
            choices = levels(biocapData$country),
            selected = "Germany"
          )
        ),
        
        selectInput(
          ns("resourceType"),
          label = "Biocapacity to compare",
          choices = c(
            "crop_land",
            "forest_land",
            "fishing_ground",
            "built_up_land",
            "grazing_land",
            "total"
          ),
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
          min(biocapData$year),
          max(biocapData$year),
          value = c(
            max(biocapData$year) - 20,
            max(biocapData$year)
          ),
          sep = "",
          step = 1
        )
      ),
      
      mainPanel(
        br(),
        plotlyOutput(ns("biocapComparison")),
        h4("Magnitude comparison of different regions"),
        plotlyOutput(ns("biocapDistribution"))
      )
    )
  )
}


biocapacityComparison <- function (input, output, session, selectBiocapData) {
  
  bioCapComparisonData1 <- reactive({
    selectBiocapData(input$regionType, input$country1, input$region1, input$dataType, input$years)
  })
  bioCapComparisonData2 <- reactive({
    selectBiocapData(input$regionType, input$country2, input$region2, input$dataType, input$years)
  })
  bioCapComparisonRegion1 <- reactive({
    if (input$regionType == 'Continents') {
      input$region1
    } else {
      input$country1
    }
  })
  bioCapComparisonRegion2 <- reactive({
    if (input$regionType == 'Continents') {
      input$region2
    } else {
      input$country2
    }
  })
  
  
  output$biocapComparison <- renderPlotly({
    comparison_data <- merge(bioCapComparisonData1()[, c("year", input$resourceType)], 
                             bioCapComparisonData2()[, c("year", input$resourceType)],
                             by = "year", sort = TRUE, all = TRUE)
    plot_ly(
      comparison_data,
      x = ~ year,
      name = bioCapComparisonRegion1(),
      y = as.formula(sprintf("~ %s.x", input$resourceType)),
      type = 'scatter',
      mode = 'lines'
    ) %>%
      add_trace(name = bioCapComparisonRegion2(),
                y = as.formula(sprintf("~ %s.y", input$resourceType)),
                mode = 'lines') %>%
      layout(
        title = sprintf("Biocapacity Evolution of a %s", input$resourceType),
        xaxis = list(title = "Year"),
        yaxis = list (title = "Biocapacity in global hectares")
      )
  })
  
  output$biocapDistribution <- renderPlotly({
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
}