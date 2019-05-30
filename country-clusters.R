

footprintTotal <-
  rawData[rawData$country != 'World' &
            rawData$record == "EFConsTotGHA", ]
footprintPerPerson <-
  rawData[rawData$country != 'World' &
            rawData$record == "EFConsPerCap", ]

countryClusterUI <- function (id) {
  ns <- NS(id)
  
  footprintType <-
    c(
      "crop_land",
      "forest_land",
      "grazing_land",
      "built_up_land",
      "fishing_ground",
      "carbon",
      "total"
    )
  
  tagList(pageWithSidebar(
    headerPanel(
      'Which groups of countries exist with a similar footprint relation?'
    ),
    sidebarPanel(
      selectInput(ns('xcol'), 'First footprint type:', footprintType, selected = "forest_land"),
      selectInput(
        ns('ycol'),
        'Second footprint type:',
        footprintType,
        selected = "crop_land"
      ),
      radioButtons(
        ns("datatype"),
        "Type of the data",
        choices = c("Total", "Per person"),
        selected = "Per person"
      ),
      numericInput(
        ns('clusters'),
        'Cluster count:',
        3,
        min = 1,
        max = 9
      ),
      sliderInput(
        ns("year"),
        "Year:",
        min = dataYears[1],
        max = dataYears[2],
        value = dataYears[2],
        sep = "",
        step = 1
      )
      
      
    ),
    mainPanel(plotlyOutput(ns('plot1')),
              tableOutput(ns("values")))
  ))
}

countryCluster <- function (input, output, session) {
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    if (input$datatype == 'Total')
      selectdata <- footprintTotal
    else
      selectdata <- footprintPerPerson
    
    
    na.omit(selectdata[selectdata[, 'year'] == input$year, ][, c(input$xcol, input$ycol, "country")])
    
  })
  
  clusters <- reactive({
    clusterData <- selectedData()
    
    kmeans(clusterData[, c(input$xcol, input$ycol)], input$clusters)
  })
  
  
  output$plot1 <- renderPlotly({
    palette <-
      c(
        "#EA2027",
        "#0652DD",
        "#FFC312",
        "#009432",
        "#FDA7DF",
        "#1B1464",
        "#C4E538",
        "#6F1E51",
        "#12CBC4"
      )
    clusters <- clusters()
    
    theData <-
      data.frame(selectedData(), cluster = factor(clusters$cluster))
    
    clusterCenters <- as.data.frame(clusters$centers)
    
    
    plot_ly(theData, mode = "markers", type = "scatter") %>%
      add_trace(
        data = theData,
        x = as.formula(paste0("~", input$xcol)),
        y = as.formula(paste0("~", input$ycol)),
        text = ~ paste("Country: ", country),
        color = ~ cluster,
        colors = palette[1:input$clusters]
      ) %>%
      add_trace(
        data = clusterCenters,
        x = as.formula(paste0("~", input$xcol)),
        y = as.formula(paste0("~", input$ycol)),
        
        
        marker = list(
          symbol = "x",
          size = 12,
          color = "black",
          opacity = 0.7
        ),
        name = "Cluster centers"
      )
  })
  
  sliderValues <- reactive({
    clusters <- clusters()
    theData <-
      data.frame(selectedData(), cluster = factor(clusters$cluster))
    a <- c()
    b <- c()
    n <- input$clusters
    
    for (i in 1:n - 1) {
      name <- paste("Cluster", toString(i + 1))
      
      b <- c(b, name)
      
      clu <- theData[theData$cluster == toString(i + 1), ]
      Countries = as.character(paste(clu$country, collapse = ","))
      a <- c(a, Countries)
    }
    
    
    data.frame(
      Cluster = b,
      
      Countries = a,
      stringsAsFactors = FALSE
    )
    
  })
  
  output$values <- renderTable({
    sliderValues()
  })
}