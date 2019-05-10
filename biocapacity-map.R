library(googleVis)
library(plotly)

biocapacityMapUI <- function (id, biocapData) {
  ns <- NS(id)
  
  tagList(
    h2("Are there regional patterns of biocapacity?"),
    
    
    sidebarLayout(
      sidebarPanel(
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
          ns("year"),
          "Year",
          min(biocapData$year),
          max(biocapData$year),
          value = max(biocapData$year),
          sep = "",
          step = 1,
          animate = animationOptions(interval = 300)
        )
      ),
      
      mainPanel(
        plotlyOutput(ns("map"))
        #,
        #htmlOutput(ns("gvis"))
      )
    )
  )
}

biocapacityMap <- function (input, output, session) {
  
  cur_data <- reactive({
    if (input$dataType == 'Total') {
      cur_data <- totalBiocapPerCountry
    } else {
      cur_data <- capitaBiocapPerCountry
    }
    cur_data <- cur_data[cur_data$year == input$year, ]
    
    #cur_data <- na.omit(cur_data)
    
    cur_data
  })
  
  output$gvis <- renderGvis({
    gvisGeoChart(cur_data(), locationvar = "alpha.2", colorvar = input$resourceType,
                 options = list(width="100%", colorAxis = "{colors: ['#F5FDF5', '#267114']}"))
  })
  
  output$map <- renderPlotly({
    g <- list(
      scope = 'world',
      showframe = FALSE,
      showcoastlines = TRUE,
      coastlinecolor = toRGB("grey")
    )
    
    l <- list(color = toRGB("grey"), width = 0.5)
    
    plot_geo(cur_data()) %>%
      add_trace(
        z = as.formula(paste0("~", input$resourceType)),
        locations = ~ ISO.alpha.3.code,
        color = as.formula(paste0("~", input$resourceType)),
        colors = "Greens",
        marker = list(line = l)
      ) %>%
      layout(title = '', geo = g)
  })
}