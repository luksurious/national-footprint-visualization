library(plotly)

mapVisualizationUI <- function (id) {
  ns <- NS(id)
  
  tagList(fluidRow(
    column(3,
           radioButtons(
             ns("recordType"),
             label = "Type of data to show",
             choices = c(
               "Ecological Deficit/Reserve",
               "Ecological Footprint",
               "Biocapacity"
             )
           )),
    column(
      3,
      radioButtons(
        ns("dataType"),
        label = "Total of country or per person?",
        choices = c("Total", "Per person")
      )
    ),
    column(
      3,
      conditionalPanel(
        condition = paste0('input["', ns('recordType'), '"] == "Ecological Footprint"'),
        selectInput(
          ns("resourceTypeEF"),
          label = "Resource type to compare",
          choices = c(
            "total",
            "crop_land",
            "forest_land",
            "fishing_ground",
            "built_up_land",
            "grazing_land",
            "carbon"
          ),
          selected = "total"
        )
      ),
      conditionalPanel(
        condition = paste0('input["', ns('recordType'), '"] == "Biocapacity"'),
        selectInput(
          ns("resourceTypeBC"),
          label = "Resource type to compare",
          choices = c(
            "total",
            "crop_land",
            "forest_land",
            "fishing_ground",
            "built_up_land",
            "grazing_land"
          ),
          selected = "total"
        )
      )
    ),
    column(
      3,
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
    )
  ),
  
  plotlyOutput(ns("themap")))
}

mapVisualization <- function (input, output, session) {
  
  mapData <- reactive({
    switch (
      input$recordType,
      "Biocapacity" = {
        if (input$dataType == 'Total') {
          cur_data <- totalBiocapPerCountry
        } else {
          cur_data <- capitaBiocapPerCountry
        }
        
        cur_data <-
          cur_data[cur_data$year == input$year &
                     cur_data$ISO.alpha.3.code != "",]
      },
      "Ecological Footprint" = {
        if (input$dataType == 'Total') {
          fullData <-
            rawData[rawData$record == 'EFConsTotGHA' &
                      rawData$ISO.alpha.3.code != "",]
          cur_data <- fullData[fullData$year == input$year,]
        } else {
          fullData <-
            rawData[rawData$record == 'EFConsPerCap' &
                      rawData$ISO.alpha.3.code != "",]
          cur_data <-
            rawData[rawData$record == 'EFConsPerCap' &
                      rawData$year == input$year & rawData$ISO.alpha.3.code != "",]
        }
      },
      "Ecological Deficit/Reserve" = {
        cur_data <- deficitData("Countries", input$dataType)
        
        cur_data <-
          cur_data[cur_data$year == input$year &
                     cur_data$ISO.alpha.3.code.x != "",]
        names(cur_data)[names(cur_data) == "ISO.alpha.3.code.x"] <- "ISO.alpha.3.code"
      }
    )
    
    cur_data
  })
  
  output$themap <- renderPlotly({
    zmid <- FALSE
    zmax <- FALSE
    zmin <- FALSE
    
    cur_data <- mapData()
    
    color <- switch (input$recordType,
      "Biocapacity" = "Greens",
      "Ecological Footprint" = "Oranges",
      "Ecological Deficit/Reserve" = "RdBu"
    )
    dataCol <- switch (input$recordType,
                     "Biocapacity" = input$resourceTypeBC,
                     "Ecological Footprint" = input$resourceTypeEF,
                     "Ecological Deficit/Reserve" = "diff"
    )
    
    if (input$recordType == "Ecological Deficit/Reserve") {
      zmid <- 0
      zmax <- min(abs(max(cur_data$diff)), abs(min(cur_data$diff)))
      zmin <- -zmax
    }
    
    g <- list(
      scope = 'world',
      showframe = FALSE,
      showcoastlines = TRUE,
      coastlinecolor = toRGB("grey")
    )
    
    l <- list(color = toRGB("grey"), width = 0.5)
    
    plot_geo(cur_data, height = 500) %>%
      add_trace(
        z = as.formula(paste0("~", dataCol)),
        locations = ~ ISO.alpha.3.code,
        color = as.formula(paste0("~", dataCol)),
        colors = color,
        marker = list(line = l),
        zmid = zmid,
        zmax = zmax,
        zmin = zmin
      ) %>%
      colorbar(
        nticks = 10,
        len = 1,
        title = "",
        ticksuffix = " GHA"
      ) %>%
      layout(title = '', geo = g)
  })
}