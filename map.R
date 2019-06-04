library(plotly)

mapVisualizationUI <- function (id) {
  ns <- NS(id)
  
  tagList(
    h2("Are geographic patterns visible for different biocapacity, footprint resources or the ecological deficit?"),
    fluidRow(
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
        choices = c("Per person", "Total")
      )
    ),
    column(
      3,
      conditionalPanel(
        condition = 'input.recordType == "Ecological Footprint"',
        ns = ns,
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
        condition = 'input.recordType == "Biocapacity"',
        ns = ns,
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
      ),
      conditionalPanel(
        condition = 'input.recordType == "Ecological Deficit/Reserve"',
        ns = ns,
        radioButtons(
          ns("colorScaleED"),
          label = "Select colorscale",
          choices = c(
            "Semantic segmented (green-red)" = "green-red",
            "Colorblind friendly (green-purple)" = "green-purple",
            "Semantic continuous (green-red)" = "continuous"
          )
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
        animate = animationOptions(interval = 400)
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
  
  divergingColorscale <- reactive({
    if (input$colorScaleED == 'green-red') {
      list(
        list(0, "#67001f"), 
        list(0.1, "#67001f"),
        list(0.10000001, "#b2182b"), 
        list(0.2, "#b2182b"), 
        list(0.20000001, "#d6604d"), 
        list(0.35, "#d6604d"), 
        list(0.35000001, "#f4a582"), 
        list(0.45, "#f4a582"), 
        list(0.45000001, "#fddbc7"), 
        list(0.49999999, "#fddbc7"), 
        list(0.5, "#F5F5F5"), 
        list(0.50000001, "#d9f0d3"), 
        list(0.54999999, "#d9f0d3"), 
        list(0.55, "#a6dba0"),
        list(0.64999999, "#a6dba0"),
        list(0.65, "#5aae61"),
        list(0.79999999, "#5aae61"),
        list(0.8, "#1b7837"),
        list(0.89999999, "#1b7837"),
        list(0.9, "#00441b"),
        list(1, "#00441b")
      )
    } else if (input$colorScaleED == 'continuous') {
      list(list(0, "#CD0000"), list(0.495, "#F2BFBF"), list(0.5, "#F5F5F5"), list(0.505, "#D3DCC4"), list(1, "#517212"))
    } else {
      list(
        list(0, "#40004b"), 
        list(0.1, "#40004b"),
        list(0.10000001, "#762a83"), 
        list(0.2, "#762a83"), 
        list(0.20000001, "#9970ab"), 
        list(0.35, "#9970ab"), 
        list(0.35000001, "#c2a5cf"), 
        list(0.45, "#c2a5cf"), 
        list(0.45000001, "#e7d4e8"), 
        list(0.49999999, "#e7d4e8"), 
        list(0.5, "#F5F5F5"), 
        list(0.50000001, "#d9f0d3"), 
        list(0.54999999, "#d9f0d3"), 
        list(0.55, "#a6dba0"),
        list(0.64999999, "#a6dba0"),
        list(0.65, "#5aae61"),
        list(0.79999999, "#5aae61"),
        list(0.8, "#1b7837"),
        list(0.89999999, "#1b7837"),
        list(0.9, "#00441b"),
        list(1, "#00441b")
      )
    }
  })
  
  output$themap <- renderPlotly({
    zmid <- FALSE
    zmax <- FALSE
    zmin <- FALSE
    reverse <- FALSE
    
    cur_data <- mapData()
    
    color <- switch (input$recordType,
      "Biocapacity" = "Greens",
      "Ecological Footprint" = "Oranges",
      "Ecological Deficit/Reserve" = divergingColorscale()
    )
    dataCol <- switch (input$recordType,
                     "Biocapacity" = input$resourceTypeBC,
                     "Ecological Footprint" = input$resourceTypeEF,
                     "Ecological Deficit/Reserve" = "diff"
    )
    
    if (input$recordType == "Ecological Deficit/Reserve") {
      zmid <- 0
      zmax <- floor(min(abs(max(cur_data$diff)), abs(min(cur_data$diff))))
      zmin <- -zmax
      
      cur_data$bin <- cut(cur_data$diff, breaks = c(-Inf, seq(zmin, zmax, length.out = 11), Inf), labels = 1:12)
      
    }
    if (input$recordType == "Biocapacity") {
      reverse <- TRUE
    }
    
    g <- list(
      scope = 'world',
      projection = list(type = 'Mercator'),
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
        #color = ~bin,
        colorscale = color,
        reversescale = reverse,
        marker = list(line = l),
        zmid = zmid,
        zmax = zmax,
        zmin = zmin
      ) %>%
      colorbar(
        len = 1,
        title = "",
        ticksuffix = " GHA"
      ) %>%
      layout(title = '', geo = g)
  })
}