library(plotly)

source("./number-format.R")

mapVisualizationUI <- function (id) {
  ns <- NS(id)
  
  tagList(
    h2("Are geographic patterns visible for different biocapacity, footprint resources or the ecological deficit?"),
    fluidRow(
    column(
      2,
     radioButtons(
       ns("recordType"),
       label = "Type of data to show",
       choices = c(
         "Ecological Deficit/Reserve",
         "Ecological Footprint",
         "Biocapacity"
       )
     )
    ),
    column(
      2,
      radioButtons(
        ns("dataType"),
        label = "",
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
            "Semantic continuous (green-red)" = "continuous",
            "Colorblind friendly (green-purple)" = "green-purple"
          )
        )
      )
    ),
    column(
      2, 
      radioButtons(ns("rangeCut"), "Min/Max of colorscale", choices = c(
        "100% quant. (show extremes)" = "1", "99% quant. (show outliers)" = "0.99", "95% quant. (ignore outliers)" = "0.95"), selected = "0.99")
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
  
  plotlyOutput(ns("themap"), height = 600)
  
  )
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
  
  sequentialColorscaleGreen <- sequentialColorscale(sequentialGreens9)
  sequentialColorscaleRed <- sequentialColorscale(sequentialReds9)
  
  divergingContinuousColorscale <- divergingContinuousColorscale(divergingContinuousGreenRedSemanticColorScale)
  
  divergingColorscale <- reactive({
    if (input$colorScaleED == 'continuous') {
      divergingContinuousColorscale
    } else {
      colorMap <- switch(input$colorScaleED,
                         "green-red" = divergingGreenRedSemanticColorScale11,
                         "green-purple" = divergingGreenPurpleColorScale11)
      divergingSegmentedColorscale(colorMap)
    }
  })
  
  output$themap <- renderPlotly({
    zmid <- FALSE
    zmax <- FALSE
    zmin <- FALSE
    ticks <- c()
    ticktext <- c()
    
    cur_data <- mapData()
    
    color <- switch (input$recordType,
      "Biocapacity" = sequentialColorscaleGreen,
      "Ecological Footprint" = sequentialColorscaleRed,
      "Ecological Deficit/Reserve" = divergingColorscale()
    )
    dataCol <- switch (input$recordType,
                     "Biocapacity" = input$resourceTypeBC,
                     "Ecological Footprint" = input$resourceTypeEF,
                     "Ecological Deficit/Reserve" = "diff"
    )
    
    cur_data <- cur_data[!is.na(cur_data[dataCol]),]
    
    rangeCut <- as.numeric(input$rangeCut)
    
    if (input$recordType == "Ecological Deficit/Reserve") {
      zmid <- 0
     
      quants <- quantile(cur_data$diff, c(1-rangeCut, rangeCut))
      zmax <- max(abs(quants[1]), abs(quants[2]))
      zmin <- -zmax
      
      breaks <- as.numeric(matrix(unlist(color), ncol = 2, byrow = TRUE)[c(TRUE, FALSE),1])
      breaks <- 1 - breaks[1:((length(breaks)-1)/2)] * 2
      
      if (length(breaks) > 3) {
        # keep standard for continuous colorscale
        ticks <- sort(c(breaks*zmax, 0, breaks*zmin))
        
        ticktext <- paste0(number_format(ticks, digits = 2), " GHA")
        ticktext[1] <- sprintf("< %s", ticktext[1])
        ticktext[length(ticktext)] <- sprintf("> %s", ticktext[length(ticktext)])
      }
      
    } else {
      zmax <- quantile(unlist(select(cur_data, dataCol)), probs = rangeCut)[1]
      if (zmax > 10) {
        zmax <- ceiling(zmax)
      }
      zmin <- 0
      
      ticks <- c(0, as.numeric(matrix(unlist(color), ncol = 2, byrow = TRUE)[c(FALSE, TRUE),1])) * zmax
      
      ticktext <- paste0(number_format(round(ticks, digits = 2), digits = 2), " GHA")
      ticktext[length(ticktext)] <- sprintf("> %s", ticktext[length(ticktext)])
    }
    
    g <- list(
      scope = 'world',
      projection = list(type = 'Mercator'),
      showframe = FALSE,
      showcoastlines = TRUE,
      coastlinecolor = toRGB("grey")
    )
    
    l <- list(color = toRGB("grey"), width = 0.5)
    
    plot_geo(cur_data, height = 600) %>%
      add_trace(
        z = as.formula(paste0("~", dataCol)),
        locations = ~ ISO.alpha.3.code,
        color = as.formula(paste0("~", dataCol)),
        colorscale = color,
        marker = list(line = l),
        zmid = zmid,
        zmax = zmax,
        zmin = zmin
      ) %>%
      colorbar(
        tickmode = "array",
        tickvals = ticks,
        ticktext = ticktext,
        len = 1,
        title = "",
        ticksuffix = " GHA"
      ) %>%
      layout(title = '', geo = g)
  })
}