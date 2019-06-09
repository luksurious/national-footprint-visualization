library(plotly)
library(purrr)
library(RColorBrewer)

deficitTrendUI <- function (id) {
  ns <- NS(id)
  
  tagList(
    h2(
      "How is the evolution of the ecological reserve/deficit?"
    ),
    
    sidebarLayout(
      sidebarPanel(
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
            ns("region"),
            label = "Choose the region to show in the chart",
            choices = dataRegions,
            selected = "World"
          )
        ),
        conditionalPanel(
          condition = "input.regionType == 'Countries'",
          ns = ns,
          selectInput(
            ns("country"),
            label = "Choose the country to show in the chart",
            choices = dataCountries,
            selected = "Spain"
          )
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
            dataYears[2] - 20,
            dataYears[2]
          ),
          sep = "",
          step = 1
        ),
        
        radioButtons(
          ns("colors"),
          "Color scheme",
          choices = c("Semantic (green & red)" = "semantic", "Colorblind friendly (Dark2)" = "Dark2")
        )
      ),
      
      mainPanel(
        plotlyOutput(ns("plot"))
      )
    )
  )
}

deficitTrend <- function (input, output, session) {
  
  filteredDeficitData <- reactive({
    mydata <- deficitData(input$regionType, input$dataType)
    
    if (input$regionType == 'Countries') {
      mydata <- mydata[mydata$country == input$country
                       & mydata$year >= input$years[1]
                       & mydata$year <= input$years[2],]
    } else {
      mydata <- mydata[mydata$UN_region == input$region
                       & mydata$year >= input$years[1]
                       & mydata$year <= input$years[2],]
    }
  })
  
  colors <- reactive({
    if (input$colors == 'semantic') {
      list(
        "bio" = c("green", '#00800050'),
        "footprint" = c("red", "#FF000050")
      )
    } else if (input$colors == 'Dark2') {
      
      list(
        "bio" = c(categoricalDark2Colors8[1], paste0(categoricalDark2Colors8[1], "50")),
        "footprint" = c(categoricalDark2Colors8[3], paste0(categoricalDark2Colors8[3], "50"))
      )
    }
  })
  
  output$plot <- renderPlotly({
    mydata <- filteredDeficitData()
    
    highEFData <- mydata[mydata$total.x > mydata$total.y,]
    highBioData <- mydata[mydata$total.y > mydata$total.x,]
    
    highEFRanges <- getEdgesOfTrace(highEFData$year, input$years[1], input$years[2])
    highBioRanges <- getEdgesOfTrace(highBioData$year, input$years[1], input$years[2])
    
    p <- plot_ly(data = mydata,
      type = 'scatter',
      mode = 'lines+markers',
      marker = list(size = 4)
    ) %>%
      layout(
        title = "Ecological Reserve/Deficit",
        xaxis = list(title = "Year"),
        yaxis = list(title = "in global hectares"))
    
    bioConfig <- bioConfig()
    footprintConfig <- footprintConfig()
    
    # add traces where biocapacity is bigger than footprint
    for (range in highBioRanges) {
      rangeData <- highBioData[highBioData$year >= range$start & highBioData$year <= range$end,]

      p <- addHighResourceTrace(bioConfig, rangeData, p)
      
      footprintConfig$legend = FALSE
      bioConfig$legend = FALSE
      
      if (range$end < input$years[2]) {
        intersections = calcIntersectionLines(mydata, range)
        
        p <- addIntersectionTrace(bioConfig, intersections, p)
      }
    }
    
    # add traces where footprint is bigger than biocapacity
    for (range in highEFRanges) {
      rangeData <- highEFData[highEFData$year >= range$start & highEFData$year <= range$end,]
      
      p <- addHighResourceTrace(footprintConfig, rangeData, p)
      footprintConfig$legend = FALSE
      
      if (range$end < input$years[2]) {
        intersections = calcIntersectionLines(mydata, range)
        
        p <- addIntersectionTrace(footprintConfig, intersections, p)
      }
    }
    
    p
  })
  
  calcIntersectionLines <- function (data, range) {
    conData <- data[data$year >= range$end & data$year <= (range$end+1),]
    
    model1 <- lm(total.y ~ year, data = conData)
    model2 <- lm(total.x ~ year, data = conData)
    
    yearIntersect <- (model1$coefficients[1] - model2$coefficients[1]) / (model2$coefficients[2] - model1$coefficients[2])
    valueIntersect <- model1$coefficients[1] + model1$coefficients[2] * yearIntersect
    
    bioX <- c(range$end, yearIntersect)
    bioY <- c(conData[conData$year == range$end, c("total.y")], valueIntersect)
    bioX1 <- c(yearIntersect, range$end+1)
    bioY1 <- c(valueIntersect, conData[conData$year == (range$end+1), c("total.y")])
    
    efX <- c(range$end, yearIntersect)
    efY <- c(conData[conData$year == range$end, c("total.x")], valueIntersect)
    efX1 <- c(yearIntersect, range$end+1)
    efY1 <- c(valueIntersect, conData[conData$year == (range$end+1), c("total.x")])
    
    list("bioX" = bioX, "bioY" = bioY, "bioX1" = bioX1, "bioY1" = bioY1, "efX" = efX, "efY" = efY, "efX1" = efX1, "efY1" = efY1)
  }
  
  addIntersectionTrace <- function (config, intersections, p) {
    
    add_trace(p = p,
             data = data.frame(intersections[config$crossLow1X], intersections[config$crossLow1Y]),
             y = as.formula(paste0("~", config$crossLow1Y)),
             x = as.formula(paste0("~", config$crossLow1X)),
             name = config$lowerName, legendgroup = config$lowerGroup,
             hoverinfo = "skip",
             showlegend = FALSE,
             marker = NULL,
             line = list(color = config$lowerColor[1], width = 2),
             mode = 'lines') %>%
      add_trace(data = data.frame(intersections[config$crossHigh1X], intersections[config$crossHigh1Y]),
                y = as.formula(paste0("~", config$crossHigh1Y)),
                x = as.formula(paste0("~", config$crossHigh1X)),
                name = config$higherName, legendgroup = config$higherGroup,
                line = list(color = config$higherColor[1], width = 2),
                fillcolor = config$higherColor[2],
                showlegend = FALSE,
                hoverinfo = "skip",
                marker = NULL,
                fill = 'tonexty',
                mode = 'lines') %>%
      add_trace(data = data.frame(intersections[config$crossLow2X], intersections[config$crossLow2Y]),
                y = as.formula(paste0("~", config$crossLow2Y)),
                x = as.formula(paste0("~", config$crossLow2X)),
                name = config$higherName, legendgroup = config$higherGroup,
                hoverinfo = "skip",
                marker = NULL,
                showlegend = FALSE,
                line = list(color = config$higherColor[1], width = 2),
                mode = 'lines') %>%
      add_trace(data = data.frame(intersections[config$crossHigh2X], intersections[config$crossHigh2Y]),
                y = as.formula(paste0("~", config$crossHigh2Y)),
                x = as.formula(paste0("~", config$crossHigh2X)),
                name = config$lowerName, legendgroup = config$lowerGroup,
                hoverinfo = "skip",
                marker = NULL,
                showlegend = FALSE,
                line = list(color = config$lowerColor[1], width = 2),
                fillcolor = config$lowerColor[2],
                fill = 'tonexty',
                mode = 'lines')
  }
  
  bioConfig <- reactive({
    colors <- colors()
    
    list("lowerColors" = colors$footprint, "higherColors" = colors$bio,
         "lowerName" = "Footprint", "higherName" = "Biocapacity",
         "lowerGroup" = "ef", "higherGroup" = "bio",
         "lowerY" = "~total.x", "higherY" = "~total.y",
         "crossLow1X" = "efX", "crossLow1Y" = "efY",
         "crossHigh1X" = "bioX", "crossHigh1Y" = "bioY", 
         "crossLow2X" = "bioX1", "crossLow2Y" = "bioY1", 
         "crossHigh2X" = "efX1", "crossHigh2Y" = "efY1",
         "legend" = TRUE)
  })
  footprintConfig <- reactive({
    colors <- colors()
    
    list("lowerColors" = colors$bio, "higherColors" = colors$footprint,
         "lowerName" = "Biocapacity", "higherName" = "Footprint",
         "lowerGroup" = "bio", "higherGroup" = "ef",
         "lowerY" = "~total.y", "higherY" = "~total.x",
         "crossLow1X" = "bioX", "crossLow1Y" = "bioY",
         "crossHigh1X" = "efX", "crossHigh1Y" = "efY", 
         "crossLow2X" = "efX1", "crossLow2Y" = "efY1", 
         "crossHigh2X" = "bioX1", "crossHigh2Y" = "bioY1",
         "legend" = TRUE)
  })
  
  addHighResourceTrace <- function (config, data, p) {
    add_trace(p = p,
             data = data,
             y = as.formula(config$lowerY),
             showlegend = config$legend,
             x = ~ year,
             name = config$lowerName, legendgroup = config$lowerGroup,
             marker = list(color = config$lowerColor[1]),
             line = list(color = config$lowerColor[1], width = 2)) %>%
      add_trace(data = data,
                y = as.formula(config$higherY),
                x = ~ year,
                showlegend = config$legend,
                name = config$higherName, legendgroup = config$higherGroup,
                marker = list(color = config$higherColor[1]),
                line = list(color = config$higherColor[1], width = 2),
                fillcolor = config$higherColor[2],
                fill = 'tonexty')
  }
  
  getEdgesOfTrace <- function (years, minYear, maxYear) {
    i <- 1
    ranges <- list()
    if (!is_empty(years)) {
      rangeStart <- minYear
      startYear <- min(years)
      if (startYear > minYear) {
        rangeStart <- startYear
      }
      for (theYear in years) {
        if (theYear > (startYear+1)) {
          # gap between previous year
          ranges[[i]] <- list(start = rangeStart, end = startYear); i <- i + 1
          rangeStart <- theYear
        }
        
        startYear <- theYear
      }
      ranges[[i]] <- list(start = rangeStart, end = max(years))
    }
    
    return(ranges)
  }
}