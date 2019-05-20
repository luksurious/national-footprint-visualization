library(plotly)
library(tidyverse)

# Resource trend
deficitTrendUI <- function (id) {
  ns <- NS(id)
  
  tagList(
    h2(
      "How is the evolution of the ecological credit/deficit?"
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
          condition = paste0("input['", ns("regionType"), "'] == 'Continents'"),
          
          selectInput(
            ns("region"),
            label = "Choose the region to show in the chart",
            choices = dataRegions,
            selected = "World"
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("regionType"), "'] == 'Countries'"),
          
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
        
        checkboxInput(ns("show_total"), label = "Show totals")
      ),
      
      mainPanel(
        plotlyOutput(ns("plot"))
      )
    )
  )
}

deficitTrend <- function (input, output, session) {
  
  
  output$plot <- renderPlotly({
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
    
    highEFData <- mydata[mydata$total.x > mydata$total.y,]
    highBioData <- mydata[mydata$total.y > mydata$total.x,]
    
    # TODO: figure out gaps, and add connecting trace
    
    # TODO1: find end points of traces
    # TODO2: calculate 0 point
    # TODO3: create new traces for +- of 0 point
    highEFRanges <- getEdgesOfTrace(highEFData$year, input$years[1], input$years[2])
    highBioRanges <- getEdgesOfTrace(highBioData$year, input$years[1], input$years[2])
    
    
    p <- plot_ly(data=mydata,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      layout(
        title = "Ecological Reserve/Deficit",
        xaxis = list(title = "Year"),
        yaxis = list(title = "in global hectares"))

    hasHighBio <- FALSE
    
    for (range in highBioRanges) {
      rangeData <- highBioData[highBioData$year >= range$start & highBioData$year <= range$end,]

      hasHighBio <- TRUE
      
      p <- add_trace(p = p,
                data = rangeData,
                y = ~ total.x,
                x = ~ year,
                name = 'Footprint', legendgroup = "ef",
                line = list(color = 'red', width = 2),
                mode = 'lines') %>%
        add_trace(data = rangeData,
                  y = ~ total.y,
                  x = ~ year,
                  name = 'Biocapacity', legendgroup = "bio",
                  line = list(color = 'green', width = 2),
                  fillcolor = 'rgba(216, 234, 186, 0.5)',
                  fill = 'tonexty',
                  mode = 'lines')
      
      if (range$end < input$years[2]) {
        conData <- mydata[mydata$year >= range$end & mydata$year <= (range$end+1),]
        
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
        
        p <- add_trace(p = p,
                       data = data.frame(efX, efY),
                       y = ~ efY,
                       x = ~ efX,
                       name = 'Footprint', legendgroup = "ef",
                       hoverinfo = "skip",
                       showlegend = FALSE,
                       line = list(color = 'red', width = 2),
                       mode = 'lines') %>%
          add_trace(data = data.frame(bioX, bioY),
                    y = ~ bioY,
                    x = ~ bioX,
                    name = 'Biocapacity', legendgroup = "bio",
                    line = list(color = 'green', width = 2),
                    fillcolor = 'rgba(216, 234, 186, 0.5)',
                    showlegend = FALSE,
                    hoverinfo = "skip",
                    fill = 'tonexty',
                    mode = 'lines') %>%
          add_trace(data = data.frame(bioX1, bioY1),
                   y = ~ bioY1,
                   x = ~ bioX1,
                   name = 'Biocapacity', legendgroup = "bio",
                   hoverinfo = "skip",
                   showlegend = FALSE,
                   line = list(color = 'green', width = 2),
                   mode = 'lines') %>%
          add_trace(data = data.frame(efX1, efY1),
                    y = ~ efY1,
                    x = ~ efX1,
                    name = 'Footprint', legendgroup = "ef",
                    hoverinfo = "skip",
                    showlegend = FALSE,
                    line = list(color = 'red', width = 2),
                    fillcolor = 'rgba(234, 216, 186, 0.5)',
                    fill = 'tonexty',
                    mode = 'lines')
      }
    }
    for (range in highEFRanges) {
      rangeData <- highEFData[highEFData$year >= range$start & highEFData$year <= range$end,]
      p <- add_trace(p = p,
                     data = rangeData,
                     y = ~ total.y,
                     showlegend = !hasHighBio,
                     x = ~ year,
                     name = 'Biocapacity', legendgroup = "bio",
                     line = list(color = 'green', width = 2),
                     mode = 'lines') %>%
        add_trace(data = rangeData,
                     y = ~ total.x,
                     x = ~ year,
                      showlegend = !hasHighBio,
                     name = 'Footprint', legendgroup = "ef",
                     line = list(color = 'red', width = 2),
                     fillcolor = 'rgba(234, 216, 186, 0.5)',
                     fill = 'tonexty',
                     mode = 'lines')
      
      if (range$end < input$years[2]) {
        conData <- mydata[mydata$year >= range$end & mydata$year <= (range$end+1),]
        
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
        
        p <- 
          add_trace(p = p,
                    data = data.frame(bioX1, bioY1),
                    y = ~ bioY1,
                    x = ~ bioX1,
                    name = 'Biocapacity', legendgroup = "bio",
                    hoverinfo = "skip",
                    showlegend = FALSE,
                    line = list(color = 'green', width = 2),
                    mode = 'lines') %>%
          add_trace(data = data.frame(efX1, efY1),
                    y = ~ efY1,
                    x = ~ efX1,
                    name = 'Footprint', legendgroup = "ef",
                    hoverinfo = "skip",
                    showlegend = FALSE,
                    line = list(color = 'red', width = 2),
                    fillcolor = 'rgba(216, 234, 186, 0.5)',
                    fill = 'tonexty',
                    mode = 'lines') %>%
          add_trace(data = data.frame(efX, efY),
                       y = ~ efY,
                       x = ~ efX,
                       name = 'Footprint', legendgroup = "ef",
                       hoverinfo = "skip",
                       showlegend = FALSE,
                       line = list(color = 'red', width = 2),
                       mode = 'lines') %>%
          add_trace(data = data.frame(bioX, bioY),
                    y = ~ bioY,
                    x = ~ bioX,
                    name = 'Biocapacity', legendgroup = "bio",
                    line = list(color = 'green', width = 2),
                    fillcolor = 'rgba(234, 216, 186, 0.5)',
                    showlegend = FALSE,
                    hoverinfo = "skip",
                    fill = 'tonexty',
                    mode = 'lines')
      }
    }
    
    p
  })
  
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