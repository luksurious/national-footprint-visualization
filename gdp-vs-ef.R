
gdpVsEFUI <- function (id) {
  ns <- NS(id)
  
  tagList(
    h2("Is there a relation between GDP growth and footprint development?"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          ns("region"),
          label = "Choose the region to show in the chart",
          choices = dataRegions,
          selected = "World"
        ),
        
        radioButtons(ns("yearMode"), label = "Year or time range", choices = c("Time range", "Year (only end of Range)"), selected = "Time range"),
        
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
        )
      ),
      mainPanel(
        plotlyOutput(ns("gdpViz"))
      )
    )
  )
}

gdpVsEF <- function (input, output, session) {
  
  output$gdpViz <- renderPlotly({
    year <- input$years[2]
    
    data <- rawData[rawData$year == year & rawData$record == 'EFConsPerCap' & rawData$UN_region != 'World',
                    c("country", "UN_region", "total", "Percapita.GDP..2010.USD.", "population")]
    
    if (input$region != 'World') {
      data <- data[data$UN_region == input$region, ]
    }
    
    if (input$yearMode == 'Time range') {
      data2 <- rawData[rawData$year == input$years[1] & rawData$record == 'EFConsPerCap' & rawData$UN_region != 'World',
                       c("country", "UN_region", "total", "Percapita.GDP..2010.USD.", "population")]
      
      if (input$region != 'World') {
        data2 <- data2[data2$UN_region == input$region, ]
      }
      
      dataM <- merge(data2, data, by = 'country')
      
      data <- na.omit(data)
      
      dataM <- within(dataM, gdpDiff <- Percapita.GDP..2010.USD..y - Percapita.GDP..2010.USD..x)
      dataM <- within(dataM, efDiff <- total.y - total.x)
      dataM <- within(dataM, gdpPerc <- gdpDiff * 100 / Percapita.GDP..2010.USD..x)
      dataM <- within(dataM, efPerc <- efDiff * 100 / total.x)
      
      
      
      plot_ly(dataM, y = ~efPerc, x = ~gdpPerc, type = 'scatter', mode = 'markers',
              text = ~paste(country, '<br>Footprint change:', efPerc, '%<br>GDP change:', gdpPerc, '%<br>Population:', population.y), hoverinfo = 'text',
              color = ~UN_region.y, marker = list(opacity = 0.7, sizemode = 'diameter'), sizes = c(5, 25)) %>%
        layout(title = "GDP vs Footprint", 
               yaxis = list(title = "Ecological Footprint change per Person in %", scaleanchor = "x", scaleratio = 1),
               xaxis = list(title = "GDP change per Person in %")
        )
    } else {
      data <- na.omit(data)
      
      plot_ly(data, y = ~total, x = ~Percapita.GDP..2010.USD., type = 'scatter', mode = 'markers',
              text = ~paste(country, '<br>Footprint:', total, '<br>GDP:', Percapita.GDP..2010.USD., '<br>Population: ', population), hoverinfo = 'text',
              color = ~UN_region, marker = list(opacity = 0.7, sizemode = 'diameter'), sizes = c(5, 25)) %>%
        layout(title = "GDP vs Footprint", yaxis = list(title = "Ecological Footprint per Person in GHA"),
               xaxis = list(title = "GDP per Person in USD")
        )
    }
  })
}