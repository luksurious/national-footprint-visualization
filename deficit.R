library(plotly)

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

    plot_ly(data=mydata,
      type = 'scatter',
      mode = 'lines'
    ) %>%
    add_trace(data = highBioData,
              y = ~ total.x,
              x = ~ year,
              name = 'Footprint',connectgaps = TRUE,
              line = list(color = 'red', width = 2),
              mode = 'lines') %>%
      add_trace(data = highBioData,
                y = ~ total.y,
                x = ~ year,
                name = 'Biocapacity',
                line = list(color = 'green', width = 2),
                fill = 'tonexty',connectgaps = TRUE,
                mode = 'lines') %>%
      add_trace(data = highEFData,
                y = ~ total.y,
                x = ~ year,
                name = 'Biocapacity2', connectgaps = TRUE,
                line = list(color = 'green', width = 2),
                mode = 'lines') %>%
      add_trace(data = highEFData,
                y = ~ total.x,
                x = ~ year,
                name = 'Footprint2',
                fill = 'tonexty',connectgaps = TRUE,
                line = list(color = 'red', width = 2),
                mode = 'lines') %>%
    layout(
      title = "Ecological Reserve/Deficit",
      xaxis = list(title = "Year"),
      yaxis = list (title = "in global hectares")
    )
  })
  
}