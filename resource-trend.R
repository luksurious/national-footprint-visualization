library(plotly)

# Resource trend
resourceTrendUI <- function (id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          ns("recordType"),
          label = "Data",
          choices = c("Biocapacity", "Footprint"),
          selected = "Biocapacity"
        ),
        
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
        )
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel(
            'Over time',
            
            h2("Are there different trends in the evolution of the different resources?"),
            plotlyOutput(ns("plot"))
          ),
          tabPanel(
            'Change',
            
            h2("How much have the different resources changed over the years?", align = "center"),
            fluidRow(splitLayout(
              cellWidths = c("50%", "50%"),
              plotlyOutput(ns("absoluteChange")),
              plotlyOutput(ns("relativeChange"))
            ))
          ),
          type = 'pills'
        )
      )
    )
  )
}

resourceTrend <- function (input, output, session) {
  
  resourceTrendData <- reactive({
    if (input$recordType == 'Footprint') {
      selectFootprintData(input$regionType, input$country, input$region, input$dataType, input$years)
    } else {
      selectBiocapData(input$regionType, input$country, input$region, input$dataType, input$years)
    }
  })  
  
  output$plot <- renderPlotly({
    p <-
      plot_ly(
        resourceTrendData(),
        x = ~ year,
        name = 'crop land',
        y = ~ crop_land,
        type = 'scatter',
        mode = 'lines'
      ) %>%
      add_trace(y = ~ grazing_land,
                name = 'grazing land',
                mode = 'lines') %>%
      add_trace(y = ~ forest_land,
                name = 'forest land',
                mode = 'lines') %>%
      add_trace(y = ~ fishing_ground,
                name = 'fishing ground',
                mode = 'lines') %>%
      add_trace(y = ~ built_up_land,
                name = 'built up land',
                mode = 'lines') %>%
      layout(
        title = paste0("Total ", input$recordType, " Development"),
        xaxis = list(title = "Year"),
        yaxis = list (title = paste0(input$recordType, " in global hectares"))
      )
    
    if (input$recordType == 'Footprint') {
      p <- add_trace(
        y = ~ carbon,
        name = 'carbon emissions',
        mode = 'lines',
        p = p
      )
    }
    p <- add_trace(
      p = p,
      y = ~ total,
      name = 'total',
      mode = 'lines',
      visible = "legendonly"
    )
    
    p
  })
  
  #########
  # Biocapacity trend change
  #########
  resourceChangeData <- reactive({
    cur_data <- resourceTrendData()
    cur_data <- cur_data[cur_data$year == min(cur_data$year) | cur_data$year == max(cur_data$year), ]
    
    crop_change <- cur_data$crop_land[2] - cur_data$crop_land[1]
    forest_change <- cur_data$forest_land[2] - cur_data$forest_land[1]
    grazing_change <- cur_data$grazing_land[2] - cur_data$grazing_land[1]
    fishing_change <- cur_data$fishing_ground[2] - cur_data$fishing_ground[1]
    built_change <- cur_data$built_up_land[2] - cur_data$built_up_land[1]
    
    
    crop_relative <- crop_change / cur_data$crop_land[1]
    forest_relative <- forest_change / cur_data$forest_land[1]
    grazing_relative <- grazing_change / cur_data$grazing_land[1]
    fishing_relative <- fishing_change / cur_data$fishing_ground[1]
    built_relative <- built_change / cur_data$built_up_land[1]
    
    type <- c("Crop", "Forest", "Grazing", "Fishing", "Built up")
    absolute <- c(crop_change, forest_change, grazing_change, fishing_change, built_change)
    relative <- c(crop_relative, forest_relative, grazing_relative, fishing_relative, built_relative)
    
    
    if (input$recordType == 'Footprint') {
      carbon_change <- cur_data$carbon[2] - cur_data$carbon[1]
      carbon_relative <- carbon_change / cur_data$carbon[1]
      
      type <- append(type, c("Carbon"))
      absolute <- append(absolute, c(carbon_change))
      relative <- append(relative, c(carbon_relative))
    }
    
    data.frame(type, absolute, relative)
  })
  
  output$absoluteChange <- renderPlotly({
    data <- resourceChangeData()
    text <- sprintf("%.0f", data$absolute)
    if (input$dataType == 'Per person') {
      factor <- 1
      suffix <- ""
    } else {
      factor <- 1000000
      suffix <- "M"
    }
    
    text <- paste(formatC(data$absolute/factor, format = "f", big.mark = ",", digits = 2, flag = '+'), suffix)
    plot_ly(data, x = ~type, y = ~absolute, type = 'bar', text = text, textposition = 'auto') %>%
      layout(yaxis = list(title = paste0("Absolute change of ", input$recordType, " in GHA")),
             xaxis = list(title = paste0("Type of ", input$recordType)))
  })
  
  output$relativeChange <- renderPlotly({
    data <- resourceChangeData()
    text <- paste(formatC(data$relative*100, format = "f", digits = 2, flag = '+'), "%")
    plot_ly(data, x = ~type, y = ~relative*100, type = 'bar', text = ~text,
            textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(yaxis = list(title = paste0("Relative change of ", input$recordType)),
             xaxis = list(title = paste0("Type of ", input$recordType)))
  })
  
}