library(plotly)

# Biocapacity trend
biocapacityTrendUI <- function (id, data) {
  ns <- NS(id)
  
  minYear <- min(data$year)
  maxYear <- max(data$year)
  
  tagList(
    h2(
      "Are there different trends in the evolution of the different biocapacity resources?"
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
            choices = levels(data$UN_region),
            selected = "World"
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("regionType"), "'] == 'Countries'"),
          
          selectInput(
            ns("country"),
            label = "Choose the country to show in the chart",
            choices = levels(data$country),
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
          minYear,
          maxYear,
          value = c(
            maxYear - 20,
            maxYear
          ),
          sep = "",
          step = 1
        ),
        
        checkboxInput(ns("show_total"), label = "Show totals")
      ),
      
      mainPanel(
        plotlyOutput(ns("plot")),
        h4("Changes in the resource types over the selected period", align = "center"),
        fluidRow(splitLayout(
          cellWidths = c("50%", "50%"),
          plotlyOutput(ns("absoluteChange")),
          plotlyOutput(ns("relativeChange"))
        ))
      )
    )
  )
}

biocapacityTrend <- function (input, output, session, selectBiocapData) {
  
  bioCapTrendData <- reactive({
    selectBiocapData(input$regionType, input$country, input$region, input$dataType, input$years)
  })  
  
  
  output$plot <- renderPlotly({
    p <-
      plot_ly(
        bioCapTrendData(),
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
        title = "Total Biocapacity Development",
        xaxis = list(title = "Year"),
        yaxis = list (title = "Biocapacity in global hectares")
      )
    
    if (input$show_total == TRUE) {
      p <- add_trace(
        y = ~ total,
        name = 'total',
        mode = 'lines',
        p = p
      )
    }
    
    p
  })
  
  #########
  # Biocapacity trend change
  #########
  bioCapChangeData <- reactive({
    cur_data <- bioCapTrendData()
    cur_data <- cur_data[cur_data$year == input$years[1] | cur_data$year == input$years[2], ]
    
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
    
    
    data.frame(type, absolute, relative)
  })
  
  output$absoluteChange <- renderPlotly({
    data <- bioCapChangeData()
    text <- sprintf("%.0f", data$absolute)
    if (input$dataType == 'Per person') {
      factor <- 1
      suffix <- ""
    } else {
      factor <- 1000000
      suffix <- "M"
    }
    
    text <- paste(formatC(data$absolute/factor, format = "f", big.mark = ",", digits = 2, flag = '+'), suffix)
    plot_ly(data, x = ~type, y = ~absolute, type = 'bar', text = text, textposition = 'auto'
    ) %>%
      layout(yaxis = list(title = "Absolute change of biocapacity in GHA"),
             xaxis = list(title = "Type of biocapacity"))
  })
  output$relativeChange <- renderPlotly({
    data <- bioCapChangeData()
    text <- paste(formatC(data$relative*100, format = "f", digits = 2, flag = '+'), "%")
    plot_ly(data, x = ~type, y = ~relative*100, type = 'bar', text = ~text,
            textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(yaxis = list(title = "Relative change of biocapacity"),
             xaxis = list(title = "Type of biocapacity"))
  })
  
}