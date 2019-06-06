############################
# Author: Jie Gao (Roger-G)
# https://github.com/Roger-G/national-footprint-visualization
############################

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

element1 = select(
  rawData,
  country,
  UN_region,
  year,
  population,
  total,
  carbon,
  ISO.alpha.3.code,
  record,
  Percapita.GDP..2010.USD.
)

element1 <- element1[element1$record == "EFConsTotGHA",]

element2 <- (element1 %>%
               group_by(ISO.alpha.3.code, country, year))

element3 <- aggregate(cbind(carbon) ~ UN_region + year, element1, FUN =
                        sum)


carbonEmissionsUI <- function (id) {
  ns <- NS(id)
  
  tagList(sidebarLayout(
    sidebarPanel(
      selectInput(
        ns("region"),
        label = "Choose the two objects to compare",
        choices = list("Country and CO2 commision", "Continent and CO2 commision"),
        selected = "Country and CO2 commision"
      ),
      conditionalPanel(
        condition = paste0("input['", ns("region"), "'] == 'Country and CO2 commision'"),
        selectInput(
          ns("countries1"),
          "Country1",
          choices = dataCountries,
          selected = "China"
        ),
        selectInput(
          ns("countries2"),
          "Country2",
          choices = dataCountries,
          selected = "United States of America"
        ),
        
        fluidRow(column(
          5,
          radioButtons(
            ns("colour"),
            "Country1",
            choices = c("blue", "red", "green", "yellow"),
            selected = 'blue'
          )
        ),
        column(
          5, radioButtons(
            ns("colour1"),
            "Country2",
            choices = c("blue", "red", "green", "pink"),
            selected = 'red'
          )
        ))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("region"), "'] == 'Continent and CO2 commision'"),
        selectInput(
          ns("continent1"),
          "Continent1",
          choices = dataRegions,
          selected = "Asia"
        ),
        selectInput(ns("continent2"), "Continent2", choices = dataRegions,selected = "North America"),
        
        fluidRow(column(
          5,
          radioButtons(
            ns("colour1_1"),
            "Continent1",
            choices = c("blue", "red", "green", "yellow"),
            selected = 'blue'
          )
        ),
        column(
          5,
          radioButtons(
            ns("colour1_2"),
            "Continent2",
            choices = c("blue", "red", "green", "pink"),
            selected = 'red'
          )
        ))
      ),
      
      radioButtons(ns("yearMode"), label = "Year or time range", choices = c("Time range", "Year"), selected = "Time range"),
      conditionalPanel(
        condition="input.yearMode=='Time range'",
        ns=ns,
        sliderInput(
        ns("years"),
        "Years",
        dataYears[1],
        dataYears[2],
        value = c(1970, dataYears[2]),
        step = 1,
        sep = "",
        animate = animationOptions(interval = 300)
      )),
      conditionalPanel(
        condition="input.yearMode=='Year'",
        ns=ns,
        sliderInput(
        ns("years"),
        "Years",
        dataYears[1],
        dataYears[2],
        value = dataYears[2],
        step = 1,
        sep = "",
        animate = animationOptions(interval = 300)
      ))
      
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          'World CO2 emission map',
          h3("How does change of CO2 emission in the world over years  ?"),
          plotlyOutput(ns("map"))
        ),
        tabPanel(
          "Distribution",
          h3("What is the trend of CO2 emission in different continents?"),
          plotlyOutput(ns("plot_box"))
        ),
        tabPanel(
          'Evolution',
          h3("What is the revolution of CO2 emission ?"),
          plotlyOutput(ns("plot_stream"))
        ),
        tabPanel(
          "Comparision",
          h3("How changes of total CO2 emission in two selected countries?"),
          plotlyOutput(ns("plot_comparision"))
        )
      )
    )
  ))
}


carbonEmissions <- function (input, output, session) {
  output$plot_comparision <- renderPlotly({
    if (input$region == "Country and CO2 commision") {
      data1 <-
        subset(
          element2,
          country %in% input$countries1 &
            year >= input$years[1] & year <= input$years[2]
        )
      data2 <-
        subset(
          element2,
          country %in% input$countries2 &
            year >= input$years[1] & year <= input$years[2]
        )
      data1 <- as.data.frame(data1)
      data2 <- as.data.frame(data2)
      ggplot(data1, aes(x = year, y = carbon / 1000000)) +
        geom_line(
          data = data1,
          aes(x = year, y = carbon / 1000000),
          size = 0.6,
          col = input$colour
        ) +
        geom_point(
          data = data1,
          aes(x = year, y = carbon / 1000000),
          size = 1,
          col = input$colour
        )+
        geom_line(
          data = data2,
          aes(x = year, y = carbon / 1000000),
          size = 0.6,
          col = input$colour1
        ) +
        geom_point(
          data = data2,
          aes(x = year, y = carbon / 1000000),
          size = 1,
          col = input$colour1
        ) +
        ylab('Total Carbon Emission / M') +
        ggtitle(
          sprintf(
            "How changes of total CO2 commision between %s and %s ?",
            input$countries1,
            input$countries2
          )
        )
    }
    else if (input$region == "Continent and CO2 commision") {
      data1 <-
        subset(
          element3,
          UN_region %in% input$continent1 &
            year >= input$years[1] & year <= input$years[2]
        )
      data2 <-
        subset(
          element3,
          UN_region %in% input$continent2 &
            year >= input$years[1] & year <= input$years[2]
        )
      ggplot(data1, aes(x = year, y = carbon / 1000000)) +
        geom_line(
          data = data1,
          aes(x = year, y = carbon / 1000000),
          size = 0.6,
          col = input$colour1_1
        ) +
        geom_point(
          data = data1,
          aes(x = year, y = carbon / 1000000),
          size = 1,
          col = input$colour1_1
        ) +
        geom_line(
          data = data2,
          aes(x = year, y = carbon / 1000000),
          size = 0.6,
          col = input$colour1_2
        ) +
        geom_point(
          data = data2,
          aes(x = year, y = carbon / 1000000),
          size = 1,
          col = input$colour1_2
        ) +
        ylab('Total Carbon Emission / M') +
        ggtitle(sprintf(
          "Total CO2 commision in %s and %s",
          input$continent1,
          input$continent2
        ))
    }
    
  })
  
  Data1 <- reactive({
    subset(element3, year >= input$years[1] & year <= input$years[2])
  })
  output$plot_box <- renderPlotly({
    box1 <- Data1()
    p <-
      plot_ly(
        box1,
        x =  ~ UN_region,
        y =  ~ carbon,
        name = 'nothing',
        type = 'box'
      ) %>%
      layout(title = sprintf("From %g to %g", input$years[1], input$years[2]))
    p
    
  })
  output$plot_stream <- renderPlotly({
    
    box1 = Data1()
    
    (
      ggplot(box1 %>% filter(UN_region != 'World'), aes(year, carbon))
      + geom_area(
        aes(fill = UN_region),
        alpha = 0.5
      )
      + scale_fill_brewer(palette = "Set1")
      + ylab('Total Carbon Emissions')
      + ggtitle(sprintf(
        "From %g to %g", input$years[1], input$years[2]
      ))
    ) %>%
      ggplotly()
  })
  output$plot2 <- renderPlotly({
    
    box1 = Data1()
    
    (
      ggplot(box1 %>% filter(UN_region != 'World'), aes(year, carbon))
      + geom_area(aes(fill = UN_region), alpha = 0.5)
      + ylab('Total Carbon Emissions')
      + ggtitle(sprintf(
        "From %g to %g", input$years[1], input$years[2]
      ))
    ) %>%
      ggplotly()
  })
  

  Data2<-reactive({
    
    element_temp<-(subset(aggregate(cbind(carbon) ~ ISO.alpha.3.code + year, element1, FUN =
                                      sum),year>=input$years[1] & year<=input$years[2]) %>% aggregate(cbind(carbon) ~ ISO.alpha.3.code,.,FUN = sum))[-1,]
  })
  
  output$map <- renderPlotly({
    data_map = Data2()
    colorsdiverging <- list(

      list(0, "#f7fcfd"),
      list(0.125, "#f7fcfd"),
      list(0.1250000001, "#e0ecf4"),
      list(0.25, "#e0ecf4"),
      list(0.250000001, "#bfd3e6"),
      list(0.375, "#bfd3e6"),
      list(0.3750000001, "#9ebcda"),
      list(0.5, "#9ebcda"),
      list(0.50000001, "#8c96c6"),
      list(0.6250000000, "#8c96c6"),
      list(0.6250000001, "#8c6bb1"),
      list(0.7500000000, "#8c6bb1"),
      list(0.7500000001, "#88419d"),
      list(0.8250000000, "#88419d"),
      list(0.8250000001, "#6e016b"),
      list(1, "#6e016b")
      
    )

    # specify some map projection/options
    g <- list(
      scope = 'world',
      projection = list(type = 'Mercator'),
      showlakes = FALSE,
      
      showframe = FALSE,
      showcoastlines = FALSE
    )

    l <- list(color = toRGB("grey"), width = 0.5)
    # create our plot
    plot_geo(data_map) %>%
      add_trace(
        z = ~ carbon,
        locations = ~ ISO.alpha.3.code,
        color = ~ carbon,
        colorscale = colorsdiverging,
        marker = list(line = l)
        
      ) %>%
      colorbar(len = 1) %>%
      layout(
        title = sprintf(
          "Mean Carbon Emissions From %g to %g",
          input$years[1],
          input$years[2]
        ) ,
        geo = g
      )
  })
}
