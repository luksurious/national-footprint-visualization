library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(janitor)
library(gridExtra)

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

## Data preparation
element2 <- (
  element1 %>%
    group_by(ISO.alpha.3.code, country, year, population) %>%
    summarise(total_mean = (mean(total)), carbon = mean(carbon))
)

mapData <- element1[element1$year == 2014 & element1$record == 'EFConsTotGHA', ]

element3 = aggregate(cbind(carbon, population) ~ UN_region + year, element1, FUN =
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
          choices = levels(element1$country),
          selected = "China"
        ),
        selectInput(
          ns("countries2"),
          "Country2",
          choices = levels((element1$country)),
          selected = "Africa"
        ),
        #checkboxInput(ns("fit"), "Add line of best fit", FALSE),
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
          choices = levels(element1$UN_region),
          selected = "Asia"
        ),
        selectInput(ns("continent2"), "Continent2", choices = levels((
          element1$UN_region
        ))),
        #checkboxInput(ns("fit"), "Add line of best fit", FALSE),
        fluidRow(column(
          5,
          radioButtons(
            ns("colour"),
            "Continent1",
            choices = c("blue", "red", "green", "yellow"),
            selected = 'yellow'
          )
        ),
        column(
          5,
          radioButtons(
            ns("colour1"),
            "Continent2",
            choices = c("blue", "red", "green", "pink"),
            selected = 'green'
          )
        ))
      ),
      
      # numericInput(ns("size"), "Point size", 1, 1),
      sliderInput(
        ns("years"),
        "Years",
        min(element1$year),
        max(element1$year),
        value = c(1970, 2001)
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot", plotlyOutput(ns("plot"))),
        tabPanel("Plot2", plotlyOutput(ns("test"))),
        tabPanel('Plot3', plotlyOutput(ns("plot2"))),
        tabPanel('World CO2 commsion map', plotlyOutput(ns("map")))
      )
    )
  ))
}


carbonEmissions <- function (input, output, session) {
  Data <- reactive({
    box1 <- subset(element3, year >= input$years[1] &
                     year <= input$years[2])
    
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
    }
    else if (input$region == "Continent and CO2 commision") {
      data1 <-
        subset(
          element3,
          UN_region %in% input$continent1 &
            year >= input$years[1] & year <= input$years[2]
        )
      #
      # print(data1)
      # print(data2)
    }
  })
  
  
  output$plot <- renderPlotly({
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
    }
    # data=curData()
    # plot_ly(curData(),x=~year,y=~carbon,name = 'nothing',type = 'box')
    data1 <- as.data.frame(data1)
    data2 <- as.data.frame(data2)
    p <- ggplot(data1, aes(x = year, y = carbon / 1000000)) +
      geom_line(
        data = data1,
        aes(x = year, y = carbon / 1000000),
        size = 1,
        col = input$colour
      ) +
      geom_line(
        data = data2,
        aes(x = year, y = carbon / 1000000),
        size = 1,
        col = input$colour1
      ) +
      ylab('Total Carbon Emission / M') +
      ggtitle(input$title)
    p
  })
  
  Data1 <- reactive({
    subset(element3, year >= input$years[1] & year <= input$years[2])
  })
  output$test <- renderPlotly({
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
    # add_trace(data2x=~year,y=~carbon,mode='point')
  })
  output$plot2 <- renderPlotly({
    # box1<-subset(element3[element3$year>=input$years[1]|element3$year<=input$years[2],])
    box1 = Data1()
    
    (
      ggplot(box1 %>% filter(UN_region != 'World'), aes(year, carbon))
        + geom_area(aes(fill = UN_region), alpha = 0.5)
        + ylab('Total Carbon Emissions')
        + ggtitle(sprintf("From %g to %g", input$years[1], input$years[2]))
    ) %>%
      ggplotly()
  })
  output$plot2 <- renderPlotly({
    # box1<-subset(element3[element3$year>=input$years[1]|element3$year<=input$years[2],])
    box1 = Data1()
    
    (
      ggplot(box1 %>% filter(UN_region != 'World'), aes(year, carbon))
        + geom_area(aes(fill = UN_region), alpha = 0.5)
        + ylab('Total Carbon Emissions')
        + ggtitle(sprintf("From %g to %g", input$years[1], input$years[2]))
    ) %>%
      ggplotly()
  })
  output$map <- renderPlotly({
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
    plot_geo(mapData) %>%
      add_trace(
        z = ~ carbon,
        locations = ~ ISO.alpha.3.code,
        color = ~ carbon,
        colors = "Blues",
        marker = list(line = l)
        
      ) %>%
      layout(title = 'Mean Carbon Emissions on the world',
             geo = g)
  })
}