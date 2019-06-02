############################
# Author: Jie Gao (Roger-G)
# https://github.com/Roger-G/national-footprint-visualization
############################

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

element1 <- select(
  totalFootprintPerCountry,
  country,
  UN_region,
  year,
  population,
  total,
  carbon,
  ISO.alpha.3.code
)


## Data preparation
element2 <- (
  element1 %>%
    group_by(ISO.alpha.3.code, country, year) 
)

element3 = aggregate(cbind(carbon) ~ UN_region + year, element1, FUN = sum)


carbonEmissionsUI <- function (id) {
  ns <- NS(id)
  
  tagList(sidebarLayout(
    sidebarPanel(
      selectInput(
        ns("region"),
        label = "Choose the two objects to compare",
        choices = list("Country and CO2 emission", "Continent and CO2 emission"),
        selected = "Country and CO2 emission"
      ),
      conditionalPanel(
        condition = "input.region == 'Country and CO2 emission'",
        ns = ns,
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
        condition = "input.region == 'Continent and CO2 emission'",
        ns = ns,
        selectInput(
          ns("continent1"),
          "Continent1",
          choices = dataRegions,
          selected = "Asia"
        ),
        selectInput(ns("continent2"), "Continent2", choices = dataRegions),
        #checkboxInput(ns("fit"), "Add line of best fit", FALSE),
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
      
      # numericInput(ns("size"), "Point size", 1, 1),
      sliderInput(
        ns("years"),
        "Years",
        dataYears[1],
        dataYears[2],
        value = c(1970, dataYears[2]),step=1,sep = "")
      
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Comparison",h3("How changes of total CO2 emission in selected countries?"), plotlyOutput(ns("plot_comparison"))),
        tabPanel("Distribution",h3("What the distribution of CO2 emission in different continents?"), plotlyOutput(ns("plot_box"))),
        tabPanel('Evolution',h3("What is the revolution of CO2 emission ?"),plotlyOutput(ns("plot_stream"))),
        tabPanel('World CO2 emission map',h3("How does changes of CO2 all the world over years ?"), plotlyOutput(ns("map")))
      )
    )
  ))
}


carbonEmissions <- function (input, output, session) {
  
  output$plot_comparison <- renderPlotly({
    if (input$region == "Country and CO2 emission") {
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
      data1<-as.data.frame(data1)
      data2<-as.data.frame(data2)
      ggplot(data1,aes(x=year,y=carbon/1000000))+
        geom_line(data=data1,aes(x=year,y=carbon/1000000),size = 1,col = input$colour) +
        geom_line(data=data2,aes(x=year,y=carbon/1000000),size = 1,col = input$colour1) +
        ylab('Total Carbon Emission / M') +
        ggtitle(sprintf("How changes of total CO2 emission between %s and %s ?",input$countries1,input$countries2))
    }
    else if (input$region == "Continent and CO2 emission") {
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
      ggplot(data1,aes(x=year,y=carbon/1000000))+
        geom_line(data=data1,aes(x=year,y=carbon/1000000),size = 1,col = input$colour1_1) +
        geom_line(data=data2,aes(x=year,y=carbon/1000000),size = 1,col = input$colour1_2) +
        ylab('Total Carbon Emission / M') +
        ggtitle(sprintf("Total CO2 emission in %s and %s",input$continent1,input$continent2))
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
    # add_trace(data2x=~year,y=~carbon,mode='point')
  })
  output$plot_stream <- renderPlotly({
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
  
  Data2<-reactive({
    temp_data<-subset(element1,year>=input$years[1] & year<=input$years[2])
    element_country<-(temp_data %>%
                        group_by(ISO.alpha.3.code)%>%
                        summarise(carbon=mean(carbon)))
    
  })
  
  output$map <- renderPlotly({
    data_map=Data2()
    
    data_map <- na.omit(data_map)
    
    # View(data_map)
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
        colors = "Blues",
        marker = list(line = l)
        
      ) %>%
      layout(title=sprintf("Mean Carbon Emissions From %g to %g",input$years[1],input$years[2]) ,
             geo = g)
  })
}