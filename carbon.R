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

element2 <- (element1 %>%
               group_by(ISO.alpha.3.code, country, year))

element3 <-
  aggregate(cbind(carbon) ~ UN_region + year, element1, FUN =
              sum)



carbonEmissionsUI <- function (id) {
  ns <- NS(id)
  
  tagList(sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.carbonTabs == 'Comparison'",
        
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
          condition = "input.region == 'Continent and CO2 emission'",
          ns = ns,
          
          selectInput(
            ns("continent1"),
            "Continent1",
            choices = dataRegions,
            selected = "Asia"
          ),
          
          selectInput(
            ns("continent2"),
            "Continent2",
            choices = dataRegions,
            selected = "North America"
          ),
          
          
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
        )
      ),
      
      sliderInput(
        ns("years"),
        "Years",
        dataYears[1],
        dataYears[2],
        
        value = c(1970, dataYears[2]),
        step = 1,
        sep = "",
        animate = animationOptions(interval = 300)
      )
      
      
      
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        id = "carbonTabs",
        
        tabPanel(
          'World CO2 emission map',
          h3("How do CO2 emissions change in the world over the years?"),
          plotlyOutput(ns("map"))
        ),
        tabPanel(
          "Distribution",
          h3(
            "What is the distribution of CO2 emissions in different continents?"
          ),
          plotlyOutput(ns("plot_box"))
        ),
        tabPanel(
          'Evolution',
          h3("How do continents share of total CO2 emissions evolve?"),
          plotlyOutput(ns("plot_area"))
        ),
        tabPanel(
          "Comparison",
          h3("How do total CO2 emissions change in selected countries?"),
          plotlyOutput(ns("plot_comparision"))
        )
        
      )
    )
  ))
}


carbonEmissions <- function (input, output, session) {
  output$plot_comparision <- renderPlotly({
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
        ) +
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
        ggtitle(sprintf(
          "CO2 Emission between %s and %s",
          input$countries1,
          input$countries2
        ))
     
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
          "Total CO2 Emission in %s and %s",
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
  output$plot_area <- renderPlotly({
    box1 = Data1()
    
    (
      ggplot(box1 %>% filter(UN_region != 'World'), aes(year, carbon))
      
      + geom_area(aes(fill = UN_region),
                  alpha = 0.5)
      + scale_fill_brewer(palette = "Set1")
      + ylab('Total Carbon Emissions')
      + ggtitle(sprintf(
        "From %g to %g", input$years[1], input$years[2]
      ))
      
    ) %>%
      ggplotly()
  })
 
  Data2 <- reactive({
    element_temp <-
      (
        subset(
          aggregate(cbind(carbon) ~ ISO.alpha.3.code + year, element1, FUN =
                      sum),
          year >= input$years[1] &
            year <= input$years[2]
        ) %>% aggregate(cbind(carbon) ~ ISO.alpha.3.code, ., FUN = sum)
      )[-1, ]
  })
  
  output$map <- renderPlotly({
    data_map = Data2()
    
    colors <- sequentialColorscale(brewer.pal(9, "OrRd"))
    
    ticks <-
      c(0, as.numeric(matrix(
        unlist(colors), ncol = 2, byrow = TRUE
      )[c(FALSE, TRUE), 1])) * max(data_map$carbon)
    ticktext <-
      paste0(number_format(round(ticks, digits = 2), digits = 2), " GHA")
    
    
    # specify some map projection/options
    g <- list(
      scope = 'world',
      projection = list(type = 'Mercator'),
      showlakes = FALSE,
      
      showframe = FALSE,
      showcoastlines = TRUE,
      coastlinecolor = toRGB("grey")
    )
    
    l <- list(color = toRGB("grey"), width = 0.5)
    # create our plot
    plot_geo(data_map) %>%
      add_trace(
        z = ~ carbon,
        locations = ~ ISO.alpha.3.code,
        color = ~ carbon,
        colorscale = colors,
        marker = list(line = l)
        
      ) %>%
      
      colorbar(
        len = 1,
        tickmode = "array",
        tickvals = ticks,
        ticktext = ticktext
      ) %>%
      layout(
        title = sprintf(
          "Total Carbon Emissions from %g to %g",
          input$years[1],
          input$years[2]
        ) ,
        geo = g
      )
    
  })
}
