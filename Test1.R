library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
elements<-read.csv(file.path("/Users/gaojie/R/Assignment","NFA 2018.csv"))
elements <- elements[elements$record == "EFConsTotGHA", ]
element_origin=select(elements,country,UN_region,year,population,total,carbon,Percapita.GDP..2010.USD.)

## Data preparation
element_country<-(element_origin %>%
             group_by(country,year,UN_region)%>%
             summarise(total_mean=(mean(total)),carbon=mean(carbon)))

element_unregion <- aggregate(cbind(carbon) ~ UN_region+year, element_origin, FUN=sum)
# element_country_per<- (element_origin %>%
#                          group_by(country,year,UN_region)%>%
#                          summarise(total_mean=(mean(total)),carbon=mean(carbon/population)))



ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "region",
        label = "Choose the two objects to compare",
        choices = list("Country and CO2 commision","Continent and CO2 commision"),
        selected = "Country and CO2 commision"
      ),
      conditionalPanel(
        condition = "input.region=='Country and CO2 commision'",
        selectInput("countries1","Country1",choices = levels(elements$country),selected = "China"),
        selectInput("countries2", "Country2",choices = levels((elements$country)),selected = "United States of America"),
        
        fluidRow(
          column(5,radioButtons("colour", "Country1",
                                choices = c("blue", "red", "green", "yellow"))),
          column(5,radioButtons("colour1", "Country2",
                                choices = c("blue", "red", "green", "pink"),selected = 'red'))
        )
      ),
      conditionalPanel(
        condition = "input.region=='Continent and CO2 commision'",
        selectInput("continent1","Continent1",choices = levels(elements$UN_region),selected = "Asia"),
        selectInput("continent2", "Continent2",choices = levels(elements$UN_region),selected="Europe"),

        fluidRow(
          column(5,radioButtons("colour1_1", "Continent1",
                                choices = c("blue", "red", "green", "yellow"),selected = 'blue')),
          column(5,radioButtons("colour1_2", "Continent2",
                                choices = c("blue", "red", "green", "pink"),selected = 'red'))
        )
      ),
      
      # numericInput("size", "Point size", 1, 1),
      sliderInput("years","Years",min(elements$year),max(elements$year),value = c(1961,2007),step=1,sep = "")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Comparision",h3("How changes of total CO2 emission in selected countries?"),plotlyOutput("plot_comparision")),
        tabPanel("Distribution",h3("What the distribution of CO2 emission in different continents?"),plotlyOutput("plot_box")),
        tabPanel('Evolution',h3("What is the revolution of CO2 emission ?"),plotlyOutput("plot_stream")),
        tabPanel('World CO2 emission map',h3("How does changes of CO2 all the world over years ?"),plotlyOutput("map"))
                 )
      )
    )
  )


server <- function(input, output, session) {
  
  Data <- reactive({
    box1<-subset(element_unregion,year>=input$years[1] & year<=input$years[2])
   
  })
  ##test
  output$plot_comparision<-renderPlotly({
    if (input$region=="Country and CO2 commision"){
      data1<-subset(element_country,country %in% input$countries1 & year>=input$years[1] & year<=input$years[2])
      data2<-subset(element_country,country %in% input$countries2 & year>=input$years[1] & year<=input$years[2])
      data1<-as.data.frame(data1)
      data2<-as.data.frame(data2)
      ggplot(data1,aes(x=year,y=carbon/1000000))+
        geom_line(data=data1,aes(x=year,y=carbon/1000000),size = 1,col = input$colour) +
        geom_line(data=data2,aes(x=year,y=carbon/1000000),size = 1,col = input$colour1) +
        ylab('Total Carbon Emission / M') +
        ggtitle(sprintf("How changes of total CO2 commision between %s and %s ?",input$countries1,input$countries2))
    }
    else if (input$region=="Continent and CO2 commision"){
      data1<-subset(element_unregion,UN_region %in% input$continent1 & year>=input$years[1] & year<=input$years[2])
      data2<-subset(element_unregion, UN_region%in% input$continent2 & year>=input$years[1] & year<=input$years[2])
      ggplot(data1,aes(x=year,y=carbon/1000000))+
        geom_line(data=data1,aes(x=year,y=carbon/1000000),size = 1,col = input$colour1_1) +
        geom_line(data=data2,aes(x=year,y=carbon/1000000),size = 1,col = input$colour1_2) +
        ylab('Total Carbon Emission / M') +
        ggtitle(sprintf("Total CO2 commision in %s and %s",input$continent1,input$continent2))
    }

  })
  
  Data1<-reactive({
    subset(element_unregion,year>=input$years[1] & year<=input$years[2])
  })
  
  output$plot_box<-renderPlotly({
    
    box1<-Data1()
    p<-plot_ly(box1,x=~UN_region,y=~carbon,name = 'nothing',type = 'box')%>%
      layout(title=sprintf("From %g to %g",input$years[1],input$years[2]))
    p
    
  })
  
  output$plot_stream<-renderPlotly({
    
    box1=Data1()
    
    (ggplot(box1 %>% filter(UN_region != 'World'), aes(year, carbon)) 
      + geom_area(aes(fill=UN_region), alpha=0.5) + ylab('Total Carbon Emission') + ggtitle(sprintf("From %g to %g",input$years[1],input$years[2]))) %>%
      ggplotly()
  })
  
  Data2<-reactive({
    temp_data<-subset(element_origin,year>=input$years[1] & year<=input$years[2])
    element_country<-(temp_data %>%
                        group_by(country)%>%
                        summarise(carbon=mean(carbon)))
    
  })
  Data3<-reactive({
    subset(element_country_per,year>=input$year[1] & year<=input$years[2])
  })
  
  output$map<-renderPlotly({
    data_map=Data2()
    
    View(data_map)
    # specify some map projection/options
    g <- list(
      scope = 'world',
      projection = list(type = 'natural earth'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    # create our plot
    plot_geo(data_map, locationmode = 'country names') %>%
      add_trace(
        z = ~carbon, 
        locations = ~country,
        color = ~carbon,
        colors = "Blues"   
      ) %>%
      layout(
        title=sprintf("'Mean Carbon Emissions From %g to %g",input$years[1],input$years[2]) ,
        geo = g
      )
  }
  )
}
shinyApp(ui, server)
