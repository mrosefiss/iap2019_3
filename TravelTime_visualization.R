library(tidyverse)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(devtools)


d <- read.csv('/Users/maryrosefissinger/Documents/MIT/Classes/IAP_2019/Assignment3/full_data.csv', 
              na.strings=c("","NA"))

d$segment <- sub("^", "stops ", d$segment )
d$segment <- gsub("stops 1_4", "Full Segment", d$segment)

names(d)[names(d)=="segment"]  <- "Segment"
names(d)[names(d)=="prepostm"]  <- "AfterImplementation"

body <- dashboardBody(

  
  fluidRow(
    box(tableOutput("myTable"), title = "Summary Statistics", width = 8)
  ),
  
  fluidRow(
    
    box(plotOutput("bar"), title = "Travel Time on Full Segment",
        width = 12)
  )
)

header <- dashboardHeader(
  
  
)

sidebar <- dashboardSidebar(  selectInput("inp_rte", "Choose a Route", 
                                          choices = d$Route %>% unique()),
                              selectInput("inp_dir", "Choose a Direction", 
                                          choices = d$Direction %>% unique()),
                              selectInput("inp_tp", "Choose a Time Period", 
                                          choices = d$time_period%>% unique())
)

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
  data <- reactive({input$inp_rte})
  dir <- reactive({input$inp_dir})
  tp <- reactive({input$inp_tp})

  
  output$myTable <- renderTable({
    # This returns a table filtered by the inputs and showing the median and standard deviation of
    # travel times by sub-segment and whether it was pre- or post-implementation 
    d %>% filter(Route == data()) %>% 
      filter(Direction == dir()) %>% 
      filter(time_period == tp()) %>% 
      group_by(Segment, AfterImplementation) %>% 
      summarise(Median = median(travel_time), StdDev = sd(travel_time)) %>% 
      arrange(factor(Segment, levels = c("Full Segment","stops 1_2", "stops 2_3", "stops 3_4"))) 
 
  })
  
  output$bar <- renderPlot({
    # This returns a histogram of travel times on the full study segment, filtered by the inputs
    # and colored by whether it was pre- or post-implementation
    p <- ggplot(d %>% filter(Route == data()) %>% 
                      filter(Direction == dir()) %>% 
                      filter(time_period == tp()) %>% 
                      filter(Segment == 'Full Segment'),
                aes(x = travel_time, color = factor(AfterImplementation), fill = factor(AfterImplementation))) + 
      geom_histogram(alpha = 0.5, position = "identity", bins = 30) + theme_minimal() +
      theme(legend.position="top")
    p1 <- p + labs(title = "Travel Times on Full Segment", x = "Travel Time", y = "Count") +
      guides(color=FALSE) +
      scale_fill_discrete(name = "Implementation Phase",labels = c("Before", "After"))
    p1
    })
  

}

shinyApp(ui, server)
  
