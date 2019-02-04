library(tidyverse)
library(ggplot2)
library(shiny)
library(devtools)


d <- read.csv('/Users/maryrosefissinger/Documents/MIT/Classes/IAP_2019/Assignment3/full_data.csv', 
              na.strings=c("","NA"))


ui <- fluidPage(
  # Set Route, Direction, and Time of Day as Inputs 
  selectInput("inp_rte", "Choose a Route", 
              choices = d$Route %>% unique()),
  selectInput("inp_dir", "Choose a Direction", 
              choices = d$Direction %>% unique()),
  selectInput("inp_tp", "Choose a Time Period", 
              choices = d$time_period%>% unique()),
  tableOutput("myTable"),
  plotOutput("bar"),
  verbatimTextOutput("info")

)

server <- function(input, output) {
  data <- reactive({input$inp_rte})
  dir <- reactive({input$inp_dir})
  tp <- reactive({input$inp_tp})
  
  output$info <- renderPrint({
    # Attempt to explain the segment notation
    "Segment 1_4 is the full study segment, while Segment 1_2 is the portion of the route between the first and second stops of the study segment, for example"
  })
  
  output$myTable <- renderTable({
    # This returns a table filtered by the inputs and showing the median and standard deviation of
    # travel times by sub-segment and whether it was pre- or post-implementation 
    d %>% filter(Route == data()) %>% 
      filter(Direction == dir()) %>% 
      filter(time_period == tp()) %>% 
      group_by(segment, prepostm) %>% 
      summarise(Median = median(travel_time), StdDev = sd(travel_time)) %>% 
      arrange(factor(segment, levels = c("1_4","1_2", "2_3", "3_4"))) 
 
  })
  
  output$bar <- renderPlot({
    # This returns a histogram of travel times on the full study segment, filtered by the inputs
    # and colored by whether it was pre- or post-implementation
    p <- ggplot(d %>% filter(Route == data()) %>% 
                      filter(Direction == dir()) %>% 
                      filter(time_period == tp()) %>% 
                      filter(segment == '1_4'),
                aes(x = travel_time, color = factor(prepostm), fill = factor(prepostm))) + 
      geom_histogram(alpha = 0.5, position = "identity", bins = 30) + theme_minimal() +
      theme(legend.position="top")
    p1 <- p + labs(title = "Travel Times on Full Segment", x = "Travel Time", y = "Count") +
      guides(color=FALSE) +
      scale_fill_discrete(name = "Implementation Phase",labels = c("Before", "After"))
    p1
    })
  

}

shinyApp(ui, server)
  
