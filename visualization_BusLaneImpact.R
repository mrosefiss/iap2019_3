library(tidyverse)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(devtools)

stops_rt1 <- data.frame("Route" = rep("Route1", 8), 
                    "Direction" = c(rep("Inbound",4),rep("Outbound",4)), 
                    "Num" = rep(1:4,2),
                    "Name" = c("Mass Ave at Pearl St","Mass Ave at Sidney St","84 Mass Ave","Mass Ave at Marlborough St","Mass Ave at Beacon St","77 Mass Ave","Mass Ave at Sidney St", "Mass Ave at Prospect St")  )
stops_rt71 <- data.frame("Route" = rep("Route71", 8), 
                        "Direction" = c(rep("Inbound",4),rep("Outbound",4)), 
                        "Num" = rep(1:4,2),
                        "Name" = c("Mt Auburn St at Ralph Piteri Terr","Mt Auburn St opp Brattle St","Mt Auburn St opp Trail St","Harvard","Harvard","Mt Auburn St at Trail St","Mt Auburn St at Brattle St", "Mt Auburn St at St. Marys St")  )

stops_rt73 <- data.frame("Route" = rep("Route73", 8), 
                         "Direction" = c(rep("Inbound",4),rep("Outbound",4)), 
                         "Num" = rep(1:4,2),
                         "Name" = c("Belmont St at St. Mary's St","Mt Auburn St opp Brattle St","Mt Auburn St opp Trail St","Harvard","Harvard","Mt Auburn St at Trail St","Mt Auburn St at Brattle St", "Belmont St at Cushing St")  )

stops <- rbind(stops_rt1, stops_rt71, stops_rt73)

d <- read.csv('/Users/maryrosefissinger/Documents/MIT/Classes/IAP_2019/Assignment3/full_data.csv', 
              na.strings=c("","NA"))

d$segment <- sub("^", "stops ", d$segment )
d$segment <- gsub("stops 1_4", "Full Segment", d$segment)

d$Implementation <- cut(d$year, c(-Inf,2017.5,Inf), c("Pre", "Post"))

names(d)[names(d)=="segment"]  <- "Segment"
names(d)[names(d)=="prepostm"]  <- "MonthofImplementation"

body <- dashboardBody(
  
  
  fluidRow(
    box(tableOutput("myTable"), title = "Summary Travel Time Statistics (in seconds)", width = 8),
    
    box(tableOutput("stopsTable"), title = "Stops Glossary", width = 4, background = "light-blue")

  ),
  
  fluidRow(
    
    box(plotOutput("bar"), title = "Travel Time on Full Segment",
        width = 12)
  ),
  
  fluidRow(
    
    box("Pre-implementation data is from weekdays between November 16 and December 21 of 2015, 2016, and 2017.", br(),
        "Post-implementation data is from weekdays between November 16 and December 21 of 2018.",br(),
        title = "Data Note",
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
      # filter(MonthofImplementation == 1) %>% 
      group_by(Segment, Implementation) %>% 
      summarise(Median = median(travel_time), '95%'=quantile(travel_time, probs=0.95),
                StdDev = sd(travel_time), n = n()) %>% 
      arrange(factor(Segment, levels = c("Full Segment","stops 1_2", "stops 2_3", "stops 3_4"))) 
    
  })
  
  output$stopsTable <- renderTable({
    stops %>% filter(Route == data()) %>% 
      filter(Direction == dir()) %>% 
      select(Num, Name)
  })
  
  output$bar <- renderPlot({
    # This returns a histogram of travel times on the full study segment, filtered by the inputs
    # and colored by whether it was pre- or post-implementation
    p <- ggplot(d %>% filter(Route == data()) %>% 
                  filter(Direction == dir()) %>% 
                  filter(time_period == tp()) %>% 
                  # filter(MonthofImplementation == 1) %>% 
                  filter(Segment == 'Full Segment'),
                aes(x = travel_time, color = factor(Implementation), fill = factor(Implementation))) + 
      geom_histogram(alpha = 0.5, position = "identity", bins = 30, aes(y =stat(width*density))) +
      theme_minimal() +
      theme(legend.position="top")
    p1 <- p + labs(x = "Travel Time", y = "Percent") +
      guides(color=FALSE) +
      scale_fill_discrete(name = "Implementation Phase",labels = c("Before", "After"))
    p1
  })
  
  
}

shinyApp(ui, server)

