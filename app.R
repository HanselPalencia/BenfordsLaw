#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(coronavirus)
library(tidyverse)
library(geofacet)
library(nCov2019)
library(stats)
library(circular)

source("Global.R")


# Define UI for application that draws a histogram
ui <- navbarPage(title = "Benford's Law",
                 fluidPage(theme = shinytheme("cerulean"),
                           sidebarPanel(width = 3
                             
                           ),
                             
                             # Show a plot of the generated distribution
                             mainPanel(width = 8,
                                       
                                       # HTML(""),
                                       # HTML("<br>"),
                                       # HTML(""),
                                       
                                       tabsetPanel(type = "tabs",
                                                   
                                                   tabPanel("China Coronavirus Reporting",
                                                            fluidRow(
                                                              dataTableOutput("table")
                                                              # column(6,
                                                              #        highchartOutput(outputId = "icp_C", width = "100%",height = "700px")),
                                                              # column(6,
                                                              #        highchartOutput(outputId = "icp_d", width = "100%",height = "700px"))
                                                            ))
                                                   
                                       )
                                       )))
                           






server <- function(input, output, session) {
  
  
  
  seed <- eventReactive(input$push, {
    
    sample(1:20, 1)
    
  })
   
  
  output$table <- renderDataTable({
    
    my_func(seed())
    
    
  })
  
  
  data <- reactive({
    my_func(seed())
    
    
  })
  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

