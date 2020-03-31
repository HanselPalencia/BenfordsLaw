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

# Define UI for application that draws a histogram
ui <- navbarPage(title = "Advising Office - BYU-Idaho",
                 windowTitle = "Advising Office - BYU-Idaho",
                 fluidPage(theme = shinytheme("cerulean"),
                             
                             # Show a plot of the generated distribution
                             mainPanel(width = 8,
                                       
                                       # HTML(""),
                                       # HTML("<br>"),
                                       # HTML(""),
                                       
                                       tabsetPanel(type = "tabs",
                                                   
                                                   tabPanel("Benford's Law Data Download",
                                                            fluidRow(
                                                              # column(6,
                                                              #        highchartOutput(outputId = "icp_C", width = "100%",height = "700px")),
                                                              # column(6,
                                                              #        highchartOutput(outputId = "icp_d", width = "100%",height = "700px"))
                                                            ))
                                                   
                                       )
                                       )))
                           






server <- function(input, output) {
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

