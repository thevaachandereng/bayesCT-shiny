#calling two different libraries
library(shiny)
library(bayesDP)
library(dplyr)
library(stats)
library(tidyverse)

function(input, output){
  
  v <- reactiveValues(data = NULL)
  
  inFile <- input$file1
  
  if (is.null(inFile))
    return(NULL)
  
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$runif, {
    v$data <- runif(100)
  })
  
  observeEvent(input$reset, {
    v$data <- NULL
  })  
  
  output$plot <- renderPlot({
    if (is.null(v$data)) return()
    hist(v$data)
  })
  

}
