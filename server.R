#calling two different libraries
library(shiny)
library(bayesDP)
library(dplyr)
library(stats)
library(tidyverse)

function(input, output){
  
  sliderValues <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    
    # for normal data
    if(input$dist == "Normal"){
      normal_analysis(data                  = NULL,
                      alternative           = "greater",
                      N_impute              = 100,
                      h0                    = 0,
                      number_mcmc           = 10000,
                      prob_ha               = 0.95,
                      futility_prob         = 0.10,
                      expected_success_prob = 0.90)

  }
    else if(input$dist == "Binomial"){
      if(input$margin_b == "add"){
        pT <- input$pC - input$marginval_b1
        val <- add.margin(delta = input$marginval_b1, varC = input$pC * (1 - input$pC), varT = pT * (1 - pT),
                          alpha = as.numeric(input$type1_b), beta = 1 - as.numeric(input$power_b))
        data.frame(Output = c("Sample Size in Control",
                              "Sample Size in Treatment",
                              "Randomization control to treatment (k:1))",
                              "Realtive Efficiency (compared to 1:1)"),
                   Values =  c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
      }
      else{
        pT <- input$pC / input$marginval_b2
        val <- multi.margin(delta = input$marginval_b2, muC = input$pC, varC = input$pC * (1 - input$pC), varT = pT * (1 - pT),
                            alpha = as.numeric(input$type1_b), beta = 1 - as.numeric(input$power_b))
        data.frame(Output = c("Sample Size in Control",
                              "Sample Size in Treatment",
                              "Randomization control to treatment (k:1)",
                              "Realtive Efficiency (compared to 1:1)"),
                   Values = c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
      }
    }
    
    
    else if(input$dist == "Poisson"){
      if(input$margin_p == "add"){
        lambdaT <- input$lambdaC - input$marginval_p1
        val <- add.margin(delta = input$marginval_p1, varC = input$lambdaC, varT = lambdaT,
                          alpha = as.numeric(input$type1_p), beta = 1 - as.numeric(input$power_p))
        data.frame(Output = c("Sample Size in Control",
                              "Sample Size in Treatment",
                              "Randomization control to treatment (k:1))",
                              "Realtive Efficiency (compared to 1:1)"),
                   Values =  c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
      }
      else{
        lambdaT <- input$lambdaC / input$marginval_p2
        val <- multi.margin(delta = input$marginval_p2, muC = input$lambdaC, varC = input$lambdaC , varT = lambdaT,
                            alpha = as.numeric(input$type1_p), beta = 1 - as.numeric(input$power_p))
        data.frame(Output = c("Sample Size in Control",
                              "Sample Size in Treatment",
                              "Randomization control to treatment (k:1)",
                              "Realtive Efficiency (compared to 1:1)"),
                   Values = c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
      }
    }
    })
  
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
}
