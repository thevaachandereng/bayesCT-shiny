#calling two different libraries
library(shiny)
library(bayesDP)
library(dplyr)
library(stats)

function(input, output){
  
  sliderValues <- reactive({
    # for normal data
    if(input$dist == "Normal"){
      if(input$margin_n == "add"){
      val <- add.margin(delta = input$marginval_n1, varC = input$varC, varT = input$varT,
                        alpha = as.numeric(input$type1_n), beta = 1 - as.numeric(input$power_n))
      data.frame(Output = c("Sample Size in Control",
                            "Sample Size in Treatment",
                            "Randomization control to treatment (k:1))",
                            "Realtive Efficiency (compared to 1:1)"),
                 Values =  c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
    }
    else{
      val <- multi.margin(delta = input$marginval_n2, muC = input$muC, varC = input$varC, varT = input$varT,
                          alpha = as.numeric(input$type1_n), beta = 1 - as.numeric(input$power_n))
      data.frame(Output = c("Sample Size in Control",
                            "Sample Size in Treatment",
                            "Randomization control to treatment (k:1)",
                            "Realtive Efficiency (compared to 1:1)"),
                 Values = c(ceiling(val$control), ceiling(val$treatment), val$randomization, val$efficiency))
    }
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
  
  
  
  eff <- reactive({
    
    if(input$dist == "Normal"){      
      if(input$margin_n == "add"){
        data.frame(control = ceiling(h * sum(as.numeric(add.margin(delta = input$marginval_n1, varC = input$varC, varT = input$varT,
                                     alpha = as.numeric(input$type1_n), beta = 1 - as.numeric(input$power_n)))[1:2])),
                   eff1 = (input$varC / h + input$varT/ (1 - h)) / (sqrt(input$varC) + sqrt(input$varT))^2)
      }
      else{
        data.frame(control = ceiling(h * sum(as.numeric(multi.margin(delta = input$marginval_n2, muC = input$muC, varC = input$varC, varT = input$varT,
                                                                     alpha = as.numeric(input$type1_n), beta = 1 - as.numeric(input$power_n)))[1:2])),
                   eff1 = (input$varC / h + input$marginval_n2^2 * input$varT/ (1 - h)) / (sqrt(input$varC) + input$marginval_n2 * sqrt(input$varT))^2)
      }
    }
    
    else if(input$dist == "Binomial"){      
      if(input$margin_b == "add"){
        pT <- input$pC - input$marginval_b1
        data.frame(control = ceiling(h * sum(as.numeric(add.margin(delta = input$marginval_b1, varC = input$pC * (1 - input$pC), varT = pT * (1 - pT),
                            alpha = as.numeric(input$type1_b), beta = 1 - as.numeric(input$power_b)))[1:2])),
                   eff1 = (input$pC *(1 - input$pC)  / (h) + pT *(1 - pT) / (1 - h)) / (sqrt(input$pC *(1 - input$pC)) + sqrt(pT *(1 - pT)))^2)
      }
      else{
        pT <- input$pC / input$marginval_b2
        data.frame(control = ceiling(h * sum(as.numeric(multi.margin(delta = input$marginval_b2, muC = input$pC, varC = input$pC * (1 - input$pC), varT = pT * (1 - pT),
                                              alpha = as.numeric(input$type1_b), beta = 1 - as.numeric(input$power_b)))[1:2])),
                   eff1 = (input$pC *(1 - input$pC)  / h + input$marginval_b2^2 * pT *(1 - pT) / (1 - h)) / (sqrt(input$pC *(1 - input$pC)) + input$marginval_b2 * sqrt(pT *(1 - pT)))^2)
      }
    }

    
    else if(input$dist == "Poisson"){      
      if(input$margin_p == "add"){
        lambdaT  <- input$lambdaC - input$marginval_p1
        data.frame(control = ceiling(h * sum(as.numeric(add.margin(delta = input$marginval_p1, varC = input$lambdaC, varT = lambdaT,
                    alpha = as.numeric(input$type1_p), beta = 1 - as.numeric(input$power_p)))[1:2])),
                   eff1 = (input$lambdaC / h + lambdaT/ (1 - h)) / (sqrt(input$lambdaC) + sqrt(lambdaT))^2)
      }
      else{
        lambdaT  <- input$lambdaC / input$marginval_p2
        data.frame(control = ceiling(h * sum(as.numeric(multi.margin(delta = input$marginval_p2, muC = input$lambdaC, varC = input$lambdaC, varT = lambdaT,
                                                                     alpha = as.numeric(input$type1_p), beta = 1 - as.numeric(input$power_p)))[1:2])),
                   eff1 = (input$lambdaC / h + input$marginval_p2^2 * lambdaT/ (1 - h)) / (sqrt(input$lambdaC) + input$marginval_p2 * sqrt(lambdaT))^2)
      }
    }
})
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
}
