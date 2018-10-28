binomial_analysis <- function(file                  = input$file, 
                              header                = input$header,
                              sep                   = input$sep,
                              y0_treatment          = input$y0_treatment,
                              N0_treatment          = input$N0_treatment,
                              y0_control            = input$y0_control, 
                              N0_control            = input$N0_control,
                              alternative           = input$alternative,
                              N_impute              = input$N_impute, 
                              number_mcmc           = input$number_mcmc,
                              prob_ha               = input$prob_ha,
                              futility_prob         = input$futility_prob, 
                              expected_success_prob = input$expected_success_prob,
                              prior                 = c(input$a_0, input$b_0), 
                              discount_function     = input$discount_function, 
                              fix_alpha             = input$fix_alpha,
                              alpha_max             = input$alpha_max
                              
                              
){
  
  print(y0_control)
}
              