normal_analysis <- function(
  data                  = NULL,
  mu0_treatment         = NULL,
  sd0_treatment         = NULL,
  N0_treatment          = NULL,
  mu0_control           = NULL,
  sd0_control           = NULL,
  N0_control            = NULL,
  alternative           = "greater",
  N_impute              = 100,
  h0                    = 0,
  number_mcmc           = 10000,
  prob_ha               = 0.95,
  futility_prob         = 0.10,
  expected_success_prob = 0.90,
  discount_function     = "identity",
  fix_alpha             = FALSE,
  alpha_max             = 1,
  weibull_scale         = 0.135,
  weibull_shape         = 3
){
  #reading the data
  data_total <- data
  
  data_interim <- data_total %>%
    mutate(futility = complete == 0)
  
  data <- data_interim %>%
    filter(!futility)
  
  summary <- data %>%
    group_by(treatment) %>%
    summarize(mu_outcome = mean(outcome), sd_outcome = sd(outcome))
  
  
  if(sum(data$treatment == 0) != 0){
    mu_c <- mean(data$outcome[data$treatment == 0])
    sd_c <- sd(data$outcome[data$treatment == 0])
    N_c  <- length(data$outcome[data$treatment == 0])
  }
  else{
    mu_c <- NULL
    sd_c <- NULL
    N_c  <- NULL
  }
  
  # analyze the data using bayesDp
  post <-  bdpnormal(mu_t                = mean(data$outcome[data$treatment == 1]),
                     sigma_t             = sd(data$outcome[data$treatment == 1]),
                     N_t                 = length(data$treatment == 1),
                     mu_c                = mu_c,
                     sigma_c             = sd_c,
                     N_c                 = N_c,
                     mu0_t               = mu0_treatment,
                     sigma0_t            = sd0_treatment,
                     N0_t                = N0_treatment,
                     mu0_c               = mu0_control,
                     sigma0_c            = sd0_control,
                     N0_c                = N0_control,
                     number_mcmc         = number_mcmc,
                     discount_function   = discount_function,
                     alpha_max           = alpha_max,
                     fix_alpha           = fix_alpha,
                     weibull_scale       = weibull_scale,
                     weibull_shape       = weibull_shape)
  
  
  # assigning stop_futility and expected success
  stop_futility         <- 0
  stop_expected_success <- 0
  expected_success_test <- 0
  
  for(i in 1:N_impute){
    data_control_success_impute <- data_interim %>%
      filter(treatment == 0) %>%
      mutate(outcome_impute = ifelse(futility,
                                     rnorm(n(), summary$mu_outcome[1], summary$sd_outcome[1]),
                                     outcome))
    # imputing success for treatment group
    data_treatment_success_impute  <- data_interim %>%
      filter(treatment == 1) %>%
      mutate(outcome_impute = ifelse(futility,
                                     rnorm(n(), summary$mu_outcome[2], summary$sd_outcome[2]),
                                     outcome))
    
    # combine the treatment and control imputed datasets
    data_success_impute <- bind_rows(data_control_success_impute,
                                     data_treatment_success_impute) %>%
      mutate(outcome = outcome_impute) %>%
      select(-outcome_impute)
    
    # Create enrolled subject data frame for discount function analysis
    data <- data_success_impute
    
    # assigning input for control arm given it is a single or double arm
    if(sum(data$treatment == 0) != 0){
      mu_c <- mean(data$outcome[data$treatment == 0])
      sd_c <- sd(data$outcome[data$treatment == 0])
      N_c  <- length(data$outcome[data$treatment == 0])
    }
    else{
      mu_c <- NULL
      sd_c <- NULL
      N_c  <- NULL
    }
    
    # analyze complete+imputed data using discount funtion via binomial
    post_imp <- bdpnormal(mu_t                = mean(data$outcome[data$treatment == 1]),
                          sigma_t             = sd(data$outcome[data$treatment == 1]),
                          N_t                 = length(data$treatment == 1),
                          mu_c                = mu_c,
                          sigma_c             = sd_c,
                          N_c                 = N_c,
                          mu0_t               = mu0_treatment,
                          sigma0_t            = sd0_treatment,
                          N0_t                = N0_treatment,
                          mu0_c               = mu0_control,
                          sigma0_c            = sd0_control,
                          N0_c                = N0_control,
                          number_mcmc         = number_mcmc,
                          discount_function   = discount_function,
                          alpha_max           = alpha_max,
                          fix_alpha           = fix_alpha,
                          weibull_scale       = weibull_scale,
                          weibull_shape       = weibull_shape)
    
    if(sum(data$treatment == 0) != 0){
      if(alternative == "two-sided"){
        effect_imp <- post_imp$posterior_treatment$posterior_mu - post_imp$posterior_control$posterior_mu
        success <- max(c(mean(effect_imp > h0), mean(-effect_imp > h0)))
      }
      else if(alternative == "greater"){
        effect_imp <- post_imp$posterior_treatment$posterior_mu - post_imp$posterior_control$posterior_mu
        success <- mean(effect_imp > h0)
      }
      else{
        effect_imp <- post_imp$posterior_treatment$posterior_mu - post_imp$posterior_control$posterior_mu
        success <- mean(-effect_imp > h0)
      }
    }
    
    else{
      effect_imp <- post_imp$final$posterior_mu
      if(alternative == "two-sided"){
        success <- max(c(mean(effect_imp > h0), mean(effect_imp < h0)))
      }
      else if(alternative == "greater"){
        success <- mean(effect_imp > h0)
      }
      else{
        success <- mean(effect_imp < h0)
      }
    }
    
    if(success > prob_ha){
      expected_success_test <- expected_success_test + 1
    }
    
  }
  
  if(expected_success_test / N_impute < futility_prob){
    stop_futility       <- 1
  }
  
  # Test if expected success criteria met
  if(expected_success_test / N_impute > expected_success_prob ){
    stop_expected_success <- 1
  }
  
  data_final <- data_interim %>%
    filter(!futility)
  
  if(sum(data_final$treatment == 0) != 0){
    mu_c <- mean(data_final$outcome[data_final$treatment == 0])
    sd_c <- sd(data_final$outcome[data_final$treatment == 0])
    N_c  <- length(data_final$outcome[data_final$treatment == 0])
  }
  else{
    mu_c <- NULL
    sd_c <- NULL
    N_c  <- NULL
  }
  
  # Analyze complete data using discount funtion via binomial
  post_final <- bdpnormal(mu_t                = mean(data_final$outcome[data_final$treatment == 1]),
                          sigma_t             = sd(data_final$outcome[data_final$treatment == 1]),
                          N_t                 = length(data_final$treatment == 1),
                          mu_c                = mu_c,
                          sigma_c             = sd_c,
                          N_c                 = N_c,
                          mu0_t               = mu0_treatment,
                          sigma0_t            = sd0_treatment,
                          N0_t                = N0_treatment,
                          mu0_c               = mu0_control,
                          sigma0_c            = sd0_control,
                          N0_c                = N0_control,
                          discount_function   = discount_function,
                          number_mcmc         = number_mcmc,
                          alpha_max           = alpha_max,
                          fix_alpha           = fix_alpha,
                          weibull_scale       = weibull_scale,
                          weibull_shape       = weibull_shape)
  
  ### Format and output results
  # Posterior effect size: test vs control or treatment itself
  if(sum(data_final$treatment == 0) != 0){
    if(alternative == "two-sided"){
      effect <- post_final$posterior_treatment$posterior_mu - post_final$posterior_control$posterior_mu
      post_paa <- max(c(mean(effect > h0), mean(-effect > h0)))
    }
    else if(alternative == "greater"){
      effect <- post_final$posterior_treatment$posterior_mu - post_final$posterior_control$posterior_mu
      post_paa <- mean(effect > h0)
    }
    else{
      effect <- post_final$posterior_treatment$posterior_mu - post_final$posterior_control$posterior_mu
      post_paa <- mean(-effect > h0)
    }
  }
  
  else{
    effect <- post_final$final$posterior_mu
    if(alternative == "two-sided"){
      post_paa <- max(c(mean(effect > h0), mean(effect < h0)))
    }
    else if(alternative == "greater"){
      post_paa <- mean(effect > h0)
    }
    else{
      post_paa <- mean(effect < h0)
    }
  }
  
  N_treatment  <- sum(data_final$treatment)         # Total sample size analyzed - test group
  N_control    <- sum(!data_final$treatment)        # Total sample size analyzed - control group
  N_enrolled   <- dim(data_total)[1]
  
  
  ## output
  results_list <- list(
    prob_of_accepting_alternative              = prob_ha,
    margin                                     = h0,                       # margin for error
    alternative                                = alternative,              # alternative hypothesis
    N_treatment                                = N_treatment,
    N_control                                  = N_control,
    N_complete                                 = N_treatment + N_control,
    N_enrolled                                 = N_enrolled,               # Total sample size enrolled when trial stopped
    post_prob_accept_alternative               = post_paa,                 # Posterior probability that alternative hypothesis is true
    est_final                                  = mean(effect),             # Posterior Mean of treatment effect
    stop_futility                              = stop_futility,            # Did the trial stop for futility
    stop_expected_success                      = stop_expected_success     # Did the trial stop for expected success
    #MLE_est                                   = MLE$coe[2],               # Treatment effect useing MLE
    #MLE_est_interim                           = MLE_int$coe[2]            # Treatment effect useing MLE at interim analysis
  )
  
  results_list
  
}

