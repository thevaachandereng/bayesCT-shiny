library(shiny)
library(bayesDP)
library(plotly)

fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  # App title 
  titlePanel("Adaptive Bayesian Clinical Trial Analysis"),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
  sidebarPanel(
  # Sidebar layout with input and output definitions ----
  selectInput("dist", "Distribution of the data", c(Binomial = "Binomial", Normal = "Normal"), selected = "Binomial"),
  fileInput("file", "Data Input (CSV file only)",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  # Input: Checkbox if file has header ----
  checkboxInput("header", "Header", TRUE),
  
  # Input: Select separator ----
  radioButtons("sep", "Separator",
               choices = c(Comma = ",",
                           Semicolon = ";",
                           Tab = "\t"),
               selected = ","),
  
  radioButtons("hist_data", "Include Historical Data:", c("Yes" = "TRUE", "No" = "FALSE"), selected = "FALSE"), 
  checkboxInput("OPC", "OPC Trial", FALSE),
  
  conditionalPanel(
    condition = "input.dist == 'Binomial' & input.hist_data == 'TRUE'",
    textInput("y0_treatment", label = "Number of Events in the Historical Treatment Group:", "0"), 
    textInput("N0_treatment", label = "Number of Sample Size in the Historical Treatment Group:", "0"),
    textInput("y0_control", label = "Number of Events in the Historical Control Group:", "0"), 
    textInput("N0_control", label = "Number of Sample Size in the Historical Control Group:", "0")
    ),
  
  conditionalPanel(
    condition = "input.dist == 'Normal' & input.hist_data == 'TRUE'",
    textInput("mu0_treatment", label = "Mean of the Historical Treatment Group:", "0"), 
    textInput("sigma0_treatment", label = "Standard Deviation of the Historical Treatment Group:", "0"),
    textInput("N0_treatment1", label = "Number of Observations of the Historical Treatment Group:", "0"),
    textInput("mu0_control", label = "Mean of the Historical Control Group:", "0"), 
    textInput("sigma0_control", label = "Standard Deviation of the Historical Control Group:", "0"),
    textInput("N0_control1", label = "Number of Observations of the Historical Control Group:", "0")
  ),
  
    radioButtons("alternative", "Alternative Hypothesis:", c("Less" = "less", "Greater" = "greater", "Two-Sided" = "two-sided"), selected = "less"), 
    sliderInput("N_impute", "Number of Monte Carlo Imputation for Predictive Distribution:", min = 50, max = 1000, value = 100, step = 10),
    sliderInput("number_mcmc", "Number of Sampling from the Posterior Distribution:", min = 100, max = 10000, value = 1000, step = 100),
    sliderInput("prob_ha", "Posterior Probability of Accepting Alternative Hypothesis:", min = 0.90, max = 1.00, value = 0.95, step = 0.005),
    sliderInput("futility_prob", "Probability of Futility:", min = 0.00, max = 0.20, value = 0.10, step = 0.01),
    sliderInput("expected_success_prob","Probability of Early Success:", min = 0.80, max = 1.00, value = 0.90, step = 0.01),
  
  
  # Horizontal line ----
  tags$hr(),
  
  conditionalPanel(
    condition = "input.dist == 'Binomial'",
    h5(withMathJax("Prior Distribution: Beta(a, b)")),
    textInput("a_0", label = "a:", "1"),
    textInput("b_0", label = "b:", "1")
  ),
  
  selectInput("discount_function", "Discount function (Historical Data) :", 
              c("Identity" = "identity", "Weibull" = "weibull", "Scaled Weibull" = "scaledweibull"), 
              selected = "identity"),
  
  
  radioButtons("fix_alpha", label = withMathJax("Fix $\\alpha$ (Historical Data):"), 
               choices = c("True" = "TRUE", "False" = "FALSE"), selected = "FALSE"), 
  
  textInput("alpha_max", label = withMathJax("Max  $\\alpha$ (Historical Data):"), "1"), 
  
    p("GPL-3 License"),
    p("Copyright (c) 2018 Thevaa Chandereng, Donald Musgrove, Tarek Haddad, 
      Graeme Hickey, Tim Hanson, Theodore Lystig")
    ),
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Table summarizing the values entered ----
      h3("Results"),
      tableOutput("values")
    ))
  )
