shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Lato|Merriweather|Work+Sans:100');

                    h2, h3, h4, h5, h6 {
                    font-family: 'Lato', sans-serif;
                    font-size: 1.80em;
                    color: #244272;
                    }

                    h1 {
                    text-align: center;
                    font-size: 3em;
                    font-family: 'Work Sans', sans-serif;
                    font-weight:10;
                    }
                    
                    p, ol {
                    font-family: 'Merriweather', serif;
                    margin-top: 15px;
                    font-size: 1.20em;
                    color: #545454;
                    }
                    "))
    ),
  
  headerPanel("GRADIENT DESCENT VISUALIZATION"),
  
  sidebarPanel(
    withMathJax("$$h(\\theta_x) = \\theta_0+\\theta_1*x$$"),
    
    sliderInput("theta0", "Initial value of theta0", 
                min = -10, max = 50, value = -5),
    
    sliderInput("theta1", "Initial value of theta1", 
                min = -10, max = 10, value = 0),
    
    sliderInput("learning_rate", "Set Learning Rate:", 
                min = 0.001, max = 0.1, value = 0.03),
    
    sliderInput("iteration", "Number of Iteration", 
                min = 1, max = 1000, value = 250)
  ),
  
  mainPanel(
    tabsetPanel(
    tabPanel("Application",
             h3("Linear Regression"),
             plotOutput("reg_line", width = 600),
             h3("Cost Function over Iterations"),
             plotOutput("cost", width = 600),
             h3("Parameter Updates"),
             plotOutput("parameters", width = 600)),
    tabPanel("Documentation",
             includeMarkdown("https://raw.githubusercontent.com/kocabiyik/shiny-apps/master/gradient-descent/www/documentation.md")
             )
    )
  )
))