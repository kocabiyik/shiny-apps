library(shiny)
source('global.R')

server <- function(input, output) {
    
    # simulate data ----
    data_sets <- reactiveValues()
    set.seed(1)
    data_sets$population <- c(rnorm(5000, mean = 3, sd = 4), rnorm(5000, mean = 15, sd = 3))
    
    # plot population ----
    output$plot1 <- renderPlot({
        hist(data_sets$population, breaks = 100, col = PRIMARY_COLOR,
             main = 'Histogram of the Population', xlab = 'x')
    })
    
    # central limit theorem visualization ----
    output$plot2 <- renderPlot({
        sample_size <- input$sample_size
        trial = input$trial
        
        for (i in 1:trial){
            if (i == 1) { observed_means = rep(NA, trial) }
            observed_means[i] = mean(sample(data_sets$population, sample_size))
        }
        hist(observed_means, breaks = 100, col = PRIMARY_COLOR, 
             main = 'Observed means of the samples', xlab = 'Observed Means')
    })
}