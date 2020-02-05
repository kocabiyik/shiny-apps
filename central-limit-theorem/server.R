library(shiny)
server <- function(input, output) {
    
    # simulate data ----
    data_sets <- reactiveValues()
    set.seed(1)
    data_sets$population <- c(rnorm(5000, mean = 3, sd = 4), rnorm(5000, mean = 15, sd = 3))
    
    # plot population ----
    output$population <- renderPlot({
        par(bg = '#fafafa')
        hist(data_sets$population, breaks = 100, col = '#563D7C', main = 'Histogram of the Population', xlab = 'x')
    })
    
    # central limit theorem visualization ----
    output$clt <- renderPlot({
        sample_size <- input$sample_size
        trial = input$trial
        
        for (i in 1:trial){
            if (i == 1) { observed_means = rep(NA, trial) }
            observed_means[i] = mean(sample(data_sets$population, sample_size))
        }
        par(bg = '#fafafa')
        hist(observed_means, breaks = 100, col = '#563D7C', main = 'Observed means of the samples', xlab = 'Observed Means')
    })
}