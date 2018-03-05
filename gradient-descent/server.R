library(shiny)
library(dplyr)
library(ggplot2)
library(markdown)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  grad <- reactive({
    
    # alpha: learning rate
    alpha <- input$learning_rate
    
    # number of iterations
    iteration <- input$iteration
    
    # number of data points
    m <- nrow(mtcars)
    
    # initial theta values
    theta <- c(input$theta0, input$theta1)
    
    # variables
    x1 <- rep(1, m)
    x2 <- mtcars$wt
    y <- mtcars$mpg
    
    # a blank dataframe to record simultaneous updates
    records <- data_frame(
      iter = as.integer(),
      theta0 = as.numeric(),
      theta1 = as.numeric(),
      cost = as.numeric())
    
    # gradient descent
    for (i in 1:iteration){
      # predictions
      yhat <- theta[1]*x1+theta[2]*x2
      
      # cost
      cost <- sum((y-yhat)^2)/(2*m)
      
      # update theta
      theta[1] <- theta[1]-alpha*(1/m)*sum((yhat-y)*x1)
      theta[2] <- theta[2]-alpha*(1/m)*sum((yhat-y)*x2)
      
      # record updates
      records[i, ] <- c(i, theta[1], theta[2], cost)
    }
    return(records)
  })
  
  # plot linear line
  output$reg_line <- renderPlot({
    records <- grad()
    theta0 <- tail(records, 1)$theta0 %>% as.numeric()
    theta1 <- tail(records, 1)$theta1 %>% as.numeric()
    
    mtcars %>% ggplot(aes(wt, mpg))+
      geom_point()+
      geom_abline(intercept = theta0,
                  slope = theta1)
    })
  
  # cost
  output$cost <- renderPlot({
    records <- grad()
    records %>% ggplot(aes(iter, cost))+
      geom_point(size = 0.4)
  })
  
  # parameters
  output$parameters <- renderPlot({
    records <- grad()
    theta0 <- tail(records, 1)$theta0 %>% as.numeric()
    theta1 <- tail(records, 1)$theta1 %>% as.numeric()
  
  records %>% ggplot(aes(x = theta0, y = theta1))+
    geom_point()+
    xlim(c(-10, 50))+
    ylim(c(-10, 10))
  })
})