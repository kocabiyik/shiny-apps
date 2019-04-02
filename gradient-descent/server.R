library(shiny)
library(dplyr)
library(ggplot2)
library(markdown)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  grad <- reactive({
    
    # alpha: learning rate
    alpha <- input$learning_rate
    
    # number of iterations
    iteration <- input$iteration
    
    # number of data points
    m <- nrow(mtcars)
    
    # initial theta values
    theta <- c(1, 1)
    
    # variables
    x1 <- rep(1, m)
    x2 <- mtcars$wt
    y <- mtcars$mpg
    
    # placeholder vectors for parameter updates
    updates_theta0 <- c()
    updates_theta1 <- c()
    updates_cost <- c()
    
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
      updates_theta0[i] <- theta[1]
      updates_theta1[i] <- theta[2]
      updates_cost[i] <- cost
      
    }
    records <- data.frame(theta0 = updates_theta0,
                          theta1 = updates_theta1,
                          cost = updates_cost,
                          iter = 1:iteration)
    
    return(records)
  })
  
  # plot linear line
  output$reg_line <- renderPlot({
    records <- grad()
    theta0 <- tail(records, 1)$theta0 %>% as.numeric()
    theta1 <- tail(records, 1)$theta1 %>% as.numeric()
    
    mtcars %>% ggplot(aes(wt, mpg))+
      geom_point(color = "#2F4F4F")+
      geom_abline(intercept = theta0,
                  slope = theta1,
                  color = "#ea596e",
                  size = 1)+
      theme(panel.background = element_rect(fill = "#e9ecef",
                                      colour = "#e9ecef",
                                      size = 0.5, linetype = "solid"),
            plot.background = element_rect(fill = "#fafafa"))
  })
  
  # parameters
  output$parameters <- renderPlot({
    records <- grad()
    theta0 <- tail(records, 1)$theta0 %>% as.numeric()
    theta1 <- tail(records, 1)$theta1 %>% as.numeric()
    
    records %>% ggplot(aes(x = theta0, y = theta1))+
      geom_point(color = "#2F4F4F",
                 size = 0.4)+
      xlim(c(-10, 50))+
      ylim(c(-10, 10))+
      theme(panel.background = element_rect(fill = "#e9ecef",
                                            colour = "#e9ecef",
                                            size = 0.5, linetype = "solid"),
            plot.background = element_rect(fill = "#fafafa")) +
      xlab(expression(theta[0]))+
      ylab(expression(theta[1]))
  })
  
  # cost
  output$cost <- renderPlot({
    records <- grad()
    records %>% ggplot(aes(iter, cost))+
      geom_point(size = 0.4,
                 color = "#2F4F4F")+
      theme(panel.background = element_rect(fill = "#e9ecef",
                                            colour = "#e9ecef",
                                            size = 0.5, linetype = "solid"),
            plot.background = element_rect(fill = "#fafafa"))+
      ylab("Cost")+
      xlab("Iteration")
  })
}