library(shiny)
library(dplyr)
library(ggplot2)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  output$reg_line <- renderPlot({
    # Himmelblau's function: https://en.wikipedia.org/wiki/Himmelblau%27s_function
    f <- function(x, y) { (x^2+y-11)^2 + (x+y^2-7)^2 }
    
    x <- seq(-6, 6, length = 100)
    y <- x
    z <- outer(x, y, f)
    
    # partial derivatives
    dx <- function(x,y) {4*x**3-4*x*y-42*x+4*x*y-14}
    dy <- function(x,y) {4*y**3+2*x**2-26*y+4*x*y-22}
    
    # gradient descent parameters
    iteration <- input$iteration
    learning_rate <- input$learning_rate
    x_val <- 6
    y_val <- 6
    
    # momentum parameters
    beta = input$beta
    vdx = 0
    vdy = 0
    
    updates_x <- vector("numeric", length = iteration)
    updates_y <- vector("numeric", length = iteration)
    updates_z <- vector("numeric", length = iteration)
    
    # parameter updates
    for (i in 1:iteration) {
      
      vdx = beta*vdx+dx(x_val,y_val)
      vdy = beta*vdy+dy(x_val,y_val)
      
      x_val <- x_val-learning_rate*vdx
      y_val <- y_val-learning_rate*vdy
      z_val <- f(x_val, y_val)
      
      updates_x[i] <- x_val
      updates_y[i] <- y_val
      updates_z[i] <- z_val
    }
    
    # Color palette (100 colors)
    col.pal<-colorRampPalette(c("yellow", "red"))
    colors<-col.pal(100)
    # height of facets
    z.facet.center <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4
    # Range of the facet center on a 100-scale (number of colors)
    z.facet.range<-cut(z.facet.center, 100)
    
    plt <- persp(x, y, z,
                 theta = 270,
                 phi = 35,
                 expand = 0.7,
                 col = colors[z.facet.range],
                 border = '#BE8AEE',
                 axes = FALSE, box = FALSE,
                 ltheta = 60, shade = 0.30
    )
    
    points(trans3d(updates_x[1:iteration], updates_y[1:iteration], updates_z[1:iteration], pmat = plt), pch = 16,
           cex = c(rep(0.6, i-1), 1.2),
           col = c(rep('white', i-1), 'black')
    )
  })
}