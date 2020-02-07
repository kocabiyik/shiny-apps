library(shiny)
library(glue)
source('global.R')

fluidPage(title = APP_TITLE, theme = 'bootstrap.min.css',
          
          suppressDependencies("bootstrap"), # do not let Shiny to include bootstrap CSS by default
          
          # additional style sources in the head tag ----
          tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "base.css"),
                    
          # fonts
          tags$style("@import url('https://fonts.googleapis.com/css?family=Open+Sans|Merriweather&display=swap');"),
              
          # if you would like to change the PRIMARY_COLOR in the global.R
          # then, also update the values in the inputs.css
          tags$link(rel = "stylesheet", type = "text/css", href = "inputs.css")),

          # navigation bar --- 
          htmlTemplate("www/navbar.html", primary_color = PRIMARY_COLOR),
          
          # plot ---
          htmlTemplate("www/plot.html",
                       slider = sliderInput("bins", "Number of bins:",
                             min = 1, max = 50, value = 30),
                       plot = plotOutput("distPlot"),
                       plot_color = PRIMARY_COLOR
                       ),
          
          # additional sources to be loaded. Eg: Google Analytics, D3 Javascripts, other tracking codes ----
          # better to keep them in the bottom of the HTML.
          htmlTemplate("www/sources.html")
)