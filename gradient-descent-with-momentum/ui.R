library(shiny)
library(markdown)
source('global.R')

fluidPage(title = APP_TITLE, theme = 'bootstrap.min.css',
          
          suppressDependencies("bootstrap"), # do not let Shiny to include bootstrap CSS by default
          
          # additional style sources in the head tag ----
          tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "base.css")),
          
          tags$head(HTML('<script defer src="https://use.fontawesome.com/releases/v5.0.2/js/all.js"></script>')),
                    
          # fonts ----
          tags$style("@import url('https://fonts.googleapis.com/css?family=Open+Sans|Merriweather&display=swap');"),
          
          # math ----
          tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML")),
          
          # source the HTML with input styles ----
          # it is not CSS but HTML because PRIMARY_COLOR value can be passed to the HTML template.
          htmlTemplate("www/inputs-style.html", primary_color = PRIMARY_COLOR),
          
          # navigation bar --- 
          htmlTemplate("www/navbar.html", primary_color = PRIMARY_COLOR),
          
          # keep the content in a container ----
          HTML('<div class="container">'),
          
          # markdown 1 ----
          includeMarkdown('markdown/markdown1.md'),
          
          # plot 1 ---
          htmlTemplate("www/plot1.html",
                       iteration = sliderInput("iteration", "Iterations",
                                                        min = 1, max = 500, value = 100),
                       
                       learning_rate = sliderInput("learning_rate", "Learning Rate",
                                                  min = 0.0001, max = 0.001, value = 0.0003),
                       theta = sliderInput("beta", " Momentum",
                                                   min = 0.01, max = 0.99, value = 0.02),
                       
                       plot1 = plotOutput("plot1")
          ),
          
          # end of the container ----
          HTML('</div>'),
          
          # additional sources to be loaded. Eg: Google Analytics, D3 Javascripts, other tracking codes ----
          # better to keep them in the bottom of the HTML.
          htmlTemplate("www/sources.html")
)