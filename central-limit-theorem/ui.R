library(shiny)
library(markdown)
source('global.R')

fluidPage(title = APP_TITLE, theme = 'https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css',
          
          suppressDependencies("bootstrap"), # do not let Shiny to include bootstrap CSS by default
          
          # additional style sources in the head tag ----
          tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "base.css")),
          
          tags$head(HTML('<script defer src="https://use.fontawesome.com/releases/v5.0.2/js/all.js"></script>')),
                    
          # fonts
          tags$style("@import url('https://fonts.googleapis.com/css?family=Open+Sans|Merriweather&display=swap');"),
          
          # math
          tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML")),
          
          # source the HTML with input styles
          # it is not CSS but HTML because PRIMARY_COLOR value can be passed to the HTML template.
          htmlTemplate("www/inputs-style.html", primary_color = PRIMARY_COLOR),
          
          # navigation bar --- 
          htmlTemplate("www/navbar.html", primary_color = PRIMARY_COLOR),
          
          # keep the content in a container ----
          HTML('<div class="container">'),
          
          # markdown 1 ----
          includeMarkdown('markdown/markdown1.md'),
          
          # plot 1 ---
          htmlTemplate("www/plot1.html", plot1 = plotOutput("plot1")),
          
          # markdown 2 ----
          includeMarkdown('markdown/markdown2.md'),
          
          # plot 2 ---
          htmlTemplate("www/plot2.html",
                       slider_sample_size = sliderInput("sample_size", "Sample Size",
                                                        min = 1, max = 50, value = 1),
                       
                       slider_trial = sliderInput("trial", "Trials:",
                                                  min = 200, max = 5000, value = 500),
                       
                       plot2 = plotOutput("plot2")
          ),
          
          # end of the container ----
          HTML('</div>'),
          
          # additional sources to be loaded. Eg: Google Analytics, D3 Javascripts, other tracking codes ----
          # better to keep them in the bottom of the HTML.
          htmlTemplate("www/sources.html")
)