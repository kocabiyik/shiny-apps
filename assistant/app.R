library(shiny)
library(ggplot2)
library(magick)
library(imager)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  title = "A Helper Hand",
  
  plotOutput('img_out', height = 800),
  
  hr(),
  
  fluidRow(
    fileInput(inputId = 'img_file', label = "Image", accept = c("image/jpg", "image/png")),
    hr(),
    column(2,
           h4("Aspect Ratio"),
           numericInput('n_width', 'Width', min = 6, max = 15, value = 15),
           numericInput('n_height', 'Height', min = 6, max = 15, value = 10)
    ),
    column(4,
           h4("Grid"),
           selectInput(inputId = 'grid_color',
                       label = 'Color',
                       choices = c(
                         'black',
                         'white',
                         'deepskyblue',
                         'darkorchid1',
                         'darksalmon',
                         'ghostwhite',
                         'deeppink1',
                         'darkslategray1',
                         'khaki2',
                         'lavenderblush',
                         'lightblue1',
                         'lightcyan1',
                         'hotpink1',
                         'ivory1',
                         'lightpink1',
                         'mediumorchid1',
                         'mistyrose',
                         'mediumpurple1',
                         'lightsteelblue',
                         'peachpuff1',
                         'pink1'),
                       selected = 'mistyrose'),
           br(),
           sliderInput("grid_alpha", "Alpha", min = 0, max = 1, value = 0.4, width = "100%")
    ),
    column(4,
           h4("Numbers"),
           selectInput(inputId = 'numbers_color', label = 'Numeric Labels Color',
                       choices = c(
                         'black',
                         'white',
                         'deepskyblue',
                         'darkorchid1',
                         'darksalmon',
                         'ghostwhite',
                         'deeppink1',
                         'darkslategray1',
                         'khaki2',
                         'lavenderblush',
                         'lightblue1',
                         'lightcyan1',
                         'hotpink1',
                         'ivory1',
                         'lightpink1',
                         'mediumorchid1',
                         'mistyrose',
                         'mediumpurple1',
                         'lightsteelblue',
                         'peachpuff1',
                         'pink1',
                         'black'),
                       selected = 'mistyrose'),
           sliderInput("numbers_size", "Size", min = 1, max = 10, value = 4, width = "100%"),
           numericInput('numbers_left_padding', 'Upper Padding', value = 0.15, min = 0),
           numericInput('numbers_upper_padding', 'Left Padding', value = 0.15, min = 0)
    ),
    column(2,
           h4("Simplification"),
           radioButtons('simplification', 'Method', choices = c('canny', 'edge', 'grayscale', 'fuzzymeans', 'None'), selected = 'None')
           
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$img_out <- renderPlot({

    # import
    if(!is.null(input$img_file)) {
      img <- magick::image_read(input$img_file$datapath)
    } else 
      img <- magick::image_read("image.jpg")
    
    if(magick::image_info(img)$width > 1000) {img <- magick::image_resize(img, "1000x1000")}
    n_width <- input$n_width
    n_height = input$n_height
    
    # specs
    img_data <- magick::image_info(img)
    img_height = img_data$height
    img_width = img_data$width
    
    # cropping
    height_new = img_height-img_height%%n_width
    width_new = img_width-img_width%%n_height
    new_img_size = paste0(width_new, "x", height_new)
    img <- magick::image_crop(img, new_img_size)
    
    # edge
    if(input$simplification == 'edge'){
      img <- magick::image_edge(img)
    }
    
    if(input$simplification == 'canny'){
      img <- magick::image_canny(img)
    }
    
    if(input$simplification == 'grayscale'){
      img <- image_quantize(img, colorspace = 'gray')
    }
    
    if(input$simplification == 'fuzzymeans'){
      img <- image_fuzzycmeans(img)
    }
    
    # new specs
    df_vlines <- data_frame(vlines = seq(1, image_info(img)$width, image_info(img)$width/n_width)) %>% 
      tibble::rownames_to_column((var = "label"))
    df_hlines <- data_frame(hlines = seq(1, image_info(img)$height, image_info(img)$height/n_height)) %>% 
      tibble::rownames_to_column((var = "label"))
    
    # masking
    magick::image_ggplot(img)+
      geom_vline(data = df_vlines, aes(xintercept = vlines), color = input$grid_color, alpha = input$grid_alpha)+
      geom_hline(data = df_hlines, aes(yintercept = hlines), color = input$grid_color, alpha = input$grid_alpha)+
      geom_text(data = df_vlines, aes(x = vlines, label = label), y = -(input$numbers_left_padding)*(image_info(img)$height/n_height), size = input$numbers_size, color = input$numbers_color, alpha = 0.8)+
      geom_text(data = df_hlines, aes(y = hlines, label = label), x = (input$numbers_upper_padding)*(image_info(img)$width/n_width), size = input$numbers_size, color = input$numbers_color, alpha = 0.8)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)