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
           numericInput('nx', 'Width', min = 6, max = 15, value = 10),
           numericInput('ny', 'Height', min = 6, max = 15, value = 15),
           h4("Simplification"),
           radioButtons('simplification', 'Method', choices = c('canny', 'edge', 'grayscale', 'fuzzymeans', 'None'), selected = 'None')
    ),
    column(4,
           h4('Masking'),
           p('Vertical Rectangles'),
           sliderInput('rect_vert', label = "Rect Left", min = 0, max = 30, step = 1, value = c(1,5)),
           sliderInput('rect_hor', label = "Rect Horizontal", min = 0, max = 30, step = 1, value = c(3,6))
           ),
    column(3,
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
    column(3,
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
    
    nx <- input$nx
    ny <- input$ny
    
    # select a simlification
    simplification <- input$simplification
    
    # masking
    rect_vert <- input$rect_vert
    rect_hor <- input$rect_hor
    
    # grid lines
    grid_color = input$grid_color
    grid_alpha <- input$grid_alpha
    
    # grid numbers
    numbers_size <- input$numbers_size
    numbers_color = input$numbers_color
    numbers_left_padding <- input$numbers_left_padding
    numbers_upper_padding <- input$numbers_upper_padding
    
    # plot ----
    
    # resize if necessary
    if(magick::image_info(img)$width > 1000) {
      img <- magick::image_resize(img, "1000x1000")
    }
    
    # specs
    img_data <- magick::image_info(img)
    img_height = img_data$height
    img_width = img_data$width
    nxl = img_width/nx
    nyl = img_height/ny
    
    # cropping
    height_new = img_height-img_height%%nx
    width_new = img_width-img_width%%ny
    new_img_size = paste0(width_new, "x", height_new)
    img <- magick::image_crop(img, new_img_size)
    
    # edge
    if(simplification == 'edge'){
      img <- magick::image_edge(img)
    }
    
    if(simplification == 'canny'){
      img <- magick::image_canny(img)
    }
    
    if(simplification == 'grayscale'){
      img <- image_quantize(img, colorspace = 'gray')
    }
    
    if(simplification == 'fuzzymeans'){
      img <- image_fuzzycmeans(img)
    }
    
    # new specs
    df_vlines <- data_frame(vlines = seq(1, img_width, img_width/nx)) %>% 
      tibble::rownames_to_column((var = "label"))
    df_hlines <- data_frame(hlines = seq(1, img_height, img_height/ny)) %>% 
      tibble::rownames_to_column((var = "label"))
    
    # masking dataframe
    df_mask <- data_frame(
      xleft = (rect_vert[1]-1)*nxl,
      xright = (rect_vert[2]-1)*nxl,
      yup = -(rect_hor[1]-1)*nyl,
      ydown = -(rect_hor[2]-1)*nyl
    )
    
    magick::image_ggplot(img)+
      # left
      geom_rect(data = df_mask, 
                xmin = 0,
                aes(xmax = xleft),
                ymin = 0,
                ymax = -img_height,
                fill = 'white')+
      
      # right
      geom_rect(data = df_mask,
                aes(xmin = xright), 
                xmax = img_width,
                ymin = 0,
                ymax = -img_height,
                fill = 'white')+
      
      # up
      geom_rect(data = df_mask,
                xmin = 0, 
                xmax = img_width,
                ymin = 0,
                aes(ymax = -yup),
                fill = 'white')+
      # down
      geom_rect(data = df_mask,
                xmin = 0,
                xmax = img_width,
                aes(ymin = -ydown), 
                ymax = -img_height,
                fill = 'white')+
      geom_vline(data = df_vlines, aes(xintercept = vlines), color = grid_color, alpha = grid_alpha)+
      geom_hline(data = df_hlines, aes(yintercept = hlines), color = grid_color, alpha = grid_alpha)+
      geom_text(data = df_vlines, aes(x = vlines, label = label),
                y = -(numbers_left_padding)*(img_height/ny),
                size = numbers_size,
                color = numbers_color,
                alpha = 0.8)+
      geom_text(data = df_hlines, aes(y = hlines, label = label),
                x = (numbers_upper_padding)*(img_width/nx),
                size = numbers_size,
                color = numbers_color,
                alpha = 0.8)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)