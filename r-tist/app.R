library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("R-tist!",
           tabPanel("Plot",
                    sidebarLayout(
                      sidebarPanel(
                        fileInput(inputId = 'img_file', label = "Image", accept = c("image/jpg", "image/png")),
                        sliderInput("n", "n", 10, 100, 50, width = "100%")
                      ),
                      mainPanel(
                        imageOutput('img_out')
                      )
                    )
           ),
           tabPanel("Inspiration",
                    HTML('<blockquote class="twitter-tweet" data-lang="en">
					<p lang="en" dir="ltr">Closer look🔬<a href="https://twitter.com/hashtag/washington?src=hash&amp;ref_src=twsrc%5Etfw">#washington</a>  <a href="https://twitter.com/hashtag/Avatar?src=hash&amp;ref_src=twsrc%5Etfw">#Avatar</a>  <a href="https://twitter.com/hashtag/UP?src=hash&amp;ref_src=twsrc%5Etfw">#UP</a>  <a href="https://twitter.com/hashtag/minecraft?src=hash&amp;ref_src=twsrc%5Etfw">#minecraft</a>  <a href="https://twitter.com/hashtag/kensukekoike?src=hash&amp;ref_src=twsrc%5Etfw">#kensukekoike</a>  <a href="https://twitter.com/hashtag/todayscuriosity?src=hash&amp;ref_src=twsrc%5Etfw">#todayscuriosity</a>  <a href="https://t.co/b9p5rYSv3l">pic.twitter.com/b9p5rYSv3l</a>
					</p>&mdash; Kensuke Koike 小池健輔 (@k_koi) <a href="https://twitter.com/k_koi/status/1027667091690868736?ref_src=twsrc%5Etfw">August 9, 2018</a>
				</blockquote>
				<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')
           )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$img_out <- renderImage({
    library(magick)
    
    n = as.integer(input$n)
    
    # source----
    img_file <- input$img_file
    
    if(!is.null(input$img_file)) {
      img <- magick::image_read(img_file$datapath)
    } else 
      img <- magick::image_read("image.jpg")
    
    if(magick::image_info(img)$width > 1000) {img <- magick::image_resize(img, "1000x1000")}
    
    # specs
    img_data <- magick::image_info(img)
    img_height = img_data$height
    img_width = img_data$width
    
    # number of squares
    n_width = img_width/n
    n_height = img_height/n
    
    # cropping
    height_new = img_height-img_height%%n
    width_new = img_width-img_width%%n
    new_img_size = paste0(width_new, "x", height_new)
    img_cropped <- magick::image_crop(img, new_img_size)
    
    # slicing and combining
    for (i in 0:(n_height-1)){ # width
      if(i%%2==0) {next}
      for (j in 1:(n_width-1)){ # height
        if(j%%2==0) {next}
        size <- paste(n, "x", n, sep = "")
        size_str <- paste(size, "+", j*n, "+", i*n, sep = "")
        img_j <- magick::image_crop(img_cropped, size_str)
        
        if(exists("img_ij_stored")){
          img_ij_stored <- magick::image_append(image_scale(c(img_ij_stored, img_j)), stack = FALSE)
        } else {
          img_ij_stored <- img_j
        }
        img_j_out <- img_ij_stored
        
      }
      rm(img_ij_stored)
      
      if(exists("img_j_stored")){
        img_j_stored <- magick::image_append(image_scale(c(img_j_stored, img_j_out)), stack = TRUE)
      } else {
        img_j_stored <- img_j_out
      }
      
    }
    
    tmpfile <- img_j_stored %>% image_write(tempfile(fileext='jpg'), format = 'jpg')
    list(src = tmpfile, contentType = "image/jpeg")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)