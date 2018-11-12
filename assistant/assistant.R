# libraries ----
library(ggplot2)
library(magick)
library(imager)
library(dplyr)

# settings ----
file <- "image.jpg"
nx <- 10
ny <- 15

# select a simlification
simplification <- c('canny', 'edge', 'grayscale', 'fuzzymeans', 'None')
simplification <- simplification[3]

# masking
rect_vert <- c(0, 10)
rect_hor <- c(0, 15)

# grid lines
grid_color = c('black', 'white', 'deepskyblue', 'darkorchid1', 'darksalmon')
grid_color = grid_color[2]
grid_alpha <- 0.5

# grid numbers
numbers_size <- 4
numbers_color = c('black', 'white', 'deepskyblue', 'darkorchid1', 'darksalmon')
numbers_color = numbers_color[4]
numbers_left_padding <- 0
numbers_upper_padding <- 0

# plot ----
img <- magick::image_read("image.jpg")

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
