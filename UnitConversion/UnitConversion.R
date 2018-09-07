### Change the lines below to align the scale bar ###
# This script is used to convert our measurements to the physical world

# Reference file
reference_file <- "reference_waves1and2.png"

# Scale bar (red)
scale_xstart <- 100
scale_xend <- 300
scale_y <- 100

# Time needed for plot to show 
# Slower computers might need to increase the time
pausetime <- 2

### Do not change anything below this line ###

# Check library installation
if("png" %in% rownames(installed.packages()) == FALSE){
  install.packages("png")
}
if("ggplot2" %in% rownames(installed.packages()) == FALSE){
  install.packages("ggplot2")
}
if("grid" %in% rownames(installed.packages()) == FALSE){
  install.packages("grid")
}

# Libraries
library("png")  # PNG library
library("ggplot2")  # ggplot2 library
library("grid")


# Graph dimensions
graph_dim <- 1000

# Graph dimensions
graph_dim <- 1000

# Picture dimensions - from file
img_sizeX <- 1920
img_sizeY <- 1200

# Load image
img <- readPNG(reference_file)  # load runner

# Prepare image
g <- rasterGrob(img, interpolate=TRUE)

# Plot
q <- qplot(1:graph_dim, 1:graph_dim*(img_sizeY/img_sizeX), geom="blank", xlab = "x", ylab = "y") +
  
  # Add image
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  
  # Add scale bar
  geom_segment(aes(x = scale_xstart, y = scale_y, 
                   xend = scale_xend, yend = scale_y, colour = "red"),
               show.legend = FALSE, lwd = 1) +
  
  # Add fixed coordinates
  coord_fixed() 

plot(q)
Sys.sleep(pausetime)





