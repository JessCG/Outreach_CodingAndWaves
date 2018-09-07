### Change the lines below to move the rulers on your image ###
## This script is used to make wave measurements

# Wave file to measure
wave_file <- "wave2_frame1.png"

# Bottom (yellow)
bottom_y <- 50;

# Water level (blue)
water_y <- 200

# Crests (white)
crests_y <- 400

# Crests (red)
crest1_x <- 50
crest2_x <- 500

# Time needed for plot to show 
# Slower computers might need to increase the time
pausetime <- 2

### Do not change anything below this line ###
# You can comment and uncomment geom_hline() or geom_vline()

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

# Picture dimensions - from file
img_sizeX <- 1920
img_sizeY <- 1200

# Load image
img <- readPNG(wave_file)  # load runner

# Prepare image
g <- rasterGrob(img, interpolate=TRUE)

# Plot
q <- qplot(1:graph_dim, 1:graph_dim*(img_sizeY/img_sizeX), geom="blank", xlab = "x", ylab = "y") +
  
  # Add image
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  
  # Add rulers
  # Bottom line
  geom_hline(yintercept = bottom_y, colour = "yellow", lwd = 1, lty = 'dotted') +
  
  # Water level line
  geom_hline(yintercept = water_y, colour = "cyan", lwd = 1, lty = 'dotted') +
  
  # Top of crests
  geom_hline(yintercept = crests_y, colour = "white", lwd = 1, lty = 'dotted') +
  
  # Crest 1
  geom_vline(xintercept = crest1_x, colour = "red", lwd = 1, lty = 'dotted') +
  
  # Crest 2
  geom_vline(xintercept = crest2_x, colour = "red", lwd = 1, lty = 'dotted') +
  
  # Fixed coordinates
  coord_fixed()

plot(q)
Sys.sleep(pausetime)







