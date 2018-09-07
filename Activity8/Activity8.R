### Change the lines below to input wave parameters ###
## This script is used to investigate wave properties

# Wavelength (in graphing units)
L <- 100

# Water depth (in graphing units)
H <- 200

# Amplitude (in graphing units)
A <- 12

# Phase shift 
p <- 3

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

# Wavenumber calculations
k <- 2*pi/L

# Graph dimensions
graph_dim <- 1000

# Horizontal grid
x <- seq(0, graph_dim, by = 1)

# Calculate wave positions for each point in x
y_wave <- A*sin(k*x + p) + H

# Make data frame
wave_data <- data.frame(x, y_wave) 

# Graph dimensions
graph_dim <- 1000

# Picture dimensions - from file
img_sizeX <- 1920
img_sizeY <- 1200

# Plot
q <- qplot(1:graph_dim, 1:graph_dim*(img_sizeY/img_sizeX), geom="blank", xlab = "x", ylab = "y") +
  
  # Modify grids
  theme_bw() +
  theme(panel.grid.major = element_line(colour="darkgrey", size=0.5)) +
  scale_x_continuous(minor_breaks = seq(-100, 1100, 50)) +
  scale_y_continuous(minor_breaks = seq(-100, 1100, 50)) +
  
  # Add wave
  geom_point(data = wave_data, mapping =
               aes(x = x, y = y_wave), alpha = 0.05,
             colour = 'blue', fill = "blue", size = 2) +
  
  # Add fixed coordinates
  coord_fixed() 

plot(q)
Sys.sleep(pausetime)
  





