### Parameters you can play with ###

# Start position of our runner
x_position_start <- 5
y_position_start <- 6

# Height of the road
road_height <- 8

# Enter the name of the image here:
fname = "runner.png"

############### Do not modify anything below this line ###############
# Check for library installations
if("png" %in% rownames(installed.packages()) == FALSE){
  install.packages("png")
}

# Load needed libraries
library("png")  # PNG library

# Load and plot each image and allow for classification
img <- readPNG(fname)  # load image

# Set up a square plot area
par(pty="s")
plot(1:10, type = 'n', main = "Make him run!", 
     xlab = "x", ylab = "y", xlim = c(0, 10), ylim = c(0, 10), 
     axes = FALSE, asp = 1)

# Fix axes
axis(1, at = seq(0, 10))
axis(2, at = seq(0, 10))

# Add guiding lines
abline(h = seq(0, 10), v = seq(0, 10), col = "grey", lty = 2)

# For this exercise, the x- and y- position of the runner is always
# the starting position, it will change in the movie
x_position <- x_position_start
y_position <- y_position_start

# Plot the runner
rasterImage(img, x_position, y_position,  # position of left corner
            x_position+1, y_position+1)  # size of runner

# Add the road for our runner
abline(h = road_height, col ="black", lwd = 5)




