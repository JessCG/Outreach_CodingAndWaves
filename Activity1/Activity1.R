### Parameters you can play with ###

# Change the position of the dot
x_position <- 2
y_position <- 4

# Enter the name of the image here:
fname = "MarieCurie.png"

############### Do not modify anything below this line ###############

# Test if libraries were installed
if("png" %in% rownames(installed.packages()) == FALSE){
  install.packages("png")
}
if("plotrix" %in% rownames(installed.packages()) == FALSE){
  install.packages("plotrix")
}

# Call libraries
library("png")  # PNG library
library("plotrix")  # Draw a circle

# Load and plot each image and allow for classification
img <- readPNG(fname)  # load image

# Set up a square plot area
par(pty="s")
plot(1:10, type = 'n', main = "Move the dot on the person's nose", 
     xlab = "x", ylab = "y", xlim = c(0, 10), ylim = c(0, 10), 
     axes = FALSE, asp = 1)

# Get the plot information so the image will fill the plot box
lim <- par()

# Draw the image
rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])

# Fix axes
axis(1, at = seq(0, 10))
axis(2, at = seq(0, 10))

# Add guiding lines
abline(h = seq(0, 10), v = seq(0, 10), col = 'white', lty = 2)

# Add a dot in the right position
draw.circle(x_position, y_position, 0.2, col = "red", lty = 1, lwd = 1)

# Save your work
dev.copy(png, paste(strsplit(fname, ".png")[[1]], '_with_a_dot.png', sep=""), 
         width = 700, height = 700)
dev.off()
