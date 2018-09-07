### Parameters you can play with ###

# Load the positions of our runner
runner_positions = read.csv("Runner_positions.csv")

# Height of the road
road_height <- 8

# Enter the name of the image here:
fname = "runner.png"

############### Do not modify anything below this line ###############
library("png")  # PNG library

# Load image of runner
img <- readPNG(fname)  # load image

# Now, we will make 10 new plots, with the runner moving one step 
# every time.
for (myframe in 1:10) {
  # Set up a square plot area
  par(pty="s")
  plot(1:10, type = 'n', main = paste("Make him run! (frame ", myframe, ")", sep = ""),  
       xlab = "x", ylab = "y", xlim = c(0, 10), ylim = c(0, 10), 
       axes = FALSE, asp = 1)
  
  # Fix axes
  axis(1, at = seq(0, 10))
  axis(2, at = seq(0, 10))
  
  # Add guiding lines
  abline(h = seq(0, 10), v = seq(0, 10), col = "grey", lty = 2)
  
  # Find the position of the runner
  x_position = runner_positions$x[myframe]
  y_position = runner_positions$y[myframe]
  
  # Plot the runner
  rasterImage(img, x_position, y_position, 
              x_position+1, y_position+1)
  
  # Add the road for our runner
  abline(h = road_height, col ="black", lwd = 5)
  
  # Pause before new plot
  Sys.sleep(2)  
  
  # Save the plot (we call it a frame in the movie)
  dev.copy(png, paste('frame_', myframe, '.png', sep=""), 
           width = 500, height = 500)
  dev.off()
  
  # Print what happened on this loop
  #print(paste("myframe:", myframe))
  #print(paste("x_position:", x_position))
  #print(paste("y_position:", y_position))
  #print("...")
  
}





