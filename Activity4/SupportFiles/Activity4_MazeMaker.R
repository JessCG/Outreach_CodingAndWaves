### This code is used to create the Architect's plans

# Enter the maze you want to build
# The Excel files need to be created first
maze = 10

# Check for library installation
if("png" %in% rownames(installed.packages()) == FALSE){
  install.packages("png")
}

# Load needed library
library("png")  # PNG library

# Warning off
options(warn=-1)

# Image files
fname_runner = "runner.png"
fname_finish = "finish.png"

# Load images
img_runner <- readPNG(fname_runner)  # load runner
img_finish <- readPNG(fname_finish)  # load finish

# Load wall information (include if statements)
walls = read.csv(paste("Walls_", maze, ".csv", sep = ""))
positions = read.csv(paste("Positions_", maze, ".csv", sep = ""))

# Set up a square plot area
par(pty="s")
plot(0:6, type = 'n', main = "Help the spy reach the end of the maze!",
     cex.main = 2, cex.lab = 2,
     xlab = "x", ylab = "y", xlim = c(0, 6), ylim = c(0, 6), 
     axes = FALSE, asp = 1)


# Fix axes
axis(1, at = seq(0, 6), lwd = 2, cex.axis = 2, tck = 0.01)
axis(2, at = seq(0, 6), lwd = 2, cex.axis = 2, tck = 0.01)

# Add walls
segments(walls$x1, walls$y1, walls$x2, walls$y2, lwd =2)

# Add guiding dots
dots_x = rep(seq(0.5, 5.5, by = 1), times = 6)
dots_y = rep(seq(0.5, 5.5, by = 1), each = 6)
points(dots_x, dots_y, cex = 2)

# Plot the runner at the start
rasterImage(img_runner, positions$x_start+0.1, positions$y_start+0.1, 
            positions$x_start+0.8, positions$y_start+0.8)

# Plot the flags at the end
rasterImage(img_finish, positions$x_end+0.05, positions$y_end+0.05, 
            positions$x_end+0.9, positions$y_end+0.9)

# Save the maze
dev.copy(png, paste('Maze', maze, '.png', sep=""), 
         width = 1000, height = 1000)
dev.off()
