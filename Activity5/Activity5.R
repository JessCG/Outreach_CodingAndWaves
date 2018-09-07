### Fill in this code to reproduce the maze provided to your team

# Load wall information (include if statements)
# walls_magenta = read.csv("")  # magenta walls
# walls_black = read.csv("")  # black walls
# walls_gold = read.csv("")  # gold walls
# walls_blue = read.csv("")  # blue walls

# Load start and end positions
# positions = read.csv("")  # start and end positions

# Check for library installation
if("png" %in% rownames(installed.packages()) == FALSE){
  install.packages("png")
}

# Load needed library
library("png")  # PNG library

# Warning off
options(warn=-1)

# Image files
# fname_runner = ""  # runner file
# fname_finish = ""  # flag file

# Load images
# img_runner <- readPNG(fname_runner)  # load runner
# img_finish <- readPNG(fname_finish)  # load finish

# Set up a square plot area
par(pty="s")
plot(0:10, type = 'n', main = "Can you reproduce this maze?", 
     xlab = "x", ylab = "y", xlim = c(0, 10), ylim = c(0, 10), 
     axes = FALSE, asp = 1)

# Fix axes
axis(1, at = seq(0, 10))
axis(2, at = seq(0, 10))

# Add guiding lines
abline(h = seq(0, 10), v = seq(0, 10), col = "lightgrey", lty = 2)

# Add walls with correct color
# segments(walls_color$x1, walls_color$y1, walls_color$x2, walls_color$y2,
#          col = "color", lwd = 4)  # color walls

# Plot the runner at the start
# rasterImage(img_runner, positions$x_start+0.1, positions$y_start+0.1, 
#             positions$x_start+0.8, positions$y_start+0.8)

# Plot the flags at the end
# rasterImage(img_finish, positions$x_end+0.05, positions$y_end+0.05, 
#             positions$x_end+0.9, positions$y_end+0.9)

# Save the maze
dev.copy(png, "Maze_by_Team_NoName.png", width = 500, height = 500)
dev.off()


