## This script is used by the Spy to play the maze game
# Written by JCG - August 2017
# Inspired from Keep Talking and Nobody Explodes
# http://www.keeptalkinggame.com/

# Check library installation
if("png" %in% rownames(installed.packages()) == FALSE){
  install.packages("png")
}
if("svDialogs" %in% rownames(installed.packages()) == FALSE){
  install.packages("svDialogs")
}

# Load libraries
library("png")  # PNG library
library("svDialogs")  # dialog box
options(warn=-1)  # Warning off

# Load runner image
fname_runner = "./SupportFiles/runner.png"
img_runner <- readPNG(fname_runner)  # load runner

# Load skull image
fname_skull = "./SupportFiles/skull.png"
img_skull <- readPNG(fname_skull)  # load skull

# Load fireworks image
fname_fireworks = "./SupportFiles/fireworks.png"
img_fireworks <- readPNG(fname_fireworks)  # load skull

# Maze choice
# all_maze <- c(1:10)

# List wave files available, using those with frame 1
maze_files <- dir(path = "./SupportFiles", pattern = "Walls_*")

# Remove Walls_ part of string
maze_files <- sub(".*_", "", maze_files)

# Keep only maze number
all_maze <- unlist(strsplit(maze_files, ".csv"))
all_maze <- sort(as.numeric(all_maze))

# Ask what maze the student will be solving (drop down)
conf = 0
while (conf == 0){  # will keep going until confirmed
  # Drop down menu
  maze <- dlgList(all_maze, multiple = FALSE,
                 title = "What maze will you be solving?")$res
  
  # Ask for confirmation
  msg <- paste("You selected maze #", maze, ". Is that ok?", sep ="")
  if (okCancelBox(msg)) conf = 1  else conf = 0 
}

# Load wall information and starting point
walls = read.csv(paste("./SupportFiles/Walls_", maze, ".csv", sep = ""))
positions = read.csv(paste("./SupportFiles/Positions_", maze, ".csv", sep = ""))
preferences = read.csv(paste("./SupportFiles/Preferences_", maze, ".csv", sep = ""))

msize = as.numeric(preferences$size)

# Set up a square plot area
par(pty="s")
plot(1:msize, type = 'n', main = "Get help from the architect to reach the end of the maze!", 
     xlab = "x", ylab = "y", xlim = c(0, msize), ylim = c(0, msize), 
     axes = FALSE, asp = 1)

# Fix axes
axis(1, at = seq(0, msize))
axis(2, at = seq(0, msize))

# Add walls - maybe advanced activity is to comment out and remove black square
# segments(walls$x1, walls$y1, walls$x2, walls$y2)

# Add a black square (the room is dark)
rect(0, 0, msize, msize,
     col = "black", border=par("fg"), lty=NULL, lwd=par("lwd"), xpd=FALSE)

# Plot the runner at the start
rasterImage(img_runner, positions$x_start+0.1, positions$y_start+0.1, 
            positions$x_start+0.8, positions$y_start+0.8)

# Pause to see the plot
Sys.sleep(2)

if (maze < 100){
  message1 <- paste("Tell the architect at what x and y coordinates you are starting.", 
                    "This way, s/he can find the right plans.")
}else{
  message1 <- paste("Tell the architect the maze number you selected.",
                    "This way, s/he can find the designer name and plans.")
}
msgBox(message1)

x_or_y <- c('x-direction', 'y-direction', 'end the game')

# Consider mid-points to ignore side walls
displacement <- -msize:msize
x_position1 <- positions$x_start + 0.5
y_position1 <- positions$y_start + 0.5

# Pause to give time to students to find where they are
Sys.sleep(2)

#### Main loop for game starts here
game <- 1
while (game == 1){  # no while loop while troubleshooting
# for (game in 1:50){  # used to troubleshoot
  # Ask in what direction to move
  conf = 0
  while (conf == 0){  # will keep going until confirmed
    # Drop down menu for direction
    res <- dlgList(x_or_y, multiple = FALSE,
                   title = "In what direction will you move?")$res
    if (res == 'end the game'){stop("You ended the game.")}
    
    # Drop down menu for how many spaces
    msg <- paste("How many spaces in the ", res, 
                 "? Make sure to indicate positive or negative.", sep = "")
    nspace <- dlgList(displacement, multiple = FALSE,
                      title = msg)$res
    
    # Ask for confirmation
    msg <- paste("You will move ", nspace, " space in the ", res, ". Is that ok?", sep ="")
    if (okCancelBox(msg)) conf = 1  else conf = 0 
  }
  
  # change runner's position
  if (res == "x-direction"){
    x_position2 <- x_position1 + as.numeric(nspace)
    y_position2 <- y_position1
  }
  if (res == "y-direction"){
    y_position2 <- y_position1 + as.numeric(nspace)
    x_position2 <- x_position1
  } 
  
  ## Test if runner will hit a wall
  cross = 0
  if (res == "x-direction"){
    path_y <- y_position1 
    
    # only integers considered
    if (nspace > 0) {path_x = (ceiling(x_position1) : floor(x_position2))}
    if (nspace < 0) {path_x = (floor(x_position1) : ceiling(x_position2))}
    if (nspace == 0) {next}
    
    for (ipath in 1:length(path_x)){
      for (iwall in 1:length(walls$x1)){
        if (path_x[ipath] <= max(walls$x1[iwall], walls$x2[iwall]) && 
            path_x[ipath] >= min(walls$x1[iwall], walls$x2[iwall]) &&
            path_y <= max(walls$y1[iwall], walls$y2[iwall]) &&
            path_y >= min(walls$y1[iwall], walls$y2[iwall])) cross = 1
      }
    }
  }
  
  if (res == "y-direction"){
    path_x <- x_position1
    
    # only integers considered
    if (nspace > 0) {path_y = (ceiling(y_position1) : floor(y_position2))}
    if (nspace < 0) {path_y = (floor(y_position1) : ceiling(y_position2))}
    if (nspace == 0) {next}
    
    for (ipath in 1:length(path_y)){
      for (iwall in 1:length(walls$x1)){
        if (path_x <= max(walls$x1[iwall], walls$x2[iwall]) && 
            path_x >= min(walls$x1[iwall], walls$x2[iwall]) &&
            path_y[ipath] <= max(walls$y1[iwall], walls$y2[iwall]) &&
            path_y[ipath] >= min(walls$y1[iwall], walls$y2[iwall])) cross = 1
      }
    }
  }
  
  # If runner crossed a wall: die
  if (cross == 1){
    rasterImage(img_skull, 0, 0, msize, msize)  # skull image
    print("You crossed a wall and died.")
    game <- 0
    next
    # stop("You crossed a wall and died.")  # replace this with end of while loop
  }
  
  if (x_position2 < 0 | y_position2 < 0){
    rasterImage(img_skull, 0, 0, msize, msize)  # skull image
    print("You went way beyond the doors and died.")
    game <- 0
    next
  }
  
  # Add a black square (the room is dark)
  rect(0, 0, msize, msize,
       col = "black", border=par("fg"), lty=NULL, lwd=par("lwd"), xpd=FALSE)
  
  # Plot the runner at the new position
  rasterImage(img_runner, x_position2-0.5+0.1, y_position2-0.5+0.1, 
              x_position2-0.5+0.8, y_position2-0.5+0.8)
  
  # If runner gets to the end: success!
  if (x_position2-0.5 == positions$x_end && y_position2-0.5 == positions$y_end){
    Sys.sleep(2)
    
    rasterImage(img_fireworks, 0, 0, msize, msize)  # fireworks image
    print("You won!")
    game <- 0
    next
    # stop("You won!")  # replace this with end of while loop
  }
  
  # End position becomes start position
  x_position1 <- x_position2
  y_position1 <- y_position2
  
  # Pause to have time to see the graph
  Sys.sleep(2)
}
