## This script is used to process wave tank data
# Written by JCG - December 2017

# Reference image
referencefile <- "reference_waves1and2.png"

# Directory with wave frames
framedir <- "./WaveFrames/"

# Pause time for graph to be plotted
# Slower computers may require a higher pause time
pausetime <- 5


### Do not modify anything below this line ###

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
if("svDialogs" %in% rownames(installed.packages()) == FALSE){
  install.packages("svDialogs")
}


# Load libraries
library("png")  # PNG library
library("ggplot2")  # ggplot2 library
library("grid")
library("svDialogs")  # dialog box

# Source functions
source('./SupportFiles/UnitConversion.R')
source('./SupportFiles/WaveCharact.R')
source('./SupportFiles/FitToFrame1.R')
source('./SupportFiles/WaveSpeed.R')
source('./SupportFiles/TrackCrest.R')

## Add test of files being already present...

# Task choice
all_tasks <- c('Identify wave characteristics',
               'Fit theoretical wave to frame 1',
               'Track wave crest', 'Convert units', 
               'Calculate wave speed', 'Exit code')

# Ask what analysis needs to be performed
conf0 = 0

## Main while loop of code - keeps going until Exit is selected
while (conf0 == 0){
  conf = 0
  
  # while loop to select task
  while (conf == 0){  # will keep going until confirmed
    # Drop down menu
    task <- dlgList(all_tasks, multiple = FALSE,
                    title = "What analysis would you like to perform?")$res
    
    # Ask for confirmation
    msg <- paste("You selected - ", task, ". Is that ok?", sep ="")
    if (okCancelBox(msg)) conf = 1  else conf = 0 
  }

  
  ## If exit is selected - break out of code
  if (task == all_tasks[6]){
    break
  }
  
  
  ## If task 1 (code from Activity 7)
  if (task == all_tasks[1]){  
    
    # Code for task 1 can be found in ./SupportFile/WaveCharact.R function
    WaveCharact(framedir, pausetime)
    
  }  # end of if task 1

  
  ## If task 2 (code from Activity 9)
  if (task == all_tasks[2]){
    
    # Code for task 2 can be found in ./SupportFile/FitToFrame1.R function
    FitToFrame1(framedir, pausetime)
    
  }  # end of if task 2 

  
  ## If task 3 (code from Activity 9 - track crest)
  if (task == all_tasks[3]){
    
    # Code for task 3 can be found in ./SupportFile/TrackCrest.R function
    TrackCrest(framedir, pausetime)
    
  }  # end of if task 3
  
  ## If task 4 (code from UnitConversion)
  if (task == all_tasks[4]){
    
    # Code for task 4 can be found in ./SupportFile/UnitConversion.R function
    UnitConversion(referencefile, pausetime)
    
  }  # end of if task 4
  
  
  ## If task 5 (code from Activity 9/10 - calculate speed)
  if (task == all_tasks[5]){

    # Code for task 5 can be found in ./SupportFile/WaveSpeed.R function
    WaveSpeed(framedir, referencefile, pausetime)

  }  # end of if task 5
  

}  # end of main loop conf0
