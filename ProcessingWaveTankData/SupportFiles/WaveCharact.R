WaveCharact <- function(framedir, pausetime){
  # Output files: 
  # wave#_rulers.csv
  # wave#_characteristics_gu.csv
  
  # List wave files available, using those with frame 1
  wave_files <- dir(path = framedir, pattern = '*_frame1.png')
  
  # keep only wave label
  wave_choice <- unlist(strsplit(wave_files, '(_.*)'))
  
  # Ask what wave we should look at
  conf = 0
  while (conf == 0){  # will keep going until confirmed
    # Drop down menu
    wave <- dlgList(wave_choice, multiple = FALSE,
                    title = "What wave would you like to analyze?")$res
    
    # Ask for confirmation
    msg <- paste("You selected ", wave, ". Is that ok?", sep ="")
    if (okCancelBox(msg)) conf = 1  else conf = 0 
  }  # end while conf
  
  # Find the relevant wave file
  Iwave = which(wave_choice == wave)
  
  # List output files
  out_files <- dir(path = "./OutputFiles/")
  
  # Check if task 1 has been completed -- check only ruler output
  ruler_file <- paste(wave, "_rulers.csv", sep = "")
  
  if (any(out_files == ruler_file)){ 
    # if speed file exists, ask if they want to proceed
    msg <- paste("It looks like you already identified wave",
                 "characteristics. Are you sure you want to continue?")
    res <- dlgMessage(msg,  type = c("yesno"))$res
    
    if (res == "no"){
      return()
    }  # end of if selected don't want to continue
    
  }  # end of checking for this activity having been done
  
  # Load frame 1
  img <- readPNG(paste(framedir, wave_files[Iwave], sep = ""))  # load frame 1
  
  # Prepare image
  g <- rasterGrob(img, interpolate=TRUE)
  
  # Graph dimensions
  graph_dim <- 1000
  
  # Picture dimensions - from file
  img_sizeX <- 1920
  img_sizeY <- 1200
  
  # Expected ruler file naming
  ruler_file <- paste(wave, "_rulers.csv", sep = "") 
  
  # Check if there are ruler information
  out_files <- dir(path = "./OutputFiles/")
  
  if (any(out_files == ruler_file)){ 
    # if ruler file exists, load information
    rulers <- read.csv(file = paste("./OutputFiles/", ruler_file, sep = ""), 
                       header = TRUE)
  } else {  
    # if ruler file does not exists, create one
    
    # Assign coordinates to rulers
    # Bottom (yellow)
    bottom_y <- 50
    
    # Water level (blue)
    water_y <- 200
    
    # Crests (white)
    crests_y <- 400
    
    # Crests (red)
    crest1_x <- 50
    crest2_x <- 500
    
    # Put in data frame
    rulers <- cbind.data.frame(bottom_y, water_y, crests_y, crest1_x, crest2_x)
    
    write.csv(rulers, file = paste("./OutputFiles/", wave, "_rulers.csv", sep = ""), 
              row.names = FALSE)
    
  }  # end of checking for rulers
  
  conf1 = 0
  # dev.off()  # remove image from plot
  
  while (conf1 == 0) {
    
    # Create list of coordinates to input
    ruler_list <- list(
      "y-coordinate for bottom (yellow):NUM" = rulers$bottom_y,
      "y-coordinate for water level (blue):NUM" = rulers$water_y,
      "y-coordinate for crest top (white):NUM" = rulers$crests_y,
      "x-coordinate for first crest (red):NUM" = rulers$crest1_x,
      "x-coordinate for second crest (red):NUM" = rulers$crest2_x
    )
    
    # Plot with rulers
    q <- qplot(1:graph_dim, 1:graph_dim*(img_sizeY/img_sizeX), geom="blank", xlab = "x", ylab = "y") +
      
      # Add image
      annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
      
      # Add rulers
      # Bottom line
      geom_hline(yintercept = rulers$bottom_y, colour = "yellow", lwd = 1, lty = 'dotted') +
      
      # Water level line
      geom_hline(yintercept = rulers$water_y, colour = "cyan", lwd = 1, lty = 'dotted') +
      
      # Top of crests
      geom_hline(yintercept = rulers$crests_y, colour = "white", lwd = 1, lty = 'dotted') +
      
      # Crest 1
      geom_vline(xintercept = rulers$crest1_x, colour = "red", lwd = 1, lty = 'dotted') +
      
      # Crest 2
      geom_vline(xintercept = rulers$crest2_x, colour = "red", lwd = 1, lty = 'dotted') +
      
      # Fixed coordinates
      coord_fixed()
    
    plot(q)
    Sys.sleep(pausetime)  # pause to plot
    
    # Ask if satisfied with the selection
    msg <- paste("Do the rulers look good to you?", sep ="")
    res <- dlgMessage(msg,  type = c("yesnocancel"))$res
    
    if (res == "yes"){
      # save new rulers
      write.csv(rulers, file = paste("./OutputFiles/", wave, "_rulers.csv", sep = ""), 
                row.names = FALSE)
      
      # calculate wave characteristics in graphing units
      wavelength.gu <- rulers$crest2_x - rulers$crest1_x
      water_height.gu <- rulers$water_y - rulers$bottom_y
      amplitude.gu <- rulers$crests_y - rulers$water_y
      bottom.gu <- rulers$bottom_y
      crest_x.gu <- rulers$crest1_x
      
      characteristics.gu = cbind.data.frame(wavelength.gu, water_height.gu, 
                                            amplitude.gu, bottom.gu, crest_x.gu)
      
      # save file
      write.csv(characteristics.gu, file = paste("./OutputFiles/", wave, "_characteristics_gu.csv", 
                                                 sep = ""), 
                row.names = FALSE)
      
      conf1 = 1  # break out of task 1 loop, return to task menu
      dev.off()  # remove image from plot
      Sys.sleep(2)  # pause before new image
      
    } else if (res == "no") {
      # Ask for new coordinates
      coord <- dlgForm(ruler_list, "Adjust each coordinate, then press enter.")$res
      
      # Update coordinates
      rulers$bottom_y <- coord[[1]]
      rulers$water_y <- coord[[2]]
      rulers$crests_y <- coord[[3]]
      rulers$crest1_x <- coord[[4]]
      rulers$crest2_x <- coord[[5]]
      
      conf1 = 0  # keep conf1 = 0 for while loop
      dev.off()  # remove image from plot
      Sys.sleep(2)  # pause before new image
      
    } else if (res == "cancel"){
      break  # terminate loop for task 1, return to task menu
      
    }  # end of confirming rulers
    
  }  # end of while conf1
  
}

