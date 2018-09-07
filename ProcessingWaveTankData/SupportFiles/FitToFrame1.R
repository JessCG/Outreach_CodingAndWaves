FitToFrame1 <- function(framedir, pausetime){
  # Output files: 
  # wave#_characteristics_gu.csv (refine)
  
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
  characteristics_file <- paste(wave, "_characteristics_gu.csv", sep = "") 
  
  # List output files
  out_files <- dir(path = "./OutputFiles/")
  
  # Check if there are ruler information
  if (any(out_files == characteristics_file)){ 
    # if ruler file exists, load information
    characteristics.gu <- read.csv(file = paste("./OutputFiles/", 
                                                characteristics_file, sep = ""), 
                                   header = TRUE)
  } else {  
    # if characteristics don't exist, ask to complete task 1
    msg <- "There are no wave characteristics file for the wave you selected, you must first identify these."
    res <- dlgMessage(msg,  type = c("ok"))$res
    return()
    
  }  # end of checking for characteristics
  
  characteristics.gu$phase.shift <- 0
  
  conf2 = 0
  
  while (conf2 == 0) {
    
    # Wavelength (graph units for now)
    L <- characteristics.gu$wavelength.gu
    
    # Water height
    H <- characteristics.gu$water_height.gu
    
    # Amplitude
    A <- characteristics.gu$amplitude.gu
    
    # Phase shift - only relevant if previously done
    p <- characteristics.gu$phase.shift
    
    # Bottom
    B <- characteristics.gu$bottom.gu
    
    # First crest
    x_crest <- characteristics.gu$crest_x.gu
    
    # Water level
    W = H + B
    
    # Wavenumber calculations
    k <- 2*pi/L
    
    # Horizontal grid
    x <- seq(0, graph_dim, by = 1)
    
    # Calculate max value of equation
    y_max <- max(A*sin(k*x) + H)
    
    # Calculate phase shift needed to align crests
    p <- asin((y_max - H) / A) - k*x_crest
    
    # Create list of characteristics to input
    characteristics_list <- list(
      "Wavelength (graphing units):NUM" = L,
      "Water level (graphing units):NUM" = W,
      "Amplitude (graphing units):NUM" = A,
      "x-position of crest:NUM" = x_crest
    )
    
    # Calculate wave positions for each point in x
    y_wave <- A*sin(k*x + p) + H
    
    # Make data frame
    wave_data <- data.frame(x, y_wave) 
    
    # Find crest of waves 
    Icrest = which(y_wave == max(y_wave))
    
    # Plot with rulers
    q <- # Plot
      qplot(1:graph_dim, 1:graph_dim*(img_sizeY/img_sizeX), geom="blank", xlab = "x", ylab = "y") +
      
      # Modify grids
      theme_bw() +
      theme(panel.grid.major = element_line(colour="darkgrey", size=0.5)) +
      scale_x_continuous(minor_breaks = seq(-100, 1100, 50)) +
      scale_y_continuous(minor_breaks = seq(-100, 1100, 50)) +
      
      # Add image
      annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
      
      # Add wave
      geom_point(data = wave_data, mapping =
                   aes(x = x, y = y_wave+B), alpha = 0.05,
                 colour = 'blue', fill = "blue", size = 2) +
      
      # Add fixed coordinates
      coord_fixed() 
    
    plot(q)
    Sys.sleep(pausetime)  # pause to plot
    
    # Ask if satisfied with the selection
    msg <- paste("Does the wave fit look good to you?", sep ="")
    res <- dlgMessage(msg,  type = c("yesnocancel"))$res
    
    if (res == "yes"){
      # save new characteristics
      colnames(characteristics.gu)[6] <- "phase.shift1"
      write.csv(characteristics.gu, file = paste("./OutputFiles/", wave, "_characteristics_gu.csv", 
                                                 sep = ""), 
                row.names = FALSE)
      
      conf2 = 1  # break out of task 1 loop, return to task menu
      
      
    } else if (res == "no") {
      # Ask for new characteristics
      coord <- dlgForm(characteristics_list, "Adjust each value, then press enter.")$res
      
      # Update coordinates
      characteristics.gu$wavelength.gu <- coord[[1]]
      characteristics.gu$water_height.gu <- coord[[2]] - B
      characteristics.gu$amplitude.gu <- coord[[3]]
      characteristics.gu$crest_x.gu <- coord[[4]]
      characteristics.gu$phase.shift <- p
      
      conf2 = 0  # keep conf2 = 0 for while loop
      dev.off()  # remove image from plot
      Sys.sleep(2)  # pause before new image
      
    } else if (res == "cancel"){
      break  # terminate loop for task 2, return to task menu
      
    }  # end of confirming rulers
    
  }  # end of while conf2
}  # end of function