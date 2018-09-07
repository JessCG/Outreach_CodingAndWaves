TrackCrest <- function(framedir, pausetime){
  # Output files: 
  # wave#_phase_shift.csv
  
  
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
  
  # Graph dimensions
  graph_dim <- 1000
  
  # Picture dimensions - from file
  img_sizeX <- 1920
  img_sizeY <- 1200
  
  # Expected characteristics file naming
  characteristics_file <- paste(wave, "_characteristics_gu.csv", sep = "") 
  
  # List output files
  out_files <- dir(path = "./OutputFiles/")
  
  ## Check if task 2 has been completed
  if (any(out_files == characteristics_file)){ 
    # if characteristics file exists, load information
    characteristics.gu <- read.csv(file = paste("./OutputFiles/", 
                                                characteristics_file, sep = ""), 
                                   header = TRUE)
  } else {  
    # if characteristics don't exist, ask to complete task 2
    msg <- "There are no wave characteristics file for the wave you selected, you must first identify these."
    res <- dlgMessage(msg,  type = c("ok"))$res
    return()
    
  }  # end of checking for characteristics
  
  
  # make sure we have phase shift information, otherwise complete task 2
  if (!any(colnames(characteristics.gu) ==  "phase.shift1")){
    msg <- "A theoretical wave has not been matched to frame 1, you must first complete this."
    res <- dlgMessage(msg,  type = c("ok"))$res
    break
    
  }  # end of if for phase shift 1 (task 2)
  
  
  # Characteristics won't change this time:
  L <- characteristics.gu$wavelength.gu
  H <- characteristics.gu$water_height.gu
  A <- characteristics.gu$amplitude.gu
  p <- characteristics.gu$phase.shift1
  B <- characteristics.gu$bottom.gu
  x_crest <- characteristics.gu$crest_x.gu
  
  k <- 2*pi/L
  
  # Loop through frames
  for (iframe in 1:5){
    conf3 = 0
    while (conf3 == 0) {
      
      # Create list of characteristics to input
      phase_list <- list(
        "x-position of crest:NUM" = x_crest
      )
      
      # Horizontal grid
      x <- seq(0, graph_dim, by = 1)
      
      # Calculate max value of equation
      y_max <- max(A*sin(k*x) + D)
      
      # Calculate phase shift needed to align crests
      p <- asin((y_max - D) / A) - k*x_crest
      
      # Calculate wave positions for each point in x
      y_wave <- A*sin(k*x + p) + H
      
      # Make data frame
      wave_data <- data.frame(x, y_wave) 
      
      # Find crest of waves 
      Icrest = which(y_wave == max(y_wave))
      
      # Load appropriate frame
      img <- readPNG(paste(framedir, wave, "_frame", iframe, ".png", sep = ""))
      
      # Prepare image
      g <- rasterGrob(img, interpolate=TRUE)
      
      # Plot with guides
      q <- qplot(1:graph_dim, 1:graph_dim*(img_sizeY/img_sizeX), 
                 geom="blank", xlab = "x", ylab = "y") +
        
        # Add title to know which frame
        labs(title = paste("Frame", iframe), subtitle = NULL) +
        
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
        
        # Add crest lines
        geom_vline(xintercept = x[Icrest], colour = "cyan", lwd = 1, lty = 'dotted') +
        
        # Add crest labels
        annotate("text", x = x[Icrest]+30, y = H/2, label = x[Icrest],
                 color = "cyan") +
        
        # Add fixed coordinates
        coord_fixed() 
      
      plot(q)
      Sys.sleep(pausetime)  # pause to plot
      
      # Ask if satisfied with the selection
      msg <- paste("Do the crest labels look good to you?", sep ="")
      res <- dlgMessage(msg,  type = c("yesnocancel"))$res
      
      if (res == "yes"){
        # ask user to input value of crest
        crest_list <- list(
          "x:NUM" = x[Icrest[1]]
        )
        x_user <- dlgForm(crest_list, "What is the x value of the crest you are following?")$res
        
        if (iframe == 1){
          phase.shift1 <- p
          crest_x1 <- x_user
        } # end iframe == 1
        
        if (iframe == 2){
          phase.shift2 <- p
          crest_x2 <- x_user
        } # end iframe == 2
        
        if (iframe == 3){
          phase.shift3 <- p
          crest_x3 <- x_user
        } # end iframe == 2
        
        if (iframe == 4){
          phase.shift4 <- p
          crest_x4 <- x_user
        } # end iframe == 2
        
        if (iframe == 5){
          phase.shift5 <- p
          crest_x5 <- x_user
          
          # save shift data
          shift = cbind.data.frame(phase.shift1, phase.shift2, phase.shift3,
                                   phase.shift4, phase.shift5, crest_x1, 
                                   crest_x2, crest_x3, crest_x4, crest_x5)
          
          colnames(shift)[6:10] <- c("crest_x1", "crest_x2", "crest_x3",
                                            "crest_x4", "crest_x5")
          
          write.csv(shift, file = paste("./OutputFiles/", wave, "_phase_shift.csv", 
                                        sep = ""), 
                    row.names = FALSE)
        }  # end iframe == 5
        
        conf3 = 1  # break out of task 3 loop, return to task menu
        
      } else if (res == "no") {
        # Ask for new characteristics
        phase <- dlgForm(phase_list, "Adjust the x-position of the crest, then press enter.")$res
        
        # Update coordinates
        x_crest <- phase[[1]]
        
        conf3 = 0  # keep conf3 = 0 for while loop
        dev.off()  # remove image from plot
        Sys.sleep(2)  # pause before new image
        
      } else if (res == "cancel"){
        return()  # terminate task 3 function
        
      }  # end of confirming rulers
      
    }  # end of while conf3
    
  }  # end for iframe
  
}  # end function