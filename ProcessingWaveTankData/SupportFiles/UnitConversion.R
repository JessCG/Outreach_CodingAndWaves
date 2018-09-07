UnitConversion <- function(referencefile, pausetime){
  # Input files needed
  # ./ReferenceImages/referencefile.png
  
  # Output file
  # referencefile_conversion_factor.csv"
  
  # Reference file
  reference_file <- paste("./ReferenceImages/", referencefile, sep = "")
  
  # Scale bar (red) - starting point
  scale_xstart <- 100
  scale_xend <- 300
  scale_y <- 100
  
  # List output files
  out_files <- dir(path = "./OutputFiles/")
  
  # Reference label
  ref_lab <- unlist(strsplit(referencefile, '(.png)'))
  
  # Expected characteristics file naming
  conversion_file <- paste(ref_lab, "_conversion_factor.csv", sep = "")
  
  if (any(out_files == conversion_file)){ 
    # if speed file exists, ask if they want to proceed
    msg <- paste("It looks like you already calculated the conversion",
                 "factor for this reference image. Are you sure you want to continue?")
    res <- dlgMessage(msg,  type = c("yesno"))$res
    
    if (res == "no"){
      return()
    }  # end of if selected don't want to continue
    
  }  # end of checking for this activity having been done
  

  # Graph dimensions
  graph_dim <- 1000
  
  # Graph dimensions
  graph_dim <- 1000
  
  # Picture dimensions - from file
  img_sizeX <- 1920
  img_sizeY <- 1200
  
  # Load image
  img <- readPNG(reference_file)  # load runner
  
  # Prepare image
  g <- rasterGrob(img, interpolate=TRUE)
  
  conf1 = 0
  # dev.off()  # remove image from plot
  
  while (conf1 == 0) {
    
    # Create list of coordinates to input
    scale_list <- list(
      "x_start:NUM" = scale_xstart,
      "x_end:NUM" = scale_xend,
      "y:NUM" = scale_y
    )
    
    
    # Plot
    q <- qplot(1:graph_dim, 1:graph_dim*(img_sizeY/img_sizeX), geom="blank", xlab = "x", ylab = "y") +
      
      # Add image
      annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
      
      # Add scale bar
      geom_segment(aes(x = scale_xstart, y = scale_y, 
                       xend = scale_xend, yend = scale_y, colour = "red"),
                   show.legend = FALSE, lwd = 1) +
      
      # Add fixed coordinates
      coord_fixed() 
    
    plot(q)
    Sys.sleep(pausetime)  # pause to plot
    
    # Ask if satisfied with the selection
    msg <- paste("Does the scale bar just cover your rerence line?", sep ="")
    res <- dlgMessage(msg,  type = c("yesnocancel"))$res
    
    if (res == "yes"){
      # Size of reference
      ref_size <- list(
        "Size of reference (cm):NUM" = 28
      )
      
      size_cm_temp <- dlgForm(ref_size, "Enter the actual size of the reference line on the picture (in cm).")$res
      size_cm <- as.numeric(size_cm_temp)
      
      convf_cm <- size_cm/(scale_xend - scale_xstart)
      convf_m <- (size_cm/100)/(scale_xend - scale_xstart)
      
      convf = cbind.data.frame(convf_m, convf_cm)
      
      # save conversion factor
      write.csv(convf, file = paste("./OutputFiles/", ref_lab, "_conversion_factor.csv", sep = ""), 
                row.names = FALSE)
      
      conf1 = 1  # break out of task 1 loop, return to task menu
      dev.off()  # remove image from plot
      Sys.sleep(2)  # pause before new image
      
    } else if (res == "no") {
      # Ask for new coordinates
      coord <- dlgForm(scale_list, "Adjust each coordinate, then press enter.")$res
      
      # Update coordinates
      scale_xstart <- coord[[1]]
      scale_xend <- coord[[2]]
      scale_y <- coord[[3]]
      
      conf1 = 0  # keep conf1 = 0 for while loop
      dev.off()  # remove image from plot
      Sys.sleep(2)  # pause before new image
      
    } else if (res == "cancel"){
      break  # terminate loop for task 1, return to task menu
      
    }  # end of confirming rulers
    
  }  # end of while conf1
    
    
  }  # end of function
  