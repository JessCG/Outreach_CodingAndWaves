WaveSpeed <- function(framedir, referencefile, pausetime){
  # Input files needed
  # referencefile_conversion_factor.csv"
  # wave#_phase_shift.csv
  # wave#_characteristics_gu.csv
  
  # Output files: 
  # wave#_speed.csv
  # wave#_characteristics_m.csv
  
  
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
  
  # Expected tracking crest file naming
  shift_file <- paste(wave, "_phase_shift.csv", sep = "") 
  
  # Reference label
  ref_lab <- unlist(strsplit(referencefile, '(.png)'))
  
  # Expected reference file naming
  reference_file <- paste(ref_lab, "_conversion_factor.csv", sep = "")
  
  # List output files
  out_files <- dir(path = "./OutputFiles/")
  
  ## Check if task 2 has been completed
  if (any(out_files == shift_file)){ 
    # if characteristics file exists, load information
    shift <- read.csv(file = paste("./OutputFiles/", 
                                                shift_file, sep = ""), 
                                   header = TRUE)
  } else {  
    # if characteristics don't exist, ask to complete task 2
    msg <- "There are no crest tracking file, you must first complete that task."
    res <- dlgMessage(msg,  type = c("ok"))$res
    return()
    
  }  # end of checking for characteristics
  
  ## Check if Unit Conversion has been completed
  if (any(out_files == reference_file)){ 
    # if reference file exists, load information
    convf <- read.csv(file = paste("./OutputFiles/", 
                                   reference_file, sep = ""), 
                                   header = TRUE)
  } else {  
    # if Unit Conversion doesn't exist, ask to complete task
    msg <- "There are no unit conversion file available, you must first complete this task."
    res <- dlgMessage(msg,  type = c("ok"))$res
    return()
    
  }  # end of checking for reference file
  
  # Check if task 3 has been completed -- check only speed output
  speed_file <- paste(wave, "_speed.csv", sep = "")
  
  if (any(out_files == speed_file)){ 
    # if speed file exists, ask if they want to proceed
    msg <- paste("It looks like you already calculated wave speed",
                 "for this wave. Are you sure you want to continue?")
    res <- dlgMessage(msg,  type = c("yesno"))$res
    
    if (res == "no"){
      return()
    }  # end of if selected don't want to continue
    
  }  # end of checking for this activity having been done
  
  
  # Ask how much time between frames (in seconds)
  time_list <- list(
    "Seconds between frames:NUM" = 1/10
  )
  
  t_s_temp <- dlgForm(time_list, "Enter the number of seconds between frames, then press enter.")$res
  t_s <- as.numeric(t_s_temp)
  
  wave_speed <- data.frame()
  
  # Calculate speed
  frame1to2.gu <- diff(as.numeric(shift[6:10]))[1] / t_s
  frame2to3.gu <- diff(as.numeric(shift[6:10]))[2] / t_s
  frame3to4.gu <- diff(as.numeric(shift[6:10]))[3] / t_s
  frame4to5.gu <- diff(as.numeric(shift[6:10]))[1] / t_s
  average.gu <- mean(diff(as.numeric(shift[6:10]))) / t_s
  
  wave_speed <- cbind.data.frame(frame1to2.gu, frame2to3.gu, frame3to4.gu,
                                 frame4to5.gu)
  
  wave_speed$average.MetersPerSecond <- average.gu * convf$convf_m
  
  # save file
  write.csv(wave_speed, file = paste("./OutputFiles/", wave, 
                                        "_speed.csv", sep = ""), row.names = FALSE)
  
  ### Calculate characteristics in m
  
  # Expected characteristics file naming
  characteristics_file <- paste(wave, "_characteristics_gu.csv", sep = "") 
  
  ## Check if task 2 has been completed
  # should not be a problem since we checked for shit, but in case
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
  
  # convert characteristics
  wavelength.m <- characteristics.gu$wavelength.gu * convf$convf_m
  water_height.m <- characteristics.gu$water_height.gu * convf$convf_m
  amplitude.m <- characteristics.gu$amplitude.gu * convf$convf_m
  
  characteristics.m <- cbind.data.frame(wavelength.m, water_height.m,
                                        amplitude.m, wave_speed$average.MetersPerSecond)
  
  colnames(characteristics.m)[4] <- c("avg.speed.MetersPerSecond")
  
  # save file
  write.csv(characteristics.m, file = paste("./OutputFiles/", wave, "_characteristics_m.csv", 
                                             sep = ""), 
            row.names = FALSE)
  
  print("All calculations were performed. Look in the OutputFiles folder!")
  
  
}  # end function