library(readr)
library(dplyr)
library(purrr)
library(plotrix)
library(lsr)

#The script is adopted from nback and DCCS NIH Cognition Battery Tool

#access all the nback files from a folder (change directory to the one on your computer)
#set up root dir for data to be processed
data_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'

#set up output dir and file name
out_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'
proc_fileName <- paste("nback_points_", Sys.Date(), ".csv", sep="", collapse=NULL)

#pull out all the subfolders (participant folders) for the root data dir
sub_folders <- list.files(data_path, pattern = "sub")

#create a spaceholder for the results 
pointsnback <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("id",
                                                                    "accScore",
                                                                    "accPercent",
                                                                    "rtMed",
                                                                    "rtScore",
                                                                    "totScore"))

#create a loop for processing each file in the folder (export and open each file)
#loop over participant (subfolders)
for(i in 1:length(sub_folders)) {
  #for this participant, find the nback file name
  nback_match <- list.files(paste(data_path,sub_folders[i], sep = "", collapse = NULL), pattern = "nback")
  csv_match <- list.files(paste(data_path,sub_folders[i], sep = "", collapse = NULL), pattern = "\\.csv")
  nback_fileName <- print(intersect(nback_match,csv_match))
  
  #logical to make sure there is a dcccs file for this participant before loading, else, skip to next participant
  if (!identical(nback_fileName, character(0))) {
    print("Woohoo! Processing file!")
    
    #read in the data for this participant
    nbackDat <- read.csv(file = paste(data_path,sub_folders[i],'/',nback_fileName, sep = "", collapse = NULL), stringsAsFactors = TRUE)
    
    # SET UP ONLY THE NECESSARY COLUMNS, REMOVE NAs AND CUT OUT THE FIRST 2 AND THE LAST 4 ROWS
    selectedCols <- c("ISI","tarOrNot", "key_resp.keys", "key_resp.corr", "key_resp.rt", "stimLoop2.ran", 
                      "corrAns", "participant", "session", 
                      "date", "expName", "psychopyVersion", "OS", "frameRate", "id")
    
    nbackTrim <- nbackDat[selectedCols]
    testDat <- nbackTrim[nbackTrim$stimLoop2.ran %in% c("1"),]
    trial_length <- nrow(testDat)
    nbackTrimTrial <- testDat[3:trial_length,]
    
    #First make sure that the accuracy for 1n is above 80%, if it is then proceed to the next step
    #if not check what is the accuracy for 2n, if it's above 80% then count it in
    
    
    #Find the accuracy score 5/29 = 0.172
    accPoint <- sum(nbackTrimTrial$key_resp.corr, na.rm = TRUE)
    accScore <- 0.172 * accPoint
    accPercent <- (accPoint/29) 
    
    #If the accuracy rate is below or equals 80% then total computed score = accuracy score.
    
    if (accPercent <= .8){
      totScore <- accScore
      rtMed <- NA
      
    } else {
      
      #CALCULATING REACTION TIME
      #find median RT only for the trials on less frequent cue 
      onlyCorr <- nbackTrimTrial[nbackTrimTrial$key_resp.corr == 1,]
      rtMean <- mean(onlyCorr$key_resp.rt)
      StDev <- sd(onlyCorr$key_resp.rt)
      
      # if mean RT is between 100ms and 3 standard deviations from the mean then use log base 10
      
      if ((rtMean >= 0.1) & (StDev < 3))  {
        
        rtMed <- median(onlyCorr$key_resp.rt) * 1000
        logRtMed <- log10(rtMed)
        rtScore <- 5 - abs(5 * abs((logRtMed - log10(500))/(log10(3000)-log10(500))))
        totScore <- accScore + rtScore
      }
    }
    
    id <- nbackTrim$id[1]
    pointsnback[nrow(pointsnback) + 1,] <-c(id,
                                                accScore,
                                                accPercent,
                                                rtMed,
                                                rtScore,
                                                totScore)
  }
  
  #if participant did not have a nback file, skip to next participant
  else {
    print("Ruh Roh... missing file")
  }
  
}

#write the extracted summary scores to disk
write.csv(pointsnback,paste(out_path,proc_fileName, sep = "", collapse = NULL), row.names=FALSE) 