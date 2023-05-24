library(readr)
library(dplyr)
library(purrr)
library(plotrix)
library(lsr)

#The script is adopted from DCCS NIH Cognition Battery Tool

#access all the dccs files from a folder (change directory to the one on your computer)
#set up root dir for data to be processed
data_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'

#set up output dir and file name
out_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'
proc_fileName <- paste("dccs_points_", Sys.Date(), ".csv", sep="", collapse=NULL)

#pull out all the subfolders (participant folders) for the root data dir
sub_folders <- list.files(data_path, pattern = "sub")

#create a spaceholder for the results 
pointsDCCS <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("id",
                                                                "accScore",
                                                                "accPercent",
                                                                "rtMed",
                                                                "rtScore",
                                                                "totScore"))

#create a loop for processing each file in the folder (export and open each file)
#loop over participant (subfolders)
for(i in 1:length(sub_folders)) {
  #for this participant, find the dccs file name
  dccs_match <- list.files(paste(data_path,sub_folders[i], sep = "", collapse = NULL), pattern = "dccs")
  csv_match <- list.files(paste(data_path,sub_folders[i], sep = "", collapse = NULL), pattern = "\\.csv")
  dccs_fileName <- print(intersect(dccs_match,csv_match))
  
  #logical to make sure there is a dcccs file for this participant before loading, else, skip to next participant
  if (!identical(dccs_fileName, character(0))) {
    print("Woohoo! Processing file!")
    
    #read in the data for this participant
    dccsDat <- read.csv(file = paste(data_path,sub_folders[i],'/',dccs_fileName, sep = "", collapse = NULL), stringsAsFactors = TRUE)
    
    # SET UP ONLY THE NECESSARY COLUMNS, REMOVE NAs AND CUT OUT THE FIRST 2 AND THE LAST 4 ROWS
    selectedCols <- c("ISI", "key_resp_13.keys", "key_resp_13.corr", "key_resp_13.rt", "left", 
                      "right", "cue", "corrAns", "participant", "session", 
                      "date", "expName", "psychopyVersion", "OS", "frameRate", "id")
    
    dccsTrim <- dccsDat[selectedCols]
    testDat <- dccsTrim[dccsTrim$cue %in% c("SHAPE", "COLOR"),]
    trial_length <- nrow(testDat) - 4
    dccsTrimTrial <- testDat[3:trial_length,]
    
    
    #First we need to find the accuracy score 
    accPoint <- sum(dccsTrimTrial$key_resp_13.corr, na.rm = TRUE)
    accScore <- 0.125 * (accPoint + 10)
    accPercent <- (accPoint/30) 
    
    #If the accuracy rate is below or equals 80% then total computed score = accuracy score.
    
    if (accPercent <= .8){
      totScore <- accScore
      rtMed <- NA
      
    } else {
      
      #CALCULATING REACTION TIME
      #find median RT only for the trials on less frequent cue 
      onlyShape <- dccsTrimTrial[dccsTrimTrial$cue == "SHAPE",]
      onlyShCorr <- onlyShape[onlyShape$key_resp_13.corr == 1,]
      rtMean <- mean(onlyShCorr$key_resp_13.rt)
      StDev <- sd(onlyShCorr$key_resp_13.rt)
      
      # if median RT is between 100ms and 3 standard deviations from the mean then use log base 10
      
        if ((rtMean >= 0.1) & (StDev < 3 ))  {
          rtMed <- median(onlyShCorr$key_resp_13.rt) * 1000
          logRtMed <- log10(rtMed)
          rtScore <- 5 - abs(5 * abs((logRtMed - log10(500))/(log10(3000)-log10(500))))
          totScore <- accScore + rtScore
      }
    }
    
    id <- dccsTrim$id[1]
    pointsDCCS[nrow(pointsDCCS) + 1,] <-c(id,
                                          accScore,
                                          accPercent,
                                          rtMed,
                                          rtScore,
                                          totScore)
  }
  
  #if participant did not have a dccs file, skip to next participant
  else {
  print("Ruh Roh... missing file")
}

}

#write the extracted summary scores to disk
write.csv(pointsDCCS,paste(out_path,proc_fileName, sep = "", collapse = NULL), row.names=FALSE) 
