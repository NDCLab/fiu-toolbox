library(readr)
library(dplyr)
library(purrr)
library(plotrix)
library(lsr)

#The script is adopted from Flanker NIH Cognition Battery Tool

#access all the flanker files from a folder (change directory to the one on your computer)
#set up root dir for data to be processed
data_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'

#set up output dir and file name
out_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'
proc_fileName <- paste("flanker_points_", Sys.Date(), ".csv", sep="", collapse=NULL)

#pull out all the subfolders (participant folders) for the root data dir
sub_folders <- list.files(data_path, pattern = "sub")

#create a spaceholder for the results 
pointsFlanker <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("id",
                                                                 "accScore",
                                                                 "accPercent",
                                                                 "rtMed",
                                                                 "rtScore",
                                                                 "totScore"))

#create a loop for processing each file in the folder (export and open each file)
#loop over participant (subfolders)
for(i in 1:length(sub_folders)) {
  #for this participant, find the flanker file name
  flanker_match <- list.files(paste(data_path,sub_folders[i], sep = "", collapse = NULL), pattern = "flanker")
  csv_match <- list.files(paste(data_path,sub_folders[i], sep = "", collapse = NULL), pattern = "\\.csv")
  flanker_fileName <- print(intersect(flanker_match,csv_match))
  
  #logical to make sure there is a dcccs file for this participant before loading, else, skip to next participant
  if (!identical(flanker_fileName, character(0))) {
    print("Woohoo! Processing file!")
    
    #read in the data for this participant
    flankerDat <- read.csv(file = paste(data_path,sub_folders[i],'/',flanker_fileName, sep = "", collapse = NULL), stringsAsFactors = TRUE)
  
    # SET UP ONLY THE NECESSARY COLUMNS, REMOVE NAs AND CUT OUT THE FIRST 2 AND THE LAST 4 ROWS
    selectedCols <- c("ISI", "key_resp_2.keys", "key_resp_2.corr", "key_resp_2.rt", "middle", 
                       "corrAns", "participant", "session", 
                      "date", "expName", "psychopyVersion", "OS", "frameRate", "id", "test_trials.thisRepN")
    
    flankerTrim <- flankerDat[selectedCols]
    testDat <- flankerTrim[flankerTrim$test_trials.thisRepN %in% c("0"),]
    flankerTrimTrial <- testDat
    
    
    #First we need to find the accuracy score 
    accPoint <- sum(flankerTrimTrial$key_resp_2.corr, na.rm = TRUE)
    accScore <- 0.125 * (accPoint + 20)
    accPercent <- (accPoint/20) 
    
    #If the accuracy rate is below or equals 80% then total computed score = accuracy score.
    
    if (accPercent <= .8){
      totScore <- accScore
      rtMed <- NA
      
    } else {
      
      #CALCULATING REACTION TIME
      #find median RT only for the trials on less frequent cue 
      onlyIncon <- flankerTrimTrial[flankerTrimTrial$middle %in% c("img/incongruent_right.png", "img/incongruent_left.png"),]
      onlyInconCorr <- onlyIncon[onlyIncon$key_resp_2.corr == 1,]
      rtMean <- mean(onlyInconCorr$key_resp_2.rt)
      StDev <- sd(onlyInconCorr$key_resp_2.rt)
      
      # if mean RT is between 100ms and 3 standard deviations from the mean then use log base 10
      
      if ((rtMean >= 0.1) & (StDev < 3))  {
        rtMed <- median(onlyInconCorr$key_resp_2.rt) * 1000
        #if RT median is less than 500 ms then it needs to be truncated (set equal to 500ms)
        if (rtMed < 500){
          rtMed <- 500
        }
        #if RT median is more than 3000 but no more than 10,000 then it needs to be truncated (set equal to 3,000ms)
        if ((rtMed > 3000) & (rtMed < 10000)){
          rtMed <- 3000
        }
        logRtMed <- log10(rtMed)
        rtScore <- 5 - abs(5 * abs((logRtMed - log10(500))/(log10(3000)-log10(500))))
        totScore <- accScore + rtScore
      }
    }
    
    id <- flankerTrim$id[1]
    pointsFlanker[nrow(pointsFlanker) + 1,] <-c(id,
                                          accScore,
                                          accPercent,
                                          rtMed,
                                          rtScore,
                                          totScore)
  }
  
  #if participant did not have a flanker file, skip to next participant
  else {
    print("Ruh Roh... missing file")
  }
  
}

#write the extracted summary scores to disk
write.csv(pointsFlanker,paste(out_path,proc_fileName, sep = "", collapse = NULL), row.names=FALSE) 
