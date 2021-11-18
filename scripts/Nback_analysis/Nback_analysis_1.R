library(readr)
library(dplyr)
library(purrr)
#The script is written by George Buzzell and Arina Polyanskaya
#set up root dir for data to be processed
data_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'

#set up output dir and file name
out_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'
proc_fileName <- "2021-11-05_nback_behavior_summary.csv"

#pull out all the subfolders (participant folders) for the root data dir
sub_folders <- list.files(data_path, pattern = "sub")

#create dataframe where we will store summary data
summaryDat <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("id", "rep_Acc_mean", "no_rep_Acc_mean", "repCorr_RT_mean", "no_repCorr_RT_mean", "repCorr_logRT_mean", "no_repCorr_logRT_mean", "nbackEff_Acc", "nbackEff_RT", "nbackEff_logRT"))

#loop over participant (subfolders)
for(i in 1:length(sub_folders)) {
  
  #for this participant, find the flanker file name
   nback_match <- list.files(paste(data_path,sub_folders[i], sep = "", collapse = NULL), pattern = "nback")
   csv_match <- list.files(paste(data_path,sub_folders[i], sep = "", collapse = NULL), pattern = "\\.csv")
   nback_fileName <- print(intersect(nback_match,csv_match))
  
  #logical to make sure there is a flanker file for this participant before loading, else, skip to next participant
  if (!identical(nback_fileName, character(0))) {
    print("Woohoo! Processing file!")
    
    #read in the data for this participant
    nbackDat <- read.csv(file = paste(data_path,sub_folders[i],'/',nback_fileName, sep = "", collapse = NULL), stringsAsFactors = TRUE)
    
    #let's get a list of the var names
    #dput(names(nbackDat))
    
    complete.cases(nbackDat$stimLoop2.ran)
    
    #remove practice trials and any rows that do not reflect experiment data
    testDat <- subset(nbackDat, complete.cases(nbackDat$stimLoop2.ran))
    
    #create a vector of vars we want to keep
    selectedCols <- c("ISI", "participant", "session", 
                      "date", "expName", "psychopyVersion", "OS", "frameRate", "id", "tarOrNot", "key_resp.keys", "key_resp.corr", "key_resp.rt")
    
    #remove unwanted vars from data frame
    testDatTrim <- testDat[selectedCols]
    
    #Identify the repeating and not repeating trials
    testDatTrim$n_repeat <- with(testDatTrim, as.numeric(testDatTrim$tarOrNot) == 1 )
    #we now have a new variable "n_repeat" that is "TRUE" for repeating trials and "FALSE for not repeating trials
    
    #remove first two rows
    testDatTrim[1, ] <- NA
    testDatTrim[2, ] <- NA
    
    #filter out NA fields
    testDatTrim <- na.omit(testDatTrim)
    
    #subset the data for repeating and not repeating trials, creating new data frames for each
    testDatTrim_rep <- testDatTrim[testDatTrim$n_repeat, ]
    testDatTrim_no_rep <- testDatTrim[!testDatTrim$n_repeat, ] #note that we take the inverse of "repeating" here to get "not repeating"
    
    #compute mean repeating and no repeating accuracy
    rep_Acc_mean <- mean(testDatTrim_rep$key_resp.corr)
    no_rep_Acc_mean <- mean(testDatTrim_no_rep$key_resp.corr)
    
    #subset the data for correct trials only, separately for congruent and incongruent trials, creating new data frames for each
    testDatTrim_repCorr <- testDatTrim_rep[testDatTrim_rep$key_resp.corr==1, ]
    testDatTrim_no_repCorr <- testDatTrim_no_rep[testDatTrim_no_rep$key_resp.corr==1, ]
    
    #For correct trials, compute mean repeating and no repeating RT WITHOUT log scaling
    repCorr_RT_mean <- mean(testDatTrim_rep$key_resp.rt)
    no_repCorr_RT_mean <- mean(testDatTrim_no_rep$key_resp.rt)
    
    #For correct trials, compute mean repeating and not repeating RT WITH log scaling
    repCorr_logRT_mean <- mean(log((1+testDatTrim_repCorr$key_resp.rt))) #note that we add 1 first before taking log to avoid negative log values
    no_repCorr_logRT_mean <- mean(log((1+testDatTrim_no_repCorr$key_resp.rt))) #note that we add 1 first before taking log to avoid negative log values
    
    #compute nback-effect scores for accuracy, RT, log-RT
    nbackEff_Acc <- no_rep_Acc_mean-rep_Acc_mean
    nbackEff_RT <- no_repCorr_RT_mean-repCorr_RT_mean
    nbackEff_logRT <- no_repCorr_logRT_mean-repCorr_logRT_mean
    
    #store data in summary matrix
    id <- testDatTrim$id[1]
    summaryDat[nrow(summaryDat) + 1,] <-c(id, rep_Acc_mean, no_rep_Acc_mean, repCorr_RT_mean, no_repCorr_RT_mean, repCorr_logRT_mean, no_repCorr_logRT_mean, nbackEff_Acc, nbackEff_RT, nbackEff_logRT)
    
    #if participant did not have a flanker file, skip to next participant
  } else {
    print("Ruh Roh... missing file")
  }
  
}

#write the extracted summary scores to disk
write.csv(summaryDat,paste(out_path,proc_fileName, sep = "", collapse = NULL), row.names=FALSE) 

##read data back in to check it is correct
#temp <- read.csv(file = paste(out_path,proc_fileName, sep = "", collapse = NULL))

library(lsr)
t.test(summaryDat$no_repCorr_RT_mean, summaryDat$repCorr_RT_mean, alternative = "two.sided", paired = TRUE)