# NBack Preprocessing
# Version: 1
# Authors: George A. Buzzell, Jessica M. Alexander, Arina Polyanskaya
# Last Updated: 2022-02-04

# VARIABLES
# _meanACC : mean accuracy
# _meanRT : mean response time
# _logMeanRT : log-corrected mean response time

# nRepeat : current trial is identical to the last trial
# nNoRepeat : current trial is NOT identical to the last trial

# nbackEff : delta in accuracy or response time between repeat and non-repeat trials


#set up root dir for input data
data_path <- '/Users/jalexand/github/training-r/data/example_pavlovia_dat/pavlovia/'

#set up output dir and filename
out_path <- '/Users/jalexand/github/training-r/results/'
proc_fileName <- paste("nback_subject-level_summary_", Sys.Date(), ".csv", sep="", collapse=NULL)
trial_fileName <- paste("nback_trial-level_summary_", Sys.Date(), ".csv", sep="", collapse=NULL)

#identify subfolders (participant folders) for the root data dir
sub_folders <- list.files(data_path, pattern = "sub")

#create dataframe in which we will store summary data
summaryDat <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(summaryDat) <- c("id",
                          "overall_accuracy",
                          "nRepeat_meanACC",
                          "nNoRepeat_meanACC",
                          "nRepeat_meanRT",
                          "nNoRepeat_meanRT",
                          "nRepeat_logMeanRT",
                          "nNoRepeat_logMeanRT",
                          "nbackEffect_meanACC",
                          "nbackEffect_meanRT",
                          "nbackEffect_logMeanRT")

#create dataframe for trial-level information across all participants
trialDat <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(trialDat) <- c("id", "trial_no", "acc", "rt", "nRepeat")

#loop over participants (subfolders)
for(i in 1:length(sub_folders)){

  #for this participant, find the dccs csv file
  nback_file <- list.files(paste(data_path,sub_folders[i], sep = "", collapse = NULL), pattern = ".*(nback)+.*(.csv)")

  #logical to make sure there is a dccs file for this participant before loading, else skip to next participant
  if (!identical(nback_file, character(0))) {
    print("Woohoo! Processing file!")

    #read in the data for this participant
    nbackDat <- read.csv(file = paste(data_path,sub_folders[i],'/',nback_file, sep = "", collapse = NULL), stringsAsFactors = FALSE)
    id <- nbackDat$id[1]

    #remove practice trials and any rows that do not reflect experiment data, remove unwanted variables from df
    testDat <- nbackDat[!is.na(nbackDat$stimLoop2.thisN),]
    testDatTrim <- testDat[c("tarOrNot", "key_resp.keys", "key_resp.corr", "key_resp.rt")]
    testDatTrim <- testDatTrim[3:nrow(testDatTrim),]

    #calculate overall participant accuracy
    overall_accuracy <- mean(testDatTrim$key_resp.corr)

    #ADD NEW VECTOR TO DATAFRAME TO IDENTIFY REPEAT STIMULI
    nRepeat <- testDatTrim$tarOrNot==1
    testDatTrim$nRepeat <- nRepeat

    #CALCULATE ACCURACY
    testDatTrim_nRepeatTrue <- testDatTrim[testDatTrim$nRepeat==TRUE,]
    nRepeat_meanACC <- mean(testDatTrim_nRepeatTrue$key_resp.corr)

    testDatTrim_nRepeatFalse <- testDatTrim[testDatTrim$nRepeat==FALSE,]
    nNoRepeat_meanACC <- mean(testDatTrim_nRepeatFalse$key_resp.corr)

    #CALCULATE RT
    testDatTrimCorr <- testDatTrim[testDatTrim$key_resp.corr==1,]
    testDatTrimCorr_nRepeatTrue <- testDatTrimCorr[testDatTrimCorr$nRepeat==TRUE,]
    testDatTrimCorr_nRepeatFalse <- testDatTrimCorr[testDatTrimCorr$nRepeat==FALSE,]

    nRepeat_meanRT <- mean(testDatTrimCorr_nRepeatTrue$key_resp.rt)
    nNoRepeat_meanRT <- mean(testDatTrimCorr_nRepeatFalse$key_resp.rt)
    nRepeat_logMeanRT <- mean(log(1+testDatTrimCorr_nRepeatTrue$key_resp.rt))
    nNoRepeat_logMeanRT <- mean(log(1+testDatTrimCorr_nRepeatFalse$key_resp.rt))

    #COMPUTE EFFECT SCORES
    nbackEffect_meanACC <-  nRepeat_meanACC - nNoRepeat_meanACC
    nbackEffect_meanRT <- nRepeat_meanRT - nNoRepeat_meanRT
    nbackEffect_logMeanRT <- nRepeat_logMeanRT - nNoRepeat_logMeanRT

    #STORE OUTPUT DATA IN SUMMARY MATRIX
    summaryDat[nrow(summaryDat) + 1,] <-c(id,
                                          overall_accuracy,
                                          nRepeat_meanACC,
                                          nNoRepeat_meanACC,
                                          nRepeat_meanRT,
                                          nNoRepeat_meanRT,
                                          nRepeat_logMeanRT,
                                          nNoRepeat_logMeanRT,
                                          nbackEffect_meanACC,
                                          nbackEffect_meanRT,
                                          nbackEffect_logMeanRT)

    #STORE TRIAL LEVEL DATA
    for (j in 1:nrow(testDatTrim)){
      trial_no <- j
      acc <- testDatTrim$key_resp.corr[j]
      rt <- testDatTrim$key_resp.rt[j]
      nRepeat <- testDatTrim$nRepeat[j]==1

      trialDat[nrow(trialDat) + 1,] <-c(id, trial_no, acc, rt, nRepeat)
    }
  }

#if participant did not have an nback file, skip to next participant
  else {
  print("Ruh Roh... missing file")
}
}

#write the extracted summary scores to CSV
write.csv(summaryDat,paste(out_path,proc_fileName, sep = "", collapse = NULL), row.names=FALSE)
write.csv(trialDat,paste(out_path,trial_fileName, sep = "", collapse = NULL), row.names=FALSE)
