# DCCS Preprocessing
# Version: 1
# Authors: Jessica M. Alexander, George A. Buzzell, Arina Polyanskaya
# Last Updated: 2022-02-04

# VARIABLES
# _meanACC : mean accuracy
# _seACC : standard error of mean accuracy
# _meanRT : mean response time
# _seRT : standard error of mean response time
# _logMeanRT : log-corrected mean response time
# _medianRT : median response time
# _logMedianRT : log-corrected median response time (specifically calculated based on Zelazo XXX)
# _costAcc : delta in accuracy between switch and non-/pre-switch
# _costRT : delta in mean response time between switch and non-/pre-switch
# _costLogRT : delta in log-corrected mean response time between switch and non-/pre-switch

# anySwitch : any trials where cue is different than the preceding trial
# toShape : any trial where the cue is the (less frequent) SHAPE cue
# toColor : any trial following the (less frequent) SHAPE cue that returns to the (more frequent) COLOR cue

# Corr : indication that only correct trials are included in the subset
# OPP : indication of the non-switch set for the given "switch" definition
# PRE : indication of the pre-switch set for the given "switch" definition
# _nonswit : indication that comparison is between switch and nonswitch groups
# _preswit : indication that comparison is between switch and preswitch groups
# _preShape : indication that comparison is with the toShapePRE group


#set up root dir for input data
data_path <- '/Users/jalexand/github/training-r/data/example_pavlovia_dat/pavlovia/'

#set up output dir and filename
out_path <- '/Users/jalexand/github/training-r/results/'
proc_fileName <- paste("dccs_subject-level_summary_", Sys.Date(), ".csv", sep="", collapse=NULL)
trial_fileName <- paste("dccs_trial-level_summary_", Sys.Date(), ".csv", sep="", collapse=NULL)

#identify subfolders (participant folders) for the root data dir
sub_folders <- list.files(data_path, pattern = "sub")

#create dataframe in which we will store summary data
summaryDat <- data.frame(matrix(ncol = 68, nrow = 0))
colnames(summaryDat) <- c("id",
                          "overall_accuracy",
                          "anySwitch_meanACC",
                          "anySwitchOPP_meanACC",
                          "anySwitch_seACC",
                          "anySwitchOPP_seACC",
                          "anySwitchCorr_meanRT",
                          "anySwitchOPPCorr_meanRT",
                          "anySwitchCorr_seRT",
                          "anySwitchOPPCorr_seRT",
                          "anySwitchCorr_logMeanRT",
                          "anySwitchOPPCorr_logMeanRT",
                          "anySwitchCorr_medianRT",
                          "anySwitchOPPCorr_medianRT",
                          "anySwitch_costAcc",
                          "anySwitch_costRT",
                          "anySwitch_costLogRT",
                          "toShape_meanACC",
                          "toShapeOPP_meanACC",
                          "toShapePRE_meanACC",
                          "toShape_seACC",
                          "toShapeOPP_seACC",
                          "toShapePRE_seACC",
                          "toShapeCorr_meanRT",
                          "toShapeOPPCorr_meanRT",
                          "toShapePRECorr_meanRT",
                          "toShapeCorr_seRT",
                          "toShapeOPPCorr_seRT",
                          "toShapePRECorr_seRT",
                          "toShapeCorr_logMeanRT",
                          "toShapeOPPCorr_logMeanRT",
                          "toShapePRECorr_logMeanRT",
                          "toShapeCorr_medianRT",
                          "toShapeOPPCorr_medianRT",
                          "toShapePRECorr_medianRT",
                          "toShape_costAcc_nonswit",
                          "toShape_costRT_nonswit",
                          "toShape_costLogRT_nonswit",
                          "toShape_costAcc_preswit",
                          "toShape_costRT_preswit",
                          "toShape_costLogRT_preswit",
                          "toColor_meanACC",
                          "toColorOPP_meanACC",
                          "toColorPRE_meanACC",
                          "toColor_seACC",
                          "toColorOPP_seACC",
                          "toColorPRE_seACC",
                          "toColorCorr_meanRT",
                          "toColorOPPCorr_meanRT",
                          "toColorPRECorr_meanRT",
                          "toColorCorr_seRT",
                          "toColorOPPCorr_seRT",
                          "toColorPRECorr_seRT",
                          "toColorCorr_logMeanRT",
                          "toColorOPPCorr_logMeanRT",
                          "toColorPRECorr_logMeanRT",
                          "toColorCorr_medianRT",
                          "toColorOPPCorr_medianRT",
                          "toColorPRECorr_medianRT",
                          "toColor_costAcc_nonswit",
                          "toColor_costRT_nonswit",
                          "toColor_costLogRT_nonswit",
                          "toColor_costAcc_preswit",
                          "toColor_costRT_preswit",
                          "toColor_costLogRT_preswit",
                          "toColor_costAcc_preShape",
                          "toColor_costRT_preShape",
                          "toColor_costLogRT_preShape")

#create dataframe for trial-level information across all participants
trialDat <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(trialDat) <- c("id", "trial_no", "acc", "rt", "trial_type")

#loop over participants (subfolders)
for(i in 1:length(sub_folders)){

  #for this participant, find the dccs csv file
  dccs_file <- list.files(paste(data_path,sub_folders[i], sep = "", collapse = NULL), pattern = ".*(dccs)+.*(.csv)")

  #logical to make sure there is a dccs file for this participant before loading, else skip to next participant
  if (!identical(dccs_file, character(0))) {
    print("Woohoo! Processing file!")

    #read in the data for this participant
    dccsDat <- read.csv(file = paste(data_path,sub_folders[i],'/',dccs_file, sep = "", collapse = NULL), stringsAsFactors = FALSE)
    id <- dccsDat$id[1]

    #remove practice trials and any rows that do not reflect experiment data, remove unwanted variables from df
    testDat <- dccsDat[dccsDat$cue %in% c("SHAPE", "COLOR"),]
    testDatTrim <- testDat[c("key_resp_13.corr", "key_resp_13.rt", "cue")]

    #calculate overall participant accuracy
    overall_accuracy <- mean(testDatTrim$key_resp_13.corr)

    #ADD NEW VECTORS TO DATAFRAME TO ENABLE SWITCH, NONSWITCH, AND PRESWITCH COMPARISONS
    #create new vectors with prior cue and prior cue accuracy
    priorCue <- c("HOLD", testDatTrim$cue)
    testDatTrim$priorCue <- priorCue[1:(length(priorCue)-1)]

    priorCueAcc <- c(2, testDatTrim$key_resp_13.corr)
    testDatTrim$priorCueAcc <- priorCueAcc[1:(length(priorCueAcc)-1)]

    #create new vector with following cue and following cue accuracy
    nextCue <- c(testDatTrim$cue[2:(length(testDatTrim$cue))], "HOLD")
    testDatTrim$nextCue <- nextCue

    nextCueAcc <- c(testDatTrim$key_resp_13.corr[2:(length(testDatTrim$key_resp_13.corr))], 2)
    testDatTrim$nextCueAcc <- nextCueAcc

    #create new vector identifying if current cue is same as prior, TRUE when cues differ and FALSE when cue is same as previous
    #create new vector identifying if either switch and preswitch trial were answered erroneously, TRUE if either switch or preswitch was error
    #remove first two trials
    testDatTrim$switchStatus <- testDatTrim$cue != testDatTrim$priorCue
    testDatTrim$toShapeSwitchPairErr <- (testDatTrim$cue == "COLOR" & testDatTrim$nextCue == "SHAPE" & (!testDatTrim$key_resp_13.corr | !testDatTrim$nextCueAcc)) |
                                        (testDatTrim$cue == "SHAPE" & (!testDatTrim$key_resp_13.corr | !testDatTrim$priorCueAcc))
    testDatTrim$toColorSwitchPairErr <- (testDatTrim$cue == "SHAPE" & (!testDatTrim$key_resp_13.corr | !testDatTrim$nextCueAcc)) |
                                        (testDatTrim$cue == "COLOR" & testDatTrim$priorCue == "SHAPE" & (!testDatTrim$key_resp_13.corr | !testDatTrim$priorCueAcc))
    testDatTrim <- testDatTrim[3:nrow(testDatTrim),]


    #ANYSWITCH
    #switch defined as above, comparison between switch group and nonswitch group

    #subset the data for switch versus nonswitch trials, create new dataframes for each
    testDatTrim_anySwitch <- testDatTrim[testDatTrim$switchStatus, ]
    testDatTrim_anySwitchOPP <- testDatTrim[!testDatTrim$switchStatus, ]

    #compute mean accuracy and standard error for switch versus nonswitch trials
    anySwitch_meanACC <- mean(testDatTrim_anySwitch$key_resp_13.corr)
    anySwitchOPP_meanACC <- mean(testDatTrim_anySwitchOPP$key_resp_13.corr)

    anySwitch_seACC <- std.error(testDatTrim_anySwitch$key_resp_13.corr)
    anySwitchOPP_seACC <- std.error(testDatTrim_anySwitchOPP$key_resp_13.corr)

    #subset the data for correct trials only, separately for switch versus nonswitch trials, creating new dataframes for each
    testDatTrim_anySwitchCorr <- testDatTrim_anySwitch[testDatTrim_anySwitch$key_resp_13.corr==1, ]
    testDatTrim_anySwitchOPPCorr <- testDatTrim_anySwitchOPP[testDatTrim_anySwitchOPP$key_resp_13.corr==1, ]

    #for correct trials, for both switch and nonswitch groups, compute mean RT (raw and log-corrected), standard error, and median RT
    anySwitchCorr_meanRT <- mean(testDatTrim_anySwitchCorr$key_resp_13.rt)
    anySwitchOPPCorr_meanRT <- mean(testDatTrim_anySwitchOPPCorr$key_resp_13.rt)

    anySwitchCorr_seRT <- std.error(testDatTrim_anySwitchCorr$key_resp_13.rt)
    anySwitchOPPCorr_seRT <- std.error(testDatTrim_anySwitchOPPCorr$key_resp_13.rt)

    anySwitchCorr_logMeanRT <- mean(log((1+testDatTrim_anySwitchCorr$key_resp_13.rt)))
    anySwitchOPPCorr_logMeanRT <- mean(log((1+testDatTrim_anySwitchOPPCorr$key_resp_13.rt)))

    anySwitchCorr_medianRT <- median(testDatTrim_anySwitchCorr$key_resp_13.rt)
    anySwitchOPPCorr_medianRT <- median(testDatTrim_anySwitchOPPCorr$key_resp_13.rt)

    #compute switch cost scores for accuracy, RT, log-RT
    anySwitch_costAcc <-  anySwitchOPP_meanACC - anySwitch_meanACC
    anySwitch_costRT <- anySwitchCorr_meanRT - anySwitchOPPCorr_meanRT
    anySwitch_costLogRT <- anySwitchCorr_logMeanRT - anySwitchOPPCorr_logMeanRT


    #TOSHAPE
    #switch defined as above, independent calculations for switch/nonswitch and switch/preswitch

    #subset the data for switch, preswitch, and nonswitch trials
    testDatTrim_toShapePairs <- subset(testDatTrim, (testDatTrim$cue=="SHAPE" | testDatTrim$nextCue=="SHAPE"))
    if(testDatTrim_toShapePairs$cue[1]=="SHAPE"){
      testDatTrim_toShapePairs <- testDatTrim_toShapePairs[2:length(testDatTrim_toShapePairs$cue),] #remove first row because no COLOR trial follows final SHAPE trial
    }

    testDatTrim_toShape <- subset(testDatTrim_toShapePairs, testDatTrim_toShapePairs$cue == "SHAPE")
    testDatTrim_toShapePRE <- subset(testDatTrim_toShapePairs, testDatTrim_toShapePairs$nextCue == "SHAPE")

    testDatTrim_toShapeOPP <- subset(testDatTrim, testDatTrim$cue != "SHAPE")

    #compute mean accuracy and standard error for switch versus preswitch/nonswitch trials
    toShape_meanACC <- mean(testDatTrim_toShape$key_resp_13.corr)
    toShapeOPP_meanACC <- mean(testDatTrim_toShapeOPP$key_resp_13.corr)
    toShapePRE_meanACC <- mean(testDatTrim_toShapePRE$key_resp_13.corr)

    toShape_seACC <- std.error(testDatTrim_toShape$key_resp_13.corr)
    toShapeOPP_seACC <- std.error(testDatTrim_toShapeOPP$key_resp_13.corr)
    toShapePRE_seACC <- std.error(testDatTrim_toShapePRE$key_resp_13.corr)

    #subset the data for correct trial pairs only, creating new dataframes for each
    testDatTrim_toShapeCorr <- testDatTrim_toShape[!testDatTrim_toShape$toShapeSwitchPairErr, ]
    testDatTrim_toShapeOPPCorr <- testDatTrim_toShapeOPP[testDatTrim_toShapeOPP$key_resp_13.corr==1, ]
    testDatTrim_toShapePRECorr <- testDatTrim_toShapePRE[!testDatTrim_toShapePRE$toShapeSwitchPairErr, ]

    #for correct trials, compute mean RT (raw and log-corrected), standard error, and median RT
    toShapeCorr_meanRT <- mean(testDatTrim_toShapeCorr$key_resp_13.rt)
    toShapeOPPCorr_meanRT <- mean(testDatTrim_toShapeOPPCorr$key_resp_13.rt)
    toShapePRECorr_meanRT <- mean(testDatTrim_toShapePRECorr$key_resp_13.rt)

    toShapeCorr_seRT <- std.error(testDatTrim_toShapeCorr$key_resp_13.rt)
    toShapeOPPCorr_seRT <- std.error(testDatTrim_toShapeOPPCorr$key_resp_13.rt)
    toShapePRECorr_seRT <- std.error(testDatTrim_toShapePRECorr$key_resp_13.rt)

    toShapeCorr_logMeanRT <- mean(log((1+testDatTrim_toShapeCorr$key_resp_13.rt)))
    toShapeOPPCorr_logMeanRT <- mean(log((1+testDatTrim_toShapeOPPCorr$key_resp_13.rt)))
    toShapePRECorr_logMeanRT <- mean(log((1+testDatTrim_toShapePRECorr$key_resp_13.rt)))

    toShapeCorr_medianRT <- median(testDatTrim_toShapeCorr$key_resp_13.rt)
    toShapeOPPCorr_medianRT <- median(testDatTrim_toShapeOPPCorr$key_resp_13.rt)
    toShapePRECorr_medianRT <- median(testDatTrim_toShapePRECorr$key_resp_13.rt)

    #compute switch cost scores for accuracy, RT, log-RT
    toShape_costAcc_nonswit <-  toShapeOPP_meanACC - toShape_meanACC
    toShape_costRT_nonswit <- toShapeCorr_meanRT - toShapeOPPCorr_meanRT
    toShape_costLogRT_nonswit <- toShapeCorr_logMeanRT - toShapeOPPCorr_logMeanRT
    toShape_costAcc_preswit <-  toShapePRE_meanACC - toShape_meanACC
    toShape_costRT_preswit <- toShapeCorr_meanRT - toShapePRECorr_meanRT
    toShape_costLogRT_preswit <- toShapeCorr_logMeanRT - toShapePRECorr_logMeanRT


    #TOCOLOR
    #switch defined as above, independent calculations for switch/nonswitch and switch/preswitch

    #subset the data for switch, preswitch, and nonswitch trials
    testDatTrim_toColorPairs <- testDatTrim[testDatTrim$cue=="SHAPE" | testDatTrim$priorCue=="SHAPE",]
    testDatTrim_toColorPairs <- testDatTrim_toColorPairs[1:length(testDatTrim_toColorPairs$cue)-1,] #remove last row because no COLOR trial follows final SHAPE trial

    testDatTrim_toColor <- subset(testDatTrim_toColorPairs, testDatTrim_toColorPairs$priorCue == "SHAPE")
    testDatTrim_toColorPRE <- subset(testDatTrim_toColorPairs, testDatTrim_toColorPairs$cue == "SHAPE")

    testDatTrim_toColorOPP <- subset(testDatTrim, testDatTrim$priorCue == "COLOR")

    #compute mean accuracy and standard error for switch versus preswitch/nonswitch trials
    toColor_meanACC <- mean(testDatTrim_toColor$key_resp_13.corr)
    toColorOPP_meanACC <- mean(testDatTrim_toColorOPP$key_resp_13.corr)
    toColorPRE_meanACC <- mean(testDatTrim_toColorPRE$key_resp_13.corr)

    toColor_seACC <- std.error(testDatTrim_toColor$key_resp_13.corr)
    toColorOPP_seACC <- std.error(testDatTrim_toColorOPP$key_resp_13.corr)
    toColorPRE_seACC <- std.error(testDatTrim_toColorPRE$key_resp_13.corr)

    #subset the data for correct trial pairs only, creating new dataframes for each
    testDatTrim_toColorCorr <- testDatTrim_toColor[!testDatTrim_toColor$toColorSwitchPairErr, ]
    testDatTrim_toColorOPPCorr <- testDatTrim_toColorOPP[testDatTrim_toColorOPP$key_resp_13.corr==1, ]
    testDatTrim_toColorPRECorr <- testDatTrim_toColorPRE[!testDatTrim_toColorPRE$toColorSwitchPairErr, ]

    #for correct trials, compute mean RT (raw and log-corrected), standard error, and median RT
    toColorCorr_meanRT <- mean(testDatTrim_toColorCorr$key_resp_13.rt)
    toColorOPPCorr_meanRT <- mean(testDatTrim_toColorOPPCorr$key_resp_13.rt)
    toColorPRECorr_meanRT <- mean(testDatTrim_toColorPRECorr$key_resp_13.rt)

    toColorCorr_seRT <- std.error(testDatTrim_toColorCorr$key_resp_13.rt)
    toColorOPPCorr_seRT <- std.error(testDatTrim_toColorOPPCorr$key_resp_13.rt)
    toColorPRECorr_seRT <- std.error(testDatTrim_toColorPRECorr$key_resp_13.rt)

    toColorCorr_logMeanRT <- mean(log((1+testDatTrim_toColorCorr$key_resp_13.rt)))
    toColorOPPCorr_logMeanRT <- mean(log((1+testDatTrim_toColorOPPCorr$key_resp_13.rt)))
    toColorPRECorr_logMeanRT <- mean(log((1+testDatTrim_toColorPRECorr$key_resp_13.rt)))

    toColorCorr_medianRT <- median(testDatTrim_toColorCorr$key_resp_13.rt)
    toColorOPPCorr_medianRT <- median(testDatTrim_toColorOPPCorr$key_resp_13.rt)
    toColorPRECorr_medianRT <- median(testDatTrim_toColorPRECorr$key_resp_13.rt)

    #compute switch cost scores for accuracy, RT, log-RT
    toColor_costAcc_nonswit <-  toColorOPP_meanACC - toColor_meanACC
    toColor_costRT_nonswit <- toColorCorr_meanRT - toColorOPPCorr_meanRT
    toColor_costLogRT_nonswit <- toColorCorr_logMeanRT - toColorOPPCorr_logMeanRT
    toColor_costAcc_preswit <-  toColorPRE_meanACC - toColor_meanACC
    toColor_costRT_preswit <- toColorCorr_meanRT - toColorPRECorr_meanRT
    toColor_costLogRT_preswit <- toColorCorr_logMeanRT - toColorPRECorr_logMeanRT

    #compute switch cost scores (accuracy, RT, log-RT) comparing toShapePRE with toColor
    toColor_costAcc_preShape <-  toShapePRE_meanACC - toColor_meanACC
    toColor_costRT_preShape <- toColorCorr_meanRT - toShapePRECorr_meanRT
    toColor_costLogRT_preShape <- toColorCorr_logMeanRT - toShapePRECorr_logMeanRT

    #STORE OUTPUT DATA IN SUMMARY MATRIX
    summaryDat[nrow(summaryDat) + 1,] <-c(id,
                                          overall_accuracy,
                                          anySwitch_meanACC,
                                          anySwitchOPP_meanACC,
                                          anySwitch_seACC,
                                          anySwitchOPP_seACC,
                                          anySwitchCorr_meanRT,
                                          anySwitchOPPCorr_meanRT,
                                          anySwitchCorr_seRT,
                                          anySwitchOPPCorr_seRT,
                                          anySwitchCorr_logMeanRT,
                                          anySwitchOPPCorr_logMeanRT,
                                          anySwitchCorr_medianRT,
                                          anySwitchOPPCorr_medianRT,
                                          anySwitch_costAcc,
                                          anySwitch_costRT,
                                          anySwitch_costLogRT,
                                          toShape_meanACC,
                                          toShapeOPP_meanACC,
                                          toShapePRE_meanACC,
                                          toShape_seACC,
                                          toShapeOPP_seACC,
                                          toShapePRE_seACC,
                                          toShapeCorr_meanRT,
                                          toShapeOPPCorr_meanRT,
                                          toShapePRECorr_meanRT,
                                          toShapeCorr_seRT,
                                          toShapeOPPCorr_seRT,
                                          toShapePRECorr_seRT,
                                          toShapeCorr_logMeanRT,
                                          toShapeOPPCorr_logMeanRT,
                                          toShapePRECorr_logMeanRT,
                                          toShapeCorr_medianRT,
                                          toShapeOPPCorr_medianRT,
                                          toShapePRECorr_medianRT,
                                          toShape_costAcc_nonswit,
                                          toShape_costRT_nonswit,
                                          toShape_costLogRT_nonswit,
                                          toShape_costAcc_preswit,
                                          toShape_costRT_preswit,
                                          toShape_costLogRT_preswit,
                                          toColor_meanACC,
                                          toColorOPP_meanACC,
                                          toColorPRE_meanACC,
                                          toColor_seACC,
                                          toColorOPP_seACC,
                                          toColorPRE_seACC,
                                          toColorCorr_meanRT,
                                          toColorOPPCorr_meanRT,
                                          toColorPRECorr_meanRT,
                                          toColorCorr_seRT,
                                          toColorOPPCorr_seRT,
                                          toColorPRECorr_seRT,
                                          toColorCorr_logMeanRT,
                                          toColorOPPCorr_logMeanRT,
                                          toColorPRECorr_logMeanRT,
                                          toColorCorr_medianRT,
                                          toColorOPPCorr_medianRT,
                                          toColorPRECorr_medianRT,
                                          toColor_costAcc_nonswit,
                                          toColor_costRT_nonswit,
                                          toColor_costLogRT_nonswit,
                                          toColor_costAcc_preswit,
                                          toColor_costRT_preswit,
                                          toColor_costLogRT_preswit,
                                          toColor_costAcc_preShape,
                                          toColor_costRT_preShape,
                                          toColor_costLogRT_preShape)

    #STORE TRIAL LEVEL DATA
    for (j in 1:nrow(testDatTrim)){
      trial_no <- j
      acc <- testDatTrim$key_resp_13.corr[j]
      rt <- testDatTrim$key_resp_13.rt[j]
      if (testDatTrim$cue[j]=="SHAPE"){
        trial_type <- "toShape"
      }
      else if (testDatTrim$nextCue[j]=="SHAPE"){
        trial_type <- "toShapePRE"
      }
      else if (testDatTrim$priorCue[j]=="SHAPE"){
        trial_type <- "toColor"
      }
      else {
        trial_type <- "otherColor"
      }

      trialDat[nrow(trialDat) + 1,] <-c(id, trial_no, acc, rt, trial_type)
    }
  }

#if participant did not have a dccs file, skip to next participant
  else {
  print("Ruh Roh... missing file")
}
}

#write the extracted summary scores to CSV
write.csv(summaryDat,paste(out_path,proc_fileName, sep = "", collapse = NULL), row.names=FALSE)
write.csv(trialDat,paste(out_path,trial_fileName, sep = "", collapse = NULL), row.names=FALSE)
