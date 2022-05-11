# DCCS Preprocessing
# Last Updated: 2021-12-14

# VARIABLES
# _acc_mean : mean accuracy
# _acc_se : standard error of mean accuracy
# _meanRT : mean response time
# _seRT : standard error of mean response time
# _logRT_mean : log-corrected mean response time
# _medianRT : median response time
# _logMedianRT : log-corrected median response time
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



library(readr)
library(dplyr)
library(purrr)
library(plotrix)
library(lsr)

#access all the dccs files from a folder (change directory to the one on your computer)
#set up root dir for data to be processed
data_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'

#set up output dir and file name
out_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'
proc_fileName <- paste("dccs_behavior_summary_", Sys.Date(), ".csv", sep="", collapse=NULL)

#pull out all the subfolders (participant folders) for the root data dir
sub_folders <- list.files(data_path, pattern = "sub")

#create a spaceholder for the results 
resultsDCCS <- setNames(data.frame(matrix(ncol = 65, nrow = 0)), c("id",
                                                                   "overall_accuracy",
                                                                   "anySwitch_acc_mean",
                                                                   "anySwitchOPP_acc_mean",
                                                                   "anySwitch_acc_se",
                                                                   "anySwitchOPP_acc_se",
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
                                                                   "toShape_acc_mean",
                                                                   "toShapeOPP_acc_mean",
                                                                   "toShapePRE_acc_mean",
                                                                   "toShape_acc_se",
                                                                   "toShapeOPP_acc_se",
                                                                   "toShapePRE_acc_se",
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
                                                                   "toColor_acc_mean",
                                                                   "toColorOPP_acc_mean",
                                                                   "toColorPRE_acc_mean",
                                                                   "toColor_acc_se",
                                                                   "toColorOPP_acc_se",
                                                                   "toColorPRE_acc_se",
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
                                                                   "toColor_costLogRT_preswit"))

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
    
    #create a data frame only with correct answers= corrAns, participant responses = key_resp_13.keys,
    #correct/incorrect responses = key_resp_13.corr, reaction time = key_resp_13.rt and condition columns = cue)
    chosen_cols <- data.frame(dccsDat[c("id")])
    chosen_cols <- cbind(chosen_cols, dccsDat[, 60])
    chosen_cols <- cbind(chosen_cols, dccsDat[, 61])
    chosen_cols <- cbind(chosen_cols, dccsDat[, 62])
    chosen_cols <- cbind(chosen_cols, dccsDat[, 71])
    
    #filter out NA fields
    chosen_cols <- na.omit(chosen_cols)
    
    #rename columns (keys=the keys that a participant pressed,correct_resp=whether a participant was correct,rt=response time,cue=what was the condition)
    names(chosen_cols) <- c('participant', 'keys','correct_resp', 'rt', 'cue')
    #make row index order begin at 1
    rownames(chosen_cols) <- 1:nrow(chosen_cols)
    
    #if participant response accuracy is lower than 80% then the participant should not be counted
    #calculate overall participant accuracy
    overall_accuracy <- mean(chosen_cols$correct_resp)
    
    #proceed with further scoring only if participant >80% accurate
    if (overall_accuracy < 0.8){
      print("Participant accuracy too low, moving to the next!")
    } else {
   
       # to find general switch between conditions create two vectors, 
    #first removing the last condition, and second removing the first condition
    condition_list1 <- (chosen_cols$cue[-length(chosen_cols$cue)])
    condition_list2 <- (chosen_cols$cue[-1])
    
    #create two df that compare first condition list to the second one with TRUE and FALSE values 
    #(true=same, false+different and reverse)
    compare_same <- data.frame(condition_list1 == condition_list2)
    compare_same <- rbind(c(NA), compare_same) #add NA to the first row since it doesn't count
    compare_same <- cbind(chosen_cols, compare_same)
    compare_same <- na.omit(compare_same)
    colnames(compare_same)[6] <- "cond_same"
    same_list <- compare_same[compare_same$cond_same==TRUE, ]
    diff_list <- compare_same[compare_same$cond_same==FALSE, ]
    
   
    #GENERAL SWITCH
    #find average for accuracy for the same or different trials
    anySwitchOPP_acc_mean <- mean(same_list$correct_resp)
    anySwitchOPP_acc_se <- std.error(same_list$correct_resp)
    anySwitch_acc_mean <- mean(diff_list$correct_resp)
    anySwitch_acc_se <- std.error(diff_list$correct_resp)
    
    #subset the data for correct trials only, separately for different and same condition trials, creating new data frames for each
    testDatTrim_sameCorr <- same_list[same_list$correct_resp==1, ]
    testDatTrim_diffCorr <- diff_list[diff_list$correct_resp==1, ]
    
    #convert true values into rt, find its average
    anySwitchOPPCorr_meanRT <- mean(testDatTrim_sameCorr$rt)
    anySwitchOPPCorr_seRT <- std.error(testDatTrim_sameCorr$rt)
    anySwitchCorr_meanRT <- mean(testDatTrim_diffCorr$rt)
    anySwitchCorr_seRT <- std.error(testDatTrim_diffCorr$rt)
    anySwitchCorr_medianRT <- median(testDatTrim_diffCorr$rt)
    anySwitchOPPCorr_medianRT <- median(testDatTrim_sameCorr$rt)
    anySwitchCorr_logMedianRT <- median(log(1+testDatTrim_diffCorr$rt))
    anySwitchOPPCorr_logMedianRT <- median(log(1+testDatTrim_sameCorr$rt))
    
    #convert true values into rt,find its log and find its average
    anySwitchOPPCorr_logMeanRT <- mean(log(1+testDatTrim_sameCorr$rt)) #note that we add 1 first before taking log to avoid negative log values
    anySwitchCorr_logMeanRT <- mean(log(1+testDatTrim_diffCorr$rt)) #note that we add 1 first before taking log to avoid negative log values
    
    
    #to SHAPE switch
    #find when the condition switches COLOR -> SHAPE
    toShape <- diff_list[diff_list$cue == "SHAPE", ]
    #find RT and accuracy for COLOR->SHAPE switch
    toShape_acc_mean <- mean(toShape$correct_resp)
    toShape_acc_se <- std.error(toShape$correct_resp)
    toShape_resp <- toShape[toShape$correct_resp==1, ] 
    toShapeCorr_meanRT <- mean(toShape_resp$rt)
    toShapeCorr_seRT <- std.error(toShape_resp$rt)
    toShapeCorr_logMeanRT <- mean(log(1+toShape_resp$rt))
    toShapeCorr_medianRT <- median(toShape_resp$rt)
   
    #find the opposite of shape switch (everything that is not shape)
    toShapeOPP <- chosen_cols[chosen_cols$cue != "SHAPE", ]
    toShapeOPP_acc_mean <- mean(toShapeOPP$correct_resp)
    toShapeOPP_acc_se <- std.error(toShapeOPP$correct_resp)
    toShapeOPP_resp <- toShapeOPP[toShapeOPP$correct_resp==1, ] 
    toShapeOPPCorr_meanRT <- mean(toShapeOPP_resp$rt)
    toShapeOPPCorr_seRT <- std.error(toShapeOPP_resp$rt)
    toShapeOPPCorr_logMeanRT <- mean(log(1+toShapeOPP_resp$rt))
    toShapeOPPCorr_medianRT <- median(toShapeOPP_resp$rt)
    
    
    #find previous to shape switch
    
    #create lists of rt and accuracy values
    trial_length <- nrow(chosen_cols)
    preshift_shape_acc <- vector(mode = 'list')
    preshiftAcc_shape_rts <- vector(mode = 'list')
    
    #create a loop that compares current trial with a previous trial
    for(trial in 2:trial_length){
      previous_cue <- chosen_cols$cue[trial-1]
      current_cue <- chosen_cols$cue[trial]
     
       #if current and previous trials are shifting from color to shape then keep the values of accuracy
      if (previous_cue == "COLOR" & current_cue == "SHAPE"){
        
        # correct response (cr) variables
        previous_cr <- chosen_cols$correct_resp[trial-1]
        current_cr <- chosen_cols$correct_resp[trial]
        preshift_shape_acc <- append(preshift_shape_acc, previous_cr)
        
        #if the response is correct for previous and current trials then keep RT for PRE trial in a list
        if (previous_cr == 1 & current_cr == 1){
          
          # reaction time (rt) 
          previous_rt <- chosen_cols$rt[trial - 1]
          current_rt <- chosen_cols$rt[trial]
          
          # append rts to lists
          preshiftAcc_shape_rts <- append(preshiftAcc_shape_rts, previous_rt)
        }
      }
    }
    #Find the mean, standard error of accuracy and rt 
    toShapePRE_acc_mean <- mean(as.numeric(preshift_shape_acc))
    toShapePRE_acc_se <- std.error(as.numeric(preshift_shape_acc))
    toShapePRECorr_meanRT <- mean(as.numeric(preshiftAcc_shape_rts))
    toShapePRECorr_seRT <- std.error(as.numeric(preshiftAcc_shape_rts))
    toShapePRECorr_logMeanRT <- mean(log(1+as.numeric(preshiftAcc_shape_rts)))
    toShapePRECorr_medianRT <- median(as.numeric(preshiftAcc_shape_rts))
    
    
    #to COLOR switch
    #find RT and accuracy for SHAPE->COLOR switch
    toColor <- diff_list[diff_list$cue != "SHAPE", ]
    
    #find RT and accuracy for SHAPE->COLOR switch
    toColor_acc_mean <- mean(toColor$correct_resp)
    toColor_acc_se <- std.error(toColor$correct_resp)
    shapecol_resp <- toColor[toColor$correct_resp==1, ] 
    toColorCorr_meanRT <- mean(shapecol_resp$rt)
    toColorCorr_seRT <- std.error(shapecol_resp$rt)
    toColorCorr_logMeanRT <- mean(log(1+shapecol_resp$rt))
    toColorCorr_medianRT <- median(shapecol_resp$rt)
    
    #find RT and accuracy for opposite of SHAPE -> COLOR switch
    toColorOPP <- rbind(toShape, same_list)
    toColorOPP_acc_mean <- mean(toColorOPP$correct_resp)
    toColorOPP_acc_se <- std.error(toColorOPP$correct_resp)
    toColorOPP_resp <- toColorOPP[toColorOPP$correct_resp==1, ] 
    toColorOPPCorr_meanRT <- mean(toColorOPP_resp$rt)
    toColorOPPCorr_seRT <- std.error(toColorOPP_resp$rt)
    toColorOPPCorr_logMeanRT <- mean(log(1+toColorOPP_resp$rt))
    toColorOPPCorr_medianRT <- median(toColorOPP$rt)
    
    #create lists of rt and accuracy values for shift to color
    preshift_color_acc <- vector(mode = 'list')
    preshiftAcc_color_rts <- vector(mode = 'list')
    
    #create a loop that compares current trial with a previous trial (-1)
    for(trial in 2:trial_length){
      previous_cue <- chosen_cols$cue[trial-1]
      current_cue <- chosen_cols$cue[trial]
      
      #if current and previous trials are shifting from SHAPE to COLOR then keep the values of accuracy
      if (previous_cue == "SHAPE" & current_cue == "COLOR"){
        
        # correct response (cr) variables for the current trial and two trials before (-2)
        previous_cr <- chosen_cols$correct_resp[trial-2]
        current_cr <- chosen_cols$correct_resp[trial]
        preshift_color_acc <- append(preshift_color_acc, previous_cr)
        
        #if the response is correct for previous (-2) and current trials then keep RT for PRE trial in a list
        if (previous_cr == 1 & current_cr == 1){
          
          # reaction time (rt) 
          previous_rt <- chosen_cols$rt[trial - 2]
          current_rt <- chosen_cols$rt[trial]
          
          # append rts to lists
          preshiftAcc_color_rts <- append(preshiftAcc_color_rts, previous_rt)
        }
      }
    }
    #Find the mean, standard error of accuracy and rt 
    toColorPRE_acc_mean <- mean(as.numeric(preshift_color_acc))
    toColorPRE_acc_se <- std.error(as.numeric(preshift_color_acc))
    toColorPRECorr_meanRT <- mean(as.numeric(preshiftAcc_color_rts))
    toColorPRECorr_seRT <- std.error(as.numeric(preshiftAcc_color_rts))
    toColorPRECorr_logMeanRT <- mean(log(1+as.numeric(preshiftAcc_color_rts)))
    toColorPRECorr_medianRT <- median(as.numeric(preshiftAcc_color_rts))
    
    
    
     #FINDING THE COST OF SWITHES  
    
    #find the cost of the general switch (same vs different reaction times)
    anySwitch_costRT <- anySwitchCorr_meanRT - anySwitchOPPCorr_meanRT
    anySwitch_costLogRT <- anySwitchCorr_logMeanRT - anySwitchOPPCorr_logMeanRT
    
    #find the cost of the general switch (same vs different accuracy)
    anySwitch_costAcc <-  anySwitch_acc_mean - anySwitchOPP_acc_mean
    
    
    #SWITCH TO SHAPE TRIALS 
    #find the cost of the shape switch (shape switch vs everything else) RT
    toShape_costRT_nonswit <- toShapeCorr_meanRT - toShapeOPPCorr_meanRT
    toShape_costLogRT_nonswit <- toShapeCorr_logMeanRT - toShapeOPPCorr_logMeanRT
    
   
    #find mean rt between switch to shape and previous trial
    toShape_costRT_preswit <- toShapeCorr_meanRT - toShapePRECorr_meanRT
    toShape_costLogRT_preswit <- toShapeCorr_logMeanRT - toShapePRECorr_logMeanRT
    
    #find the cost of the shape switch (shape switch vs everything else) accuracy
    toShape_costAcc_nonswit <-  toShape_acc_mean - toShapeOPP_acc_mean

    #Shape switch trials vs preswitch trials accuracy
    toShape_costAcc_preswit <- toShape_acc_mean - toShapePRE_acc_mean 
    
    
    #SWITCH TO COLOR TRIALS
    #find the cost of the color switch (color switch vs everything else) RT
    toColor_costRT_nonswit <- toColorCorr_meanRT - toColorOPPCorr_meanRT
    toColor_costLogRT_nonswit <- toColorCorr_logMeanRT - toColorOPPCorr_logMeanRT
    
    #find mean rt between switch to shape and previous trial
    toColor_costRT_preswit <- toColorCorr_meanRT - toColorPRECorr_meanRT
    toColor_costLogRT_preswit <- toColorCorr_logMeanRT - toColorPRECorr_logMeanRT
    
    #find the cost of the color switch (color switch vs everything else) accuracy
    toColor_costAcc_nonswit <- toColor_acc_mean - toColorOPP_acc_mean
    
    #Color switch trials vs preswitch trials accuracy
    toColor_costAcc_preswit <- toColor_acc_mean - toColorPRE_acc_mean 
    
    
    #create a dataframe that captures all of the outcomes from each dataframe
    id <- chosen_cols[1, 1] #create a variable reflecting id of a participant
    resultsDCCS[nrow(resultsDCCS) + 1,] <-c(id,
                                            overall_accuracy,
                                            anySwitch_acc_mean,
                                            anySwitchOPP_acc_mean,
                                            anySwitch_acc_se,
                                            anySwitchOPP_acc_se,
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
                                            toShape_acc_mean,
                                            toShapeOPP_acc_mean,
                                            toShapePRE_acc_mean,
                                            toShape_acc_se,
                                            toShapeOPP_acc_se,
                                            toShapePRE_acc_se,
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
                                            toColor_acc_mean,
                                            toColorOPP_acc_mean,
                                            toColorPRE_acc_mean,
                                            toColor_acc_se,
                                            toColorOPP_acc_se,
                                            toColorPRE_acc_se,
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
                                            toColor_costLogRT_preswit)
    
  }
  
  #if participant did not have a dccs file, skip to next participant
} else {
  print("Ruh Roh... missing file")
  }
}


# save the results in a file 
write.csv(resultsDCCS, paste(out_path,proc_fileName, sep = "", collapse = NULL), row.names=FALSE)


#QUICK STATS
anySwitch_acc_mean_ttest <- t.test(resultsDCCS$anySwitch_acc_mean, resultsDCCS$anySwitchOPP_acc_mean, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
anySwitch_meanRT_ttest <- t.test(resultsDCCS$anySwitchCorr_meanRT, resultsDCCS$anySwitchOPPCorr_meanRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
anySwitch_logMeanRT_ttest <- t.test(resultsDCCS$anySwitchCorr_logMeanRT, resultsDCCS$anySwitchOPPCorr_logMeanRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
anySwitch_medianRT_ttest <- t.test(resultsDCCS$anySwitchCorr_medianRT, resultsDCCS$anySwitchOPPCorr_medianRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)

toShape_nonswit_acc_mean_ttest <- t.test(resultsDCCS$toShape_acc_mean, resultsDCCS$toShapeOPP_acc_mean, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toShape_preswit_acc_mean_ttest <- t.test(resultsDCCS$toShape_acc_mean, resultsDCCS$toShapePRE_acc_mean, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toShape_nonswit_meanRT_ttest <- t.test(resultsDCCS$toShapeCorr_meanRT, resultsDCCS$toShapeOPPCorr_meanRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toShape_preswit_meanRT_ttest <- t.test(resultsDCCS$toShapeCorr_meanRT, resultsDCCS$toShapePRECorr_meanRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toShape_nonswit_logMeanRT_ttest <- t.test(resultsDCCS$toShapeCorr_logMeanRT, resultsDCCS$toShapeOPPCorr_logMeanRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toShape_preswit_logMeanRT_ttest <- t.test(resultsDCCS$toShapeCorr_logMeanRT, resultsDCCS$toShapePRECorr_logMeanRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toShape_nonswit_medianRT_ttest <- t.test(resultsDCCS$toShapeCorr_medianRT, resultsDCCS$toShapeOPPCorr_medianRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toShape_preswit_medianRT_ttest <- t.test(resultsDCCS$toShapeCorr_medianRT, resultsDCCS$toShapePRECorr_medianRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)

toColor_nonswit_acc_mean_ttest <- t.test(resultsDCCS$toColor_acc_mean, resultsDCCS$toColorOPP_acc_mean, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toColor_preswit_acc_mean_ttest <- t.test(resultsDCCS$toColor_acc_mean, resultsDCCS$toColorPRE_acc_mean, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toColor_nonswit_meanRT_ttest <- t.test(resultsDCCS$toColorCorr_meanRT, resultsDCCS$toColorOPPCorr_meanRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toColor_preswit_meanRT_ttest <- t.test(resultsDCCS$toColorCorr_meanRT, resultsDCCS$toColorPRECorr_meanRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toColor_nonswit_logMeanRT_ttest <- t.test(resultsDCCS$toColorCorr_logMeanRT, resultsDCCS$toColorOPPCorr_logMeanRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toColor_preswit_logMeanRT_ttest <- t.test(resultsDCCS$toColorCorr_logMeanRT, resultsDCCS$toColorPRECorr_logMeanRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toColor_nonswit_medianRT_ttest <- t.test(resultsDCCS$toColorCorr_medianRT, resultsDCCS$toColorOPPCorr_medianRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)
toColor_preswit_medianRT_ttest <- t.test(resultsDCCS$toColorCorr_medianRT, resultsDCCS$toColorPRECorr_medianRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)

toColor_preShape_meanRT_ttest <- t.test(resultsDCCS$toColorCorr_meanRT, resultsDCCS$toShapePRECorr_meanRT, alternative = c("two.sided"), paired=FALSE, conf.level=0.95)


stat_summary <- data.frame(matrix(ncol = 21, nrow = 0))
stat_summary[nrow(stat_summary) + 1,] <- c(anySwitch_acc_mean_ttest$p.value, #1
                                           anySwitch_meanRT_ttest$p.value, #2
                                           anySwitch_logMeanRT_ttest$p.value, #3
                                           anySwitch_medianRT_ttest$p.value, #4
                                           toShape_nonswit_acc_mean_ttest$p.value, #5
                                           toShape_preswit_acc_mean_ttest$p.value, #6
                                           toShape_nonswit_meanRT_ttest$p.value, #7
                                           toShape_preswit_meanRT_ttest$p.value, #8
                                           toShape_nonswit_logMeanRT_ttest$p.value, #9
                                           toShape_preswit_logMeanRT_ttest$p.value, #10
                                           toShape_nonswit_medianRT_ttest$p.value, #11
                                           toShape_preswit_medianRT_ttest$p.value, #12
                                           toColor_nonswit_acc_mean_ttest$p.value, #13
                                           toColor_preswit_acc_mean_ttest$p.value, #14
                                           toColor_nonswit_meanRT_ttest$p.value, #15
                                           toColor_preswit_meanRT_ttest$p.value, #16
                                           toColor_nonswit_logMeanRT_ttest$p.value, #17
                                           toColor_preswit_logMeanRT_ttest$p.value, #18
                                           toColor_nonswit_medianRT_ttest$p.value, #19
                                           toColor_preswit_medianRT_ttest$p.value, #20
                                           toColor_preShape_meanRT_ttest$p.value) #21