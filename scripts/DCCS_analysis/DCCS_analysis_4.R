library(readr)
library(dplyr)
library(purrr)
library(plotrix)

#access all the dccs files from a folder (change directory to the one on your computer)
#set up root dir for data to be processed
data_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'

#set up output dir and file name
out_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'
proc_fileName <- "2021-11-05_dccs_behavior_summary.csv"

#pull out all the subfolders (participant folders) for the root data dir
sub_folders <- list.files(data_path, pattern = "sub")

#create a spaceholder for the results 
resultsDCCS <- setNames(data.frame(matrix(ncol = 19, nrow = 0)), c("id", "colshape_corr_av", "colshape_corr_se", "colshape_rt_av",
              "colshape_rt_log_av","colshape_rt_se","av_same_rt", "se_same_rt", "av_diff_rt", "se_same_rt", "av_same_rt_log", "av_diff_rt_log", "av_same_corr","se_same_corr",
              "av_diff_corr", "se_diff_corr", "switch_cost_RT", "switch_cost_RT_log", "switch_cost_corr"))

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
    
   
    #find average for accuracy for the same or different trials
    av_same_corr <- mean(same_list$correct_resp)
    se_same_corr <- std.error(same_list$correct_resp)
    av_diff_corr <- mean(diff_list$correct_resp)
    se_diff_corr <- std.error(diff_list$correct_resp)
    
    #subset the data for correct trials only, separately for different and same condition trials, creating new data frames for each
    testDatTrim_sameCorr <- same_list[same_list$correct_resp==1, ]
    testDatTrim_diffCorr <- diff_list[diff_list$correct_resp==1, ]
    
    #convert true values into rt, find its average
    av_same_rt <- mean(testDatTrim_sameCorr$rt)
    se_same_rt <- std.error(testDatTrim_sameCorr$rt)
    av_diff_rt <- mean(testDatTrim_diffCorr$rt)
    se_diff_rt <- std.error(testDatTrim_diffCorr$rt)
    
    #convert true values into rt,find its log and find its average
    av_same_rt_log <- mean(log(1+testDatTrim_sameCorr$rt)) #note that we add 1 first before taking log to avoid negative log values
    av_diff_rt_log <- mean(log(1+testDatTrim_diffCorr$rt)) #note that we add 1 first before taking log to avoid negative log values
    
    
    #find when the condition switches COLOR -> SHAPE
    col_shape <- diff_list[diff_list$cue == "SHAPE", ]
    #find RT and accuracy for COLOR->SHAPE switch
    colshape_corr_av <- mean(col_shape$correct_resp)
    colshape_corr_se <- std.error(col_shape$correct_resp)
    colshape_resp <- col_shape[col_shape$correct_resp==1, ] 
    colshape_rt_av <- mean(colshape_resp$rt)
    colshape_rt_se <- std.error(colshape_resp$rt)
    colshape_rt_log_av <- mean(log(1+colshape_resp$rt))
    
    
    #find RT and accuracy for SHAPE->COLOR switch
    shape_col <- diff_list[diff_list$cue != "SHAPE", ]
    #find RT and accuracy for COLOR->SHAPE switch
    shapecol_corr_av <- mean(shape_col$correct_resp)
    shapecol_corr_se <- std.error(shape_col$correct_resp)
    shapecol_resp <- shape_col[shape_col$correct_resp==1, ] 
    shapecol_rt_av <- mean(shapecol_resp$rt)
    shapecol_rt_se <- std.error(shapecol_resp$rt)
    shapecol_rt_log_av <- mean(log(1+shapecol_resp$rt))
    
    
    #find the difference between same vs different reaction times
    switch_cost_RT <- av_diff_rt - av_same_rt
    switch_cost_RT_log <- av_diff_rt_log - av_same_rt_log
    
    #find the difference between same vs different accuracy
    switch_cost_corr <-  av_diff_corr - av_same_corr
    
    #create a dataframe that captures all of the outcomes from each dataframe
    id <- chosen_cols[1, 1] #create a variable reflecting id of a participant
    resultsDCCS[nrow(resultsDCCS) + 1,] <-c(id, colshape_corr_av, colshape_corr_se, colshape_rt_av,
                                            colshape_rt_log_av, colshape_rt_se, av_same_rt, se_same_rt, av_diff_rt, se_same_rt, av_same_rt_log, av_diff_rt_log, av_same_corr, se_same_corr,
                                            av_diff_corr, se_diff_corr, switch_cost_RT, switch_cost_RT_log, switch_cost_corr)
    print("Ruh Roh... missing file")
    }
}
  

# save the results in a file 
write.csv(resultsDCCS, paste(out_path,proc_fileName, sep = "", collapse = NULL), row.names=FALSE)

library(lsr)
t.test(resultsDCCS$av_diff_corr, resultsDCCS$av_same_corr, alternative = "two.sided", paired = TRUE)
