library(readr)
library(dplyr)
library(purrr)

#access all the dccs files from a folder (change directory to the one on your computer)
#set up root dir for data to be processed
data_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'

#set up output dir and file name
out_path <- '/Users/arinapolyanskaya/Desktop/example_pavlovia_dat/pavlovia/'
proc_fileName <- "2021-11-05_dccs_behavior_summary.txt"

#pull out all the subfolders (participant folders) for the root data dir
sub_folders <- list.files(data_path, pattern = "sub")

#create a spaceholder for the results 
resultsDCCS <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("id", "av_same_rt", "av_diff_rt", "av_same_rt_log", "av_diff_rt_log", "av_same_corr", "av_diff_corr", "switch_cost_RT", "switch_cost_RT_log", "switch_cost_corr"))

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

    #create two vectors, first removing the last condition, and second removing the first condition
    condition_list1 <- (chosen_cols$cue[-length(chosen_cols$cue)])
    condition_list2 <- (chosen_cols$cue[-1])

    #create a list that compares first condition list to the second one with TRUE and FALSE values (true=same, false+different and reverse)
    compare_same <- c(condition_list1 == condition_list2)
    compare_diff <- c(condition_list1 != condition_list2)

    #convert true values into rt, find its average
    av_same_rt <- mean(chosen_cols$rt[compare_same])
    av_diff_rt <- mean(chosen_cols$rt[compare_diff])

    #convert true values into rt,find its log and find its average
    av_same_rt_log <- mean(log(1+chosen_cols$rt[compare_same])) #note that we add 1 first before taking log to avoid negative log values
    av_diff_rt_log <- mean(log(1+chosen_cols$rt[compare_diff])) #note that we add 1 first before taking log to avoid negative log values

    #find the difference between same vs different reaction times
    switch_cost_RT <- av_same_rt - av_diff_rt
    switch_cost_RT_log <- av_same_rt_log - av_diff_rt_log

    #find average for accuracy for the same or different trials
    av_same_corr <- mean(chosen_cols$correct_resp[compare_same])
    av_diff_corr <- mean(chosen_cols$correct_resp[compare_diff])

    #find the difference between same vs different accuracy
    switch_cost_corr <- av_same_corr - av_diff_corr

    #create a dataframe that captures all of the outcomes from each dataframe
    id <- chosen_cols[1, 1] #create a variable reflecting id of a participant
    resultsDCCS[nrow(resultsDCCS) + 1,] <-c(id, av_same_rt, av_diff_rt, av_same_rt_log, av_diff_rt_log, av_same_corr, av_diff_corr, switch_cost_RT, switch_cost_RT_log, switch_cost_corr)
  } else {
    print("Ruh Roh... missing file")
  }
}  
# save the results in a file 
write.csv(resultsDCCS, paste(out_path,proc_fileName, sep = "", collapse = NULL), row.names=FALSE)
