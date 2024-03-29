library(readr)
library(dplyr)
library(purrr)

#access all the dccs files from a folder (change directory to the one on your computer)
path <- "/Users/arinapolyanskaya/Desktop/DCCS/ft-dccs-o_s1_r1_e1~_146779_2021-07-25_16h58.27_87d155a8-ed69-11eb-accb-ac1f6b405aea/raw/"
files <- list.files(path=path, pattern="*.csv")

#create a spaceholder for the results 
results = NULL

#create a loop for processing each file in the folder (export and open each file)
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))

#put file into a dataframe
dccs <- get(gsub(" ", "", substr(file, 1, perpos-1)))

#create a data frame only with correct answers= corrAns, participant responses = key_resp_13.keys,
#correct/incorrect responses = key_resp_13.corr, reaction time = key_resp_13.rt and condition columns = cue)
chosen_cols <- data.frame(dccs[c("id")])
chosen_cols <- cbind(chosen_cols, dccs[, 60])
chosen_cols <- cbind(chosen_cols, dccs[, 61])
chosen_cols <- cbind(chosen_cols, dccs[, 62])
chosen_cols <- cbind(chosen_cols, dccs[, 71])

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
av_same_rt_log <- mean(log(chosen_cols$rt[compare_same]))
av_diff_rt_log <- mean(log(chosen_cols$rt[compare_diff]))

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
results <- rbind(results, data.frame(id, av_same_rt, av_diff_rt, av_same_rt_log, av_diff_rt_log, av_same_corr, av_diff_corr, switch_cost_RT, switch_cost_RT_log, switch_cost_corr))
}

# save the results in a file 
write.csv(results, 'DCCS_results.csv')