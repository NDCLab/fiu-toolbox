library(readr)
library(dplyr)

#access all the dccs files from a folder
raw.files <- data_frame(filename = list.files('/Users/arinapolyanskaya/Desktop/DCCS/ft-dccs-o_s1_r1_e1~_146779_2021-07-25_16h58.27_87d155a8-ed69-11eb-accb-ac1f6b405aea/'))
raw.file.paths <- raw.files  %>%
  mutate(filepath = paste0("/Users/arinapolyanskaya/Desktop/DCCS/ft-dccs-o_s1_r1_e1~_146779_2021-07-25_16h58.27_87d155a8-ed69-11eb-accb-ac1f6b405aea/", filename))

read.csv.and.add.filename <- function(filepath){
  read_csv(filepath) %>%
    mutate(filepath=filepath)
}
raw.file.paths %>%
  head(3)

read.csv.and.add.filename <- function(filepath){
  read_csv(filepath) %>%
    mutate(filepath=filepath)
}
raw.data.with.paths <- raw.file.paths %>%
  rowwise() %>%
  do(., read.csv.and.add.filename(.$filepath))

raw.data.with.paths %>%
  sample_n(10) %>%
  pander()

#export and open the file
dccs <- read_csv('12_ft-dccs-o_s1_r1_e1_2021-07-22_12h56.39.604.csv')
#create a data frame only with correct answers= corrAns, participant responses = key_resp_13.keys,
#correct/incorrect responses = key_resp_13.corr, reaction time = key_resp_13.rt and condition columns = cue)
chosen_cols <- dccs[c("corrAns","key_resp_13.keys","key_resp_13.corr","key_resp_13.rt","cue")]
#sort out N/A spaces 
sorted_cols <- chosen_cols[complete.cases(chosen_cols[c("corrAns","key_resp_13.keys","key_resp_13.corr","key_resp_13.rt","cue")]), ]
#str(sorted_cols)
 #create two vectors, first removing the last condition, and second removing the first condition
condition_list1 <- (sorted_cols$cue[-length(sorted_cols$cue)])
condition_list2 <- (sorted_cols$cue[-1])
print (condition_list1)
print (condition_list2)
#create a list that compares first condition list to the second one with TRUE and FALSE values (true=same, false+different and reverse)
compare_same <- c(condition_list1 == condition_list2)
compare_diff <- c(condition_list1 != condition_list2)
#convert true values into rt, find its average
av_same_rt <- mean(sorted_cols$key_resp_13.rt[compare_same])
av_diff_rt <- mean(sorted_cols$key_resp_13.rt[compare_diff])
#find the difference between same vs different reaction times
switch_cost_RT <- av_same_rt - av_diff_rt

#find average for accuracy for the same or different trials
av_same_corr <- mean(sorted_cols$key_resp_13.corr[compare_same])
av_diff_corr <- mean(sorted_cols$key_resp_13.corr[compare_diff])
#find the difference between same vs different accuracy
switch_cost_corr <- av_same_corr - av_diff_corr

#create a dataframe and save it to the file
#dccs_analys <- data.frame(switch_cost_RT, switch_cost_corr)
#save(dccs_analys, file='DCCS_analys_1a.csv')

