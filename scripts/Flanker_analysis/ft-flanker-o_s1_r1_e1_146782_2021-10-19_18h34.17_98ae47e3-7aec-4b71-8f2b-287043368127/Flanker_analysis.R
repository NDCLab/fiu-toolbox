library(readr)
library(dplyr)
library(purrr)

#access all the dccs files from a folder (change directory to the one on your computer)
#path <- "/Users/arinapolyanskaya/Desktop/Flanker/ft-flanker-o_s1_r1_e1_146782_2021-10-19_18h34.17_98ae47e3-7aec-4b71-8f2b-287043368127"
#files <- list.files(path=path, pattern="*.csv")

#create a spaceholder for the results 
#results = NULL

#create a loop for processing each file in the folder (export and open each file)
#for(file in files)
#{
#  perpos <- which(strsplit(file, "")[[1]]==".")
#  assign(
#    gsub(" ","",substr(file, 1, perpos-1)), 
#    read.csv(paste(path,file,sep="")))
  
  #put file into a dataframe
#  flanker <- get(gsub(" ", "", substr(file, 1, perpos-1)))
 
flanker <- read_csv('2_ft-flanker-o_s1_r1_e1_2021-07-02_09h20.17.596.csv') 
  #create a data frame only with correct answers= corrAns, participant responses = key_resp_13.keys,
  #correct/incorrect responses = key_resp_13.corr, reaction time = key_resp_13.rt and condition columns = cue)
  chosen_cols <- data.frame(flanker[, 17])
  chosen_cols <- cbind(chosen_cols, flanker[, 18])
  chosen_cols <- cbind(chosen_cols, flanker[, 20])
  chosen_cols <- cbind(chosen_cols, flanker[c("middle", "id")])
  
  #filter out practice trials 
  #test_trials <- subset(chosen_cols, trials.thisTrialN != 'NA')
  #chosen_cols[trials.thisTrialN(chosen_cols) == NA]
  
  #filter out NA fields
  chosen_cols <- na.omit(chosen_cols)
 
  #create congruent vector by adding the values from middle column = img/congruent_right.png or img/congruent_left.png
  #congruent <- subset(chosen_cols[c("middle"=="img/congruent_right.png")])
 
  #create incongruent vector by adding the values from middle column = img/incongruent_right.png or img/incongruent_left.png
  
  
  # find average of congruent trials for accuracy
  
  # find average of congruent trials for Reaction time:
  # 1) Find log+average
  
  # 2) Find average
  
  
  # find average of incongruent trials for accuracy
  
  # find average of incongruent trials for Reaction time:
  # 1) Find log+average
  
  # 2) Find average
  
  
  #Find difference between the avegare values (incongruent-congruent)
  
  #create a table
  
  #save the table into scv file