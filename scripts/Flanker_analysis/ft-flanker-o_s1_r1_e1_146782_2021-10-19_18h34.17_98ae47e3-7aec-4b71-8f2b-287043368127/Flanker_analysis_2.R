library(readr)
library(dplyr)
library(purrr)

#access all the dccs files from a folder (change directory to the one on your computer)
path <- "/Users/arinapolyanskaya/Desktop/Flanker/ft-flanker-o_s1_r1_e1_146782_2021-10-19_18h34.17_98ae47e3-7aec-4b71-8f2b-287043368127/"
files <- list.files(path=path, pattern="*.csv")
summaryDat <- NULL

#create a loop for processing each file in the folder (export and open each file)
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
  
#put file into a dataframe
flanker <- get(gsub(" ", "", substr(file, 1, perpos-1)))

#filter out practice trials 
testDat <- subset(flanker, complete.cases(flanker$test_trials.thisN))

#create a data frame only with correct responses, reaction times, condition, and participant id)
  chosen_cols <- data.frame(testDat[, 17])
  chosen_cols <- cbind(chosen_cols, testDat[, 18])
  chosen_cols <- cbind(chosen_cols, testDat[c("middle", "id")])
  
  #rename columns (keys=the keys that a participant pressed,correct_resp=whether a participant was correct,rt=response time,cue=what was the condition)
  names(chosen_cols) <- c('correct_resp','rt','middle','id')
  
  #Identify the incongruent and congruent trials
  
  #change middle column into a factor with 4 levels 
  chosen_cols$middle <- factor(chosen_cols$middle)
  #Note: first had to identify that testDatTrim$congruent is a factor and what the levels correspond to
  #we now make a new variable "congruent" that is "TRUE" for congruent trials and "FALSE for incongruent trials
  chosen_cols$congruent <- with(chosen_cols, as.numeric(chosen_cols$middle) < 3 )
 
  #subset the data for congruent and incongruent trials, creating new data frames for each
  #note that we take the inverse of "congruent" here to get incongruent
  flanker_con <- chosen_cols[chosen_cols$congruent, ]
  flanker_incon <- chosen_cols[!chosen_cols$congruent, ] 
  
  #compute mean incongruent and congruent accuracy
  con_Acc_mean <- mean(flanker_con$correct_resp)
  incon_Acc_mean <- mean(flanker_incon$correct_resp)
 
  #subset the data for correct trials only, separately for congruent and incongruent trials, creating new data frames for each
  flanker_conCorr <- flanker_con[flanker_con$correct_resp==1, ]
  flanker_inconCorr <- flanker_incon[flanker_incon$correct_resp==1, ]
  
  #For correct trials, compute mean incongruent and congruent RT WITHOUT log scaling
  conCorr_RT_mean <- mean(flanker_conCorr$rt)
  inconCorr_RT_mean <- mean(flanker_inconCorr$rt)
  
  #For correct trials, compute mean incongruent and congruent RT WITH log scaling
  conCorr_logRT_mean <- mean(log((1+flanker_conCorr$rt))) #note that we add 1 first before taking log to avoid negative log values
  inconCorr_logRT_mean <- mean(log((1+flanker_inconCorr$rt))) #note that we add 1 first before taking log to avoid negative log values
  
  #compute flanker-effect scores for accuracy, RT, log-RT
  flankEff_Acc <- incon_Acc_mean-con_Acc_mean
  flankEff_RT <- inconCorr_RT_mean-conCorr_RT_mean
  flankEff_logRT <- inconCorr_logRT_mean-conCorr_logRT_mean
  
  #store data in summary matrix
  id <- flanker$id[1] #create a variable reflecting id of a participant
  
  summaryDat <- rbind(summaryDat, data.frame(id, con_Acc_mean, incon_Acc_mean, conCorr_RT_mean, inconCorr_RT_mean, conCorr_logRT_mean, inconCorr_logRT_mean, flankEff_Acc, flankEff_RT, flankEff_logRT))
}
  # save the results in a file 
  write.csv(summaryDat, 'Flanker_results.csv')