# Flanker Preprocessing
# Version: 1
# Authors: George A. Buzzell, Jessica M. Alexander, Arina Polyanskaya
# Last Updated: 2022-02-04

# VARIABLES
# _meanACC : mean accuracy
# _meanRT : mean response time
# _logMeanRT : log-corrected mean response time

#cong : stimulus was congruent with distractor arrows
#incong : stimulus was incongruent with distractor arrows

# flankEff : delta in accuracy or response time between congruent and incongruent trials


#set up root dir for input data
data_path <- '/home/data/NDClab/datasets/'

print("script started!")

# match all pavlovia data
pav_pattern <- "pavlovia"
data_sets <- list.dirs(data_path)
data_sets <- data_sets[ grepl(pav_pattern, data_sets) ]
data_sets <- unique(gsub("\\/.*", "", data_sets))

for(i in 1:length(data_sets)) {
	# get pavlovia dataset
	pav_data <- data_sets[i]
	print("preprocessing ", pav_data)
	#set up output dir and filename
	out_path <- '~/Downloads/'
	proc_fileName <- paste("flanker_subject-level_summary_", Sys.Date(), ".csv", sep="", collapse=NULL)
	trial_fileName <- paste("flanker_trial-level_summary_", Sys.Date(), ".csv", sep="", collapse=NULL)

	#pull out all the subfolders (participant folders) for the root data dir
	sub_folders <- list.files(pav_data, pattern = "sub")

	#create dataframe where we will store summary data
	summaryDat <- data.frame(matrix(ncol = 11, nrow = 0))
	colnames(summaryDat) <- c("id",
							"overall_accuracy",
							"cong_meanACC",
							"incong_meanACC",
							"congCorr_meanRT",
							"incongCorr_meanRT",
							"congCorr_logMeanRT",
							"incongCorr_logMeanRT",
							"flankEff_meanACC",
							"flankEff_meanRT",
							"flankEff_logMeanRT")

	#create dataframe for trial-level information across all participants
	trialDat <- data.frame(matrix(ncol = 5, nrow = 0))
	colnames(trialDat) <- c("id", "trial_no", "acc", "rt", "congruent")

	#loop over participant (subfolders)
	for(i in 1:length(sub_folders)){

		#for this participant, find the flanker csv file
		flanker_file <- list.files(paste(pav_data,sub_folders[i], sep = "", collapse = NULL), pattern = ".*(flanker)+.*(.csv)")

		#logical to make sure there is a flanker file for this participant before loading, else, skip to next participant
		if (!identical(flanker_file, character(0))) {
			print("Woohoo! Processing file!")

			#read in the data for this participant
			flankerDat <- read.csv(file = paste(pav_data,sub_folders[i],'/',flanker_file, sep = "", collapse = NULL), stringsAsFactors = TRUE)
			id <- flankerDat$id[1]

			#remove practice trials and any rows that do not reflect experiment data
			testDat <- subset(flankerDat, complete.cases(flankerDat$test_trials.thisN))
			testDatTrim <- testDat[c("key_resp_2.corr", "key_resp_2.rt", "middle")]

			#calculate overall participant accuracy
			overall_accuracy <- mean(testDatTrim$key_resp_2.corr)

			#create new vector to indicate trial congruency (TRUE=congruent trial, FALSE=incongruent trial)
			testDatTrim$congruent <-  as.numeric(testDatTrim$middle) < 4

			#subset the data for congruent and incongruent trials, creating new data frames for each
			testDatTrim_cong <- testDatTrim[testDatTrim$congruent, ]
			testDatTrim_incong <- testDatTrim[!testDatTrim$congruent, ]

			#compute mean accuracy for congruent and incongruent trials
			cong_meanACC <- mean(testDatTrim_cong$key_resp_2.corr)
			incong_meanACC <- mean(testDatTrim_incong$key_resp_2.corr)

			#subset the data for correct trials only, separately for congruent and incongruent trials, creating new data frames for each
			testDatTrim_congCorr <- testDatTrim_cong[testDatTrim_cong$key_resp_2.corr==1, ]
			testDatTrim_incongCorr <- testDatTrim_incong[testDatTrim_incong$key_resp_2.corr==1, ]

			#for correct trials, compute mean RT (raw and log-corrected)
			congCorr_meanRT <- mean(testDatTrim_congCorr$key_resp_2.rt)
			incongCorr_meanRT <- mean(testDatTrim_incongCorr$key_resp_2.rt)

			congCorr_logMeanRT <- mean(log((1+testDatTrim_congCorr$key_resp_2.rt)))
			incongCorr_logMeanRT <- mean(log((1+testDatTrim_incongCorr$key_resp_2.rt)))

			#compute flanker-effect scores for accuracy, RT, log-RT
			flankEff_meanACC <- incong_meanACC - cong_meanACC
			flankEff_meanRT <- incongCorr_meanRT - congCorr_meanRT
			flankEff_logMeanRT <- incongCorr_logMeanRT - congCorr_logMeanRT

			#STORE OUTPUT DATA IN SUMMARY MATRIX
			summaryDat[nrow(summaryDat) + 1,] <-c(id,
											overall_accuracy,
											cong_meanACC,
											incong_meanACC,
											congCorr_meanRT,
											incongCorr_meanRT,
											congCorr_logMeanRT,
											incongCorr_logMeanRT,
											flankEff_meanACC,
											flankEff_meanRT,
											flankEff_logMeanRT)

			#STORE TRIAL LEVEL DATA
			for (j in 1:nrow(testDatTrim)){ 
				trial_no <- j
				acc <- testDatTrim$key_resp_2.corr[j]
				rt <- testDatTrim$key_resp_2.rt[j]
				congruent <- testDatTrim$congruent[j]

				trialDat[nrow(trialDat) + 1,] <-c(id, trial_no, acc, rt, congruent)
			}
		#if participant did not have a flanker file, skip to next participant
		} else {
			print("Ruh Roh... missing file")
		}
	}
}
#write the extracted summary scores to disk
write.csv(summaryDat,paste(out_path,proc_fileName, sep = "", collapse = NULL), row.names=FALSE)
write.csv(trialDat,paste(out_path,trial_fileName, sep = "", collapse = NULL), row.names=FALSE)
