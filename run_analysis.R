library(dplyr)
#set data path
dataPath <- "./data"

#download zip file
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "data.zip")
if(!file.exists(dataPath)){
        dir.create(dataPath)        
}
unzip("data.zip", exdir = dataPath, overwrite = TRUE)

#################################################################
#1. Merges the training and the test sets to create one data set.
#################################################################

fixwidthlength <- 16

#load the feature names into a vector
featureNames <- read.csv( paste(dataPath, "/features.txt", sep=""), sep = " ", stringsAsFactors = FALSE, header = FALSE)[ ,2]
#define the columns class
trainrawsample <- read.fwf(paste(dataPath, "/train/X_train.txt", sep=""), widths = rep.int(fixwidthlength, length(featureNames)), stringsAsFactors = FALSE, nrows = 5, header = FALSE)
classes <- sapply(trainrawsample, class)
#load the train data and set the variable names
trainraw <- read.fwf(paste(dataPath, "/train/X_train.txt", sep=""), widths = rep.int(fixwidthlength, length(featureNames)), stringsAsFactors = FALSE, header = FALSE, colClasses = classes, col.names = featureNames)
#load labels for train data
trainlabels <-read.csv(paste(dataPath, "/train/Y_train.txt", sep=""), stringsAsFactors = FALSE, header = FALSE, colClasses = "numeric", col.names = "activityid")
#add a column with activityid for train data
trainraw$activityid <- trainlabels$activityid
#load subject for train data
trainsubjects <-read.csv(paste(dataPath, "/train/subject_train.txt", sep=""), stringsAsFactors = FALSE, header = FALSE, colClasses = "numeric", col.names = "subject")
#add a column with subject for train data
trainraw$subject <- trainsubjects$subject

#load the test data and set the variable names
testraw <- read.fwf(paste(dataPath, "/test/X_test.txt", sep=""), widths = rep.int(fixwidthlength, length(featureNames)), stringsAsFactors = FALSE, header = FALSE, colClasses = classes, col.names = featureNames)
#load labels for test data
testlabels <- read.csv(paste(dataPath, "/test/Y_test.txt", sep=""), stringsAsFactors = FALSE, header = FALSE, colClasses = "numeric", col.names = "activityid")
#add a column with activityid for test data
testraw$activityid <- testlabels$activityid
#load subject for test data
testsubjects <-read.csv(paste(dataPath, "/test/subject_test.txt", sep=""), stringsAsFactors = FALSE, header = FALSE, colClasses = "numeric", col.names = "subject")
#add a column with subject for test data
testraw$subject <- testsubjects$subject

#merge train & test data
dsraw <- rbind(trainraw, testraw)

#remove data from memory
rm(classes, trainrawsample, trainraw, trainlabels, trainsubjects, testraw, testlabels, testsubjects)

###########################################################################################
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
###########################################################################################

#filter the coloms by name wher contains mean. or std.
cols <- grepl("(mean|std)", names(dsraw))
#force to mantain the last two columns activityid and subject
cols[562:563] <- TRUE
#remove the columns
dsraw <- dsraw[ , cols]


##########################################################################
#3. Uses descriptive activity names to name the activities in the data set
##########################################################################

#load activity labels
activities <- read.csv(paste(dataPath, "/activity_labels.txt", sep=""), sep = " ", stringsAsFactors = TRUE, header = FALSE, col.names = c("activityid","activity"))
dsraw <- merge(dsraw, activities, by="activityid", all.x = TRUE) #left join
#reorder columns: subject, activity first
refcols <- c("subject", "activity")
dsraw$activityid <- NULL
dsraw <- dsraw[ , c(refcols, setdiff(names(dsraw), refcols))]

######################################################################
#4. Appropriately labels the data set with descriptive variable names.
######################################################################

tidyNames <- gsub("\\.", "", names(dsraw))
tidyNames <- gsub("^f", "frequencyDomain", tidyNames) 
tidyNames <- gsub("^t", "timeDomain", tidyNames)
tidyNames <- gsub("Acc", "Accelerometer", tidyNames) 
tidyNames <- gsub("Gyro", "Gyroscope", tidyNames) 
tidyNames <- gsub("Mag", "Magnitude", tidyNames) 
tidyNames <- gsub("Freq", "Frequency", tidyNames)
tidyNames <- gsub("mean", "Mean", tidyNames) 
tidyNames <- gsub("std", "StandardDeviation", tidyNames)
dsraw <- setNames(dsraw, tidyNames)

###############################################################################
#5. From the data set in step 4, creates a second, independent tidy data set 
#       with the average of each variable for each activity and each subject.
###############################################################################

# group by subject and activity and summarise using mean
dsrawt <- dsraw %>% 
        group_by(subject, activity) %>%
        summarise_all(mean)

# output to file "tidy_data.txt"
write.table(dsrawt, "tidy_data.txt", row.names = FALSE, quote = FALSE)



