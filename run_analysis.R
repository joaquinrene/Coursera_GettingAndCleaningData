##############################################################################
#
#   Script: run_analysis.R
#
#   This script tries to perform some operations with the data collected from the 
#   Samsung Galaxy smartphone accelerometers. The objective is to have a set of tidy
#   and clean data.
#
#   Output file: tidy_data.txt
#

library(dplyr)

# Getting the initial smartphone data from given url
zUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zFile <- "UCI_HAR_Dataset.zip"
if (!file.exists(zFile)) {
  download.file(zUrl, zFile, mode = "wb")
}

data <- "UCI HAR Dataset"
if (!file.exists(data)) {
  unzip(zFile)
}

# Step: 1
# Merge the training and test sets to create one data set
###############################################################################

# Training data
trainSubjects <- read.table(file.path(data, "train", "subject_train.txt"))
xTrain_Values <- read.table(file.path(data, "train", "X_train.txt"))
yTrain_Activity <- read.table(file.path(data, "train", "y_train.txt"))

# Test data
testSubjects <- read.table(file.path(data, "test", "subject_test.txt"))
xTest_Values <- read.table(file.path(data, "test", "X_test.txt"))
yTest_Activity <- read.table(file.path(data, "test", "y_test.txt"))

# merging data tables to make a single data table
activityData <- rbind(
  cbind(trainSubjects, xTrain_Values, yTrain_Activity),
  cbind(testSubjects, xTest_Values, yTest_Activity)
)

# removing individual data tables to optimize memory
rm(trainSubjects, xTrain_Values, yTrain_Activity, 
   testSubjects, xTest_Values, yTest_Activity)

# reading features
all_features <- read.table(file.path(data, "features.txt"), as.is = TRUE)

# reading activity labels
activity <- read.table(file.path(data, "activity_labels.txt"))
colnames(activity) <- c("activityId", "activityLabel")

# assign column names
colnames(activityData) <- c("subject", all_features[, 2], "activity")


# Step 2:  
# Extract only the measurements on the mean and standard deviation for each measurement
##############################################################################

# select columns to keep
columnsToKeep <- grepl("subject|activity|mean|std", colnames(activityData))
activityData <- activityData[, columnsToKeep]


# Step 3: 
# Use descriptive activity names to name the activities in the data set
##############################################################################

activityData$activity <- factor(activityData$activity, levels = activity[, 1], labels = activity[, 2])


# Step 4:
# Appropriately label the data set with descriptive variable names
##############################################################################

activityDataNames <- colnames(activityData)

# removing special characters
activityDataNames <- gsub("[\\(\\)-]", "", activityDataNames)

# expand abbreviations and clean up names
activityDataNames <- gsub("^f", "frequencyDomain", activityDataNames)
activityDataNames <- gsub("^t", "timeDomain", activityDataNames)
activityDataNames <- gsub("Acc", "Accelerometer", activityDataNames)
activityDataNames <- gsub("Gyro", "Gyroscope", activityDataNames)
activityDataNames <- gsub("Mag", "Magnitude", activityDataNames)
activityDataNames <- gsub("Freq", "Frequency", activityDataNames)
activityDataNames <- gsub("mean", "Mean", activityDataNames)
activityDataNames <- gsub("std", "StandardDeviation", activityDataNames)
activityDataNames <- gsub("BodyBody", "Body", activityDataNames)

# update new label names
colnames(activityData) <- activityDataNames


# Step 5:
# Create a second, independent tidy set with the average of each variable for 
# each activity and each subject
##############################################################################

# group by subject and activity and summarise using mean
activityDataMeans <- activityData %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# create output file "tidy_data.txt"
write.table(activityDataMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)

