#######################
#Getting and Cleaning Data Course Project
######################

#set directory and download data
setwd("/CleaningData")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "dataset.zip", method = "curl", mode='wb')
unzip('./dataset.zip')


#load packages
library(doBy)

# Load Data
#train dataset
df.X.Train <- read.table('./UCI HAR Dataset/train/X_train.txt')
df.Y.Train <- read.table('./UCI HAR Dataset/train/y_train.txt')

#test dataset
df.X.Test <- read.table('./UCI HAR Dataset/test/X_test.txt')
df.Y.Test <- read.table('./UCI HAR Dataset/test/y_test.txt')

#subject dataset
df.Subject.Train <- read.table('./UCI HAR Dataset/train/subject_train.txt')
df.Subject.Test <- read.table('./UCI HAR Dataset/test/subject_test.txt')

#features
features <- read.table('./UCI HAR Dataset/features.txt')
#activity
activity <- read.table("./UCI HAR Dataset//activity_labels.txt")


# 1.Merges the training and the test sets to create one data set
X.data <- rbind(df.X.Train, df.X.Test)
Y.data <- rbind(df.Y.Train, df.Y.Test)
Subject.data <- rbind(df.Subject.Train, df.Subject.Test)


#2 Extracts only the measurements on the mean and standard deviation for each measurement. 

i <- grep("mean\\(\\)|std\\(\\)", features[,2])
X.data <- X.data[, i ]
names(X.data) <- features[i, 2]
names(X.data) <- gsub("\\(\\)", "", features[i, 2]) 
names(X.data) <- gsub("mean", "Mean", names(X.data)) 
names(X.data) <- gsub("std", "Std", names(X.data)) 
names(X.data) <- gsub("-","",names(X.data))

#3 Uses descriptive activity names to name the activities in the data set

activity[, 2] <- tolower(activity[,2])
activity[, 2] <- gsub("_"," ",activity[, 2])
activity[,2]  <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", activity[,2], perl=TRUE)
activity[,2]  <- gsub(" ","",activity[,2])

names(Y.data) <- "Activity"
Y.data[,1] <- activity[Y.data[,1],2]

# 4. Appropriately labels the data set with descriptive variable names. 
names(Subject.data) <- "Subject"
dataset <- cbind(Subject.data, X.data, Y.data)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
activity.subject <- summaryBy(. ~ Subject + Activity, data=dataset, FUN=mean)
names(activity.subject) <- gsub(".mean$","", names(activity.subject))

#Save dataset
write.table(activity.subject, "dataset_means.txt" , row.names = FALSE) 


