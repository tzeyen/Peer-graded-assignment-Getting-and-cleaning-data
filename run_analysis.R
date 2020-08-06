##Peer graded assignment: Getting and cleaning data

##Loading required packages
library(dplyr)

##Step 1: Merges the training and the test sets to create one data set.
##Reading files 
features <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

##Assigning column names
colnames(x_train) <- features[,2]
colnames(x_test) <- features[,2]

##Merging all data in one set
x_train$activity <- y_train[,1]
x_train$subject <- subject_train[,1]
x_test$activity <- y_test[,1]
x_test$subject <- subject_test[,1]

tidy_set <- rbind(x_train, x_test)

##Step 2: Extracts only the measurements on the mean and standard deviation for each measurement
col_names <- colnames(tidy_set)
mean_std <- (col_names == "activity" | col_names == "subject" |
                     grepl("mean\\(\\)", col_names) |
                     grepl("std\\(\\)", col_names))
measurements <- tidy_set[,mean_std]


##Step 3: Uses descriptive activity names to name the activities in the data set.
measurements$activity <- activity_labels[measurements$activity,]$V2

##Step 4: Appropriately labels the data set with descriptive variable names.
names(measurements) <- gsub("Acc", "Accelerator", names(measurements))
names(measurements) <- gsub("Mag", "Magnitude", names(measurements))
names(measurements) <- gsub("Gyro", "Gyroscope", names(measurements))
names(measurements) <- gsub("^t", "time", names(measurements))
names(measurements) <- gsub("^f", "frequency", names(measurements))
names(measurements)<-gsub("BodyBody", "Body", names(measurements))

##Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
TidyDataSets <- aggregate(. ~ activity + subject, measurements, mean)
write.table(TidyDataSets, "TidyDataSets.txt", row.name=FALSE)

##Produce a codebook for the final dataset
library(memisc)
Write(codebook(TidyDataSets), file="CodeBook.md")
