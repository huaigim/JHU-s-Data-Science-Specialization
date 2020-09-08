# Getting and Cleaning Data Course Project
# Johns Hopkins University (Coursera)
# Author: Ong Huai Gim Nicholas

# 1. Merges the training and the test sets to create one data set
# 2. Extracts only the measurements on the mean and standard deviation for each measurement
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

# Load packages and get data
library(data.table)
library(reshape2)

path <- getwd()
path
url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if (!file.exists('./UCI HAR Dataset.zip')) {
        download.file(url, file.path(path, 'UCI HAR Dataset.zip'))
        unzip(zipfile = 'UCI HAR Dataset.zip')
}

# Load activity labels and features
activity_labels <- fread(file.path(path, 'UCI HAR Dataset/activity_labels.txt'),
                         col.names = c('class_labels', 'activity_name'))
features <- fread(file.path(path, 'UCI HAR Dataset/features.txt'), 
                  col.names = c('index', 'feature_names'))
features_wanted <- grep('(mean|std)\\(\\)', features[, feature_names])
measurements <- features[features_wanted, feature_names]
measurements <- gsub('[()]', '', measurements)

# Load train datasets
train <- fread(file.path(path, 'UCI HAR Dataset/train/X_train.txt'))[, features_wanted, with = F]
data.table::setnames(train, colnames(train), measurements)
train_activities <- fread(file.path(path, 'UCI HAR Dataset/train/Y_train.txt'), 
                          col.names = c('Activity'))
train_subjects <- fread(file.path(path, 'UCI HAR Dataset/train/subject_train.txt'), 
                        col.names = c('Subject_Num'))
train <- cbind(train_subjects, train_activities, train)

# Load test datasets
test <- fread(file.path(path, 'UCI HAR Dataset/test/X_test.txt'))[, features_wanted, with = F]
data.table::setnames(test, colnames(test), measurements)
test_activities <- fread(file.path(path, 'UCI HAR Dataset/test/Y_test.txt'), 
                         col.names = c('Activity'))
test_subjects <- fread(file.path(path, 'UCI HAR Dataset/test/subject_test.txt'), 
                       col.names = c('Subject_Num'))
test <- cbind(test_subjects, test_activities, test)

# Merge datasets
combined <- rbind(train, test)

# Convert class_labels to activity_name
combined[['Activity']] <- factor(combined[, Activity], 
                                 levels = activity_labels[['class_labels']],
                                 labels = activity_labels[['activity_name']])

combined[['Subject_Num']] <- as.factor(combined[, Subject_Num])
combined <- reshape2::melt(data = combined, id = c('Subject_Num', 'Activity'))
combined <- reshape2::dcast(data = combined, Subject_Num + Activity ~ variable, 
                            fun.aggregate = mean)

data.table::fwrite(x = combined, file = 'tidyData.txt', quote = F)