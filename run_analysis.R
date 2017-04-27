## Ths is a R script which is used for the project.
## Author : Giridhar
## Written on 27-04-2017
## Steps that this R code covers are as follows.

## Merges the training and the test sets to create one data set.

## Extracts only the measurements on the mean and standard deviation 
## for each measurement.

## Uses descriptive activity names to name the activities in the data set

## Appropriately labels the data set with descriptive variable names.

## creates a second, independent tidy data set with the average of each variable 
## for each activity and each subject.



setwd("C:\\Users\\admin\\Desktop\\DATASCIENCE\\7. Getting and CLeaning Data\\34. Final Project");

features <- read.table("./UCI HAR Dataset/features.txt", header = F, sep = " ");
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = F);

## Reading test data

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = F);
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = F);
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = F);

## Reading training data

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = F);
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = F);
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = F);

## Merging all datasets together   - - - STEP 1

complete_test <- cbind(subject_test,x_test,y_test);
complete_train <- cbind(subject_train,x_train,y_train);
merged_dataset <- rbind(complete_test,complete_train);

## Extract only mean and stdev data - - - STEP 2

names_to_be_added <- c("subjectid",as.character(features[,2]),"activityid");
colnames(merged_dataset) <- names_to_be_added;
lv <- grepl("mean[()]",names_to_be_added) | grepl("std()",names_to_be_added);
lv[1] <- TRUE;
lv[length(lv)] <- TRUE;
mean_std_data <- merged_dataset[,lv];

## descriptive activity names to name the activities in the data set

names(activity_labels) <- c("activityid", "activityname");
activity_named_data <- merge(mean_std_data,activity_labels, by = "activityid", all.x = TRUE);

## clean up the column names

clean_names <- colnames(activity_named_data);
clean_names <- gsub("\\()","",clean_names);
clean_names <- gsub("^t","time",clean_names);
clean_names <- gsub("^f","fourier",clean_names);
clean_names <- gsub("-","",clean_names);
clean_names <- tolower(clean_names);

colnames(activity_named_data) <- clean_names;

## Creating a temporary dataset with average of all values

library(dplyr);
temp_dataset <- select(activity_named_data,-activityname);
grouped_dataset <- group_by(temp_dataset,activityid,subjectid);
final_data <- summarise_each(grouped_dataset,funs(mean));
final_data <- merge(final_data, activity_labels, by = "activityid", all.x = TRUE);
write.table(final_data, './tidyData.txt',row.names=FALSE,sep='\t');


