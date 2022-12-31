## You should create one R script called run_analysis.R that does the following. 

## Merges the training and the test sets to create one data set.

## Extracts only the measurements on the mean and standard deviation for each measurement. 

## Uses descriptive activity names to name the activities in the data set

## Appropriately labels the data set with descriptive variable names. 

## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

fileurl <-  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile = "wk4dataset.zip")
unzip("wk4dataset.zip")

library(dplyr)
library(tidyselect)

## read the test data/subjects/activities
test_subject <- read.table("~/Rstudio_projects/datasciencecoursera/UCI HAR Dataset/test/subject_test.txt") ## subject ids 1-30
test_set <- read.table("~/Rstudio_projects/datasciencecoursera/UCI HAR Dataset/test/X_test.txt") ## results data
test_labels <- read.table("~/Rstudio_projects/datasciencecoursera/UCI HAR Dataset/test/y_test.txt") ## activity names 1-6
test_subject <- test_subject %>% rename(subject=V1) ## rename column name
test_labels <- test_labels %>% rename(activity=V1) ## rename column name

## read the train data/subjects/activities
train_subject <- read.table("~/Rstudio_projects/datasciencecoursera/UCI HAR Dataset/train/subject_train.txt")
train_set <- read.table("~/Rstudio_projects/datasciencecoursera/UCI HAR Dataset/train/X_train.txt")
train_labels <- read.table("~/Rstudio_projects/datasciencecoursera/UCI HAR Dataset/train/y_train.txt")
train_subject <- train_subject %>% rename(subject=V1)
train_labels <- train_labels %>% rename(activity=V1)

## read the features file that provides the variable names for the data
features <- read.table("~/Rstudio_projects/datasciencecoursera/UCI HAR Dataset/features.txt")
features <- features[c(2)] ## subset the descriptions
features_t <- as.data.frame(t(features)) ## transpose the rows and columns
features_t <- features_t %>% as.data.frame(row.names = 1:nrow(.)) ## renumber the rows

## bind the test data and features and make the features new variable names
test_result <- rbind(features_t, test_set)
test_result <- test_result %>%  row_to_names(row_number = 1)
clean_names(test_result)
test_result <- test_result %>% as.data.frame(row.names = 1:nrow(.))

## bind the train data and features and make the features new variable names
train_result <- rbind(features_t, train_set)
train_result <- train_result %>%  row_to_names(row_number = 1) ## convert features row to variable names
clean_names(train_result)
train_result <- train_result %>% as.data.frame(row.names = 1:nrow(.)) ## renumber rows

## merge the test set with subject and activity
test_result <- cbind(test_labels, test_result)
test_result <- cbind(test_subject, test_result)

## merge the train set with subject and activity
train_result <- cbind(train_labels, train_result)
train_result <- cbind(train_subject, train_result)

## 1. Merge the training and the test sets to create one data set.
combined_result <- rbind(test_result, train_result)

## 2. Extract only the measurements on the mean and standard deviation for each measurement.

extract1 <- combined_result[,vars_select(names(combined_result), contains("mean()"), .include = "subject")]
extract2 <- combined_result[,vars_select(names(combined_result), contains("std()"), .include = "activity")]

## combine the resulting extracts
extract_final <- cbind(extract1, extract2)

## move the activity column after the subject column
extract_final <- relocate(extract_final, activity, .after = subject)

## 3. Use descriptive activity names to name the activities in the data set
extract_final["activity"][extract_final["activity"] == 1] <- "walking" ## use the descriptions contained in activity_labels.txt
extract_final["activity"][extract_final["activity"] == 2] <- "walkingUpStairs"
extract_final["activity"][extract_final["activity"] == 3] <- "walkingDownStairs"
extract_final["activity"][extract_final["activity"] == 4] <- "sitting"
extract_final["activity"][extract_final["activity"] == 5] <- "standing"
extract_final["activity"][extract_final["activity"] == 6] <- "laying"

## 4. Appropriately label the data set with descriptive variable names.
## I have merged the actual features list to create the variable names above.

## 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
extract_final <- extract_final %>% group_by(subject, activity) # group by subject and activity to calc the means
extract_transform <- extract_final %>% mutate(across(where(is.character), ~ as.numeric(.x))) ## make character columns numeric in order to calc the means
extract_summary <- extract_transform %>% summarise_each(funs(mean)) ## create independent tidy data set of the means
