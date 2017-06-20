# Load list of all features
features <- read.table("UCI HAR Dataset/features.txt", header = FALSE, as.is = TRUE)
# Take just second column, so we have character vector with feature names
features <- features[,2]

# Load activity labels
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

# Load subjects who performed the activity for each window sample from training data set
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)

# Load training data set
feature_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

# Load training activity labels
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)

# Load subjects who performed the activity for each window sample from test data set
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)

# Load test set
feature_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

# Load test labels
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)

# Task 1. Merges the training and the test sets to create one data set.
feature_all <- rbind(feature_train, feature_test)
subject_all <- rbind(subject_train, subject_test)
activity_all <- rbind(activity_train, activity_test)

# Task 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# Rename columns so we can extract based on variable name
names(feature_all) <- features
feature_complete <- feature_all[,grep("(mean|std)\\(\\)", names(feature_all))]

# Task 3. Uses descriptive activity names to name the activities in the data set
activity_complete <- merge(activity_all, activity_labels, by = "V1")

# Merge subject, activity and feature data sets
tidy_data <- tbl_df(cbind(subject_all, activity_complete[,2], feature_complete))

# Task 4. Appropriately labels the data set with descriptive variable names.
names(tidy_data)[1:2] <- c("subject_id","activity")

# Task 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Add factor columns so we can group by based on subject_id and activity
tidy_data <- tidy_data %>% mutate(subject_id = factor(tidy_data$subject_id), activity = factor(tidy_data$activity)) %>% 
             group_by(subject_id, activity) %>% summarize_all(mean)

# Write tidy data to a file
write.table(tidy_data, "tidy_data.txt", row.name=FALSE)
