# Clear the variable environment!

rm(list = ls())

# Read in the data files

features <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt", col.names = c("n", "Features"))

# Only mean and std features

#features_desired_ids <- grep("mean|std", features_orig[,2])
#features_desired <- features_orig[features_desired_ids,]
#features <- features_desired

activity_labels <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
x_train <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", col.names = features$Features)

#x_train <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
#names(x_train) <- features_orig[,2]
#x_train <- x_train[,features_desired[,2]]

y_train <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt", col.names = "Activity")

x_test <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", col.names = features$Features)
#names(x_test) <- features_orig[,2]
#x_test <- x_test[,features_desired[,2]]

y_test <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt", col.names = "Activity")

subject_train <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", col.names = "Subject")
subject_test <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", col.names = "Subject")

#x_totall <- rbind(x_train, x_test)
#y_totall <- rbind(y_train, y_test)

data_train <- cbind(subject_train, y_train, x_train)
data_test <- cbind(subject_test, y_test, x_test)
#subject_total <- rbind(subject_train, subject_test)

# Question 1: Merges the training and the test sets to create one data set.

data_raw <- rbind(data_train, data_test)

# Question 2: Extracts only the measurements on the mean and standard deviation for each measurement. 

features_desired <- grepl(pattern = "mean\\(\\)|std\\(\\)", x = features$Features)
features_desired[1:2] <- TRUE
data_desired <- data_raw[,features_desired]

# Question 3: Uses descriptive activity names to name the activities in the data set

#TidyData <- merge(activity_labels, TidyData , by.x="Class", by.y = "Class", all.x=TRUE)
data_desired$Activity <- factor(data_desired$Activity, labels = c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"))

# #Question 4: Appropriately labels the data set with descriptive variable names. 

names(data_desired) <- sub("Acc", replacement = "Accelerometer", x = names(data_desired))
names(data_desired) <- sub("Gyro", replacement = "Gyroscope", x = names(data_desired))

names(data_desired) <- sub("^t", replacement = "Time", x = names(data_desired))
names(data_desired) <- sub("^f", replacement = "Frequency", x = names(data_desired))

names(data_desired) <- sub("BodyBody", replacement = "Body", x = names(data_desired))
names(data_desired) <- sub("tBody", replacement = "TimeBody", x = names(data_desired))

names(data_desired) <- sub("Mag", replacement = "Magnitude", x = names(data_desired))
names(data_desired) <- sub("angle", replacement = "Angle", x = names(data_desired))
names(data_desired) <- sub("gravity", replacement = "Gravity", x = names(data_desired))

# Question 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

data_desired$Subject= as.factor(data_desired$Subject)
#TidyData$Activity = as.factor(TidyData$Activity)

data_tidy <- aggregate(. ~Subject + Activity, data_desired, mean)
data_tidy <- data_tidy[order(data_tidy$Subject,data_tidy$Activity),]
write.table(data_tidy, file = "Tidy.txt", row.names = FALSE)
