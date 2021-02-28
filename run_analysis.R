# Clear the variable environment!

rm(list = ls())

# Read in the data files

features <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt", col.names = c("n", "Features"))
activity_labels <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", col.names = c("Class", "Activity"))
x_train <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", col.names = features$Features)
y_train <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt", col.names = "Class")

x_test <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", col.names = features$Features)
y_test <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt", col.names = "Class")

subject_train <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", col.names = "Subject")
subject_test <- read.table("C:/DataScience/GettingCleaningData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", col.names = "Subject")

x_totall <- rbind(x_train, x_test)
y_totall <- rbind(y_train, y_test)
subject_total <- rbind(subject_train, subject_test)

# Question 1

TidyData <- cbind(subject_total, x_totall, y_totall)

# Question 2
grep(pattern = "mean\\(\\)|std\\(\\)", x = features$Features, value = TRUE)


# Question 3
TidyData <- merge(activity_labels, TidyData , by.x="Class", by.y = "Class", all.x=TRUE)

#Question 4
names(TidyData) <- sub("Acc", replacement = "Accelerometer", x = names(TidyData))
names(TidyData) <- sub("Gyro", replacement = "Gyroscope", x = names(TidyData))

names(TidyData) <- sub("^t", replacement = "Time", x = names(TidyData))
names(TidyData) <- sub("^f", replacement = "Frequency", x = names(TidyData))

names(TidyData) <- sub("BodyBody", replacement = "Body", x = names(TidyData))
names(TidyData) <- sub("tBody", replacement = "TimeBody", x = names(TidyData))

names(TidyData) <- sub("Mag", replacement = "Magnitude", x = names(TidyData))
names(TidyData) <- sub("angle", replacement = "Angle", x = names(TidyData))
names(TidyData) <- sub("gravity", replacement = "Gravity", x = names(TidyData))

# Question 5

TidyData$Subject= as.factor(TidyData$Subject)
TidyData$Activity = as.factor(TidyData$Activity)

TidyData_Aggregated <- aggregate(. ~Subject + Activity, TidyData, mean)
TidyData_Aggregated <- TidyData_Aggregated[order(TidyData_Aggregated$Subject,TidyData_Aggregated$Activity),]
write.table(TidyData_Aggregated, file = "Tidy.txt", row.names = FALSE)
