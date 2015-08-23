# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# 1. Merge the training and test sets to create one data set

x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject_data <- rbind(subject_train, subject_test)

# 2. Extract only the measurements on the mean and standard deviation for each measurement

features <- read.table("UCI HAR Dataset/features.txt")
mean_std <- grep(".*\\bmean\\b.*|.*\\bstd\\b.*", features[, 2])
x_data <- x_data[,mean_std]
names(x_data)<-features[mean_std, 2]

# 3. Use descriptive activity names to name the activities in the data set

activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt")
y_data[, 1] <- activitylabels[y_data[, 1], 2]
names(y_data) <- "activity"

# 4. Appropriately label the data set with descriptive variable names

names(subject_data) <- "subject"
clean_data <- cbind(subject_data,y_data,x_data)

# 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject

average_data <-  as.data.frame(matrix(NA, nrow=(dim(activitylabels)[1])*(length(table(subject_data))), ncol=(dim(clean_data)[2])))
activity<-as.character(activitylabels[, 2])
r<-1
coln<-(dim(clean_data)[2])
for(i in 1:(length(table(subject_data)))) {
  for(j in 1:(dim(activitylabels)[1])) {
        average_data[r, 1] <- sort(unique(subject_data)[, 1])[i]
        average_data[r, 2] <- activity[j]
        a <- i == clean_data$subject
        b <- activity[j] == clean_data$activity
        average_data[r, 3:coln] <- colMeans(clean_data[a&b, 3:coln])
        r <- r + 1
  }
}
colnames(average_data)<-colnames(clean_data)

write.table(average_data,"average_data.txt",row.name=FALSE)


