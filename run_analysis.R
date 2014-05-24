# set path
path <- 'UCI HAR Dataset'
activity_labels_path <- paste(path, 'activity_labels.txt', sep='\\')
y_train_path <- paste(path, 'train', 'y_train.txt', sep='\\')
y_test_path <- paste(path, 'test', 'y_test.txt', sep='\\')
X_train_path <- paste(path, 'train', 'X_train.txt', sep='\\')
X_test_path <- paste(path, 'test', 'X_test.txt', sep='\\')
features_path <- paste(path, 'features.txt', sep='\\')
subject_train_path <- paste(path, 'train', 'subject_train.txt', sep='\\')
subject_test_path <- paste(path, 'test', 'subject_test.txt', sep='\\')
output_path <- paste(path, 'tidy_data.txt', sep='\\')

# load data
load_data <- function(data_path){read.table(data_path, head=F, stringsAsFactors=F)}
activity_labels <- load_data(activity_labels_path)
y_train <- load_data(y_train_path)
y_test <- load_data(y_test_path)
X_train <- load_data(X_train_path)
X_test <- load_data(X_test_path)
features <- load_data(features_path)
subject_train <- load_data(subject_train_path)
subject_test <- load_data(subject_test_path)

# Merges the training and the test sets to create one data set.
data <- rbind(X_train, X_test)

# Extracts only the measurements on the mean and standard deviation for each measurement.
names(data) <- features[[2]]
data <- data[, grepl('mean\\(\\)', features[[2]]) | grepl('std\\(\\)', features[[2]])]

# Appropriately labels the data set with descriptive activity names.
activity_train <- sapply(y_train[[1]], function(x){activity_labels[x, 2]})
activity_test <- sapply(y_test[[1]], function(x){activity_labels[x, 2]})
activity <- c(activity_train, activity_test)
data$activities <- activity

# Appropriately labels the data set with subject ID.
subject <- c(subject_train[[1]], subject_test[[1]])
data$subjects <- subject

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(reshape2)
n <- ncol(data)
data <- melt(data, id.vars= names(data)[(n-1):n], measure.vars=names(data)[c(1:(n-2))])
data <- dcast(data, subjects+activities~variable, mean)

# Output
write.table(data2, output_path, row.names=F)
