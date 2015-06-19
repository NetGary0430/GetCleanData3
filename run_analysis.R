require(plyr)

## Clear workspace
rm(list=ls())
# Directories and files
pathDir <- "./UCI HAR Dataset/"
featureFile <- paste(pathDir, "/features.txt", sep = "")
actLabelFile <- paste(pathDir, "/activity_labels.txt", sep = "")
xTrainFile <- paste(pathDir, "/train/X_train.txt", sep = "")
yTrainFile <- paste(pathDir, "/train/y_train.txt", sep = "")
subjectTrainFile <- paste(pathDir, "/train/subject_train.txt", sep = "")
xTestFile  <- paste(pathDir, "/test/X_test.txt", sep = "")
yTestFile  <- paste(pathDir, "/test/y_test.txt", sep = "")
subjectTestFile <- paste(pathDir, "/test/subject_test.txt", sep = "")

# Load raw data
features <- read.table(featureFile, colClasses = c("character"))
activity_labels <- read.table(actLabelFile, col.names = c("ActivityId", "Activity"))
x_train <- read.table(xTrainFile)
y_train <- read.table(yTrainFile)
subject_train <- read.table(subjectTrainFile)
x_test <- read.table(xTestFile)
y_test <- read.table(yTestFile)
subject_test <- read.table(subjectTestFile)


# 1. Merges the training and the test sets to create one data set.


# Combine sensor data
training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_sensor_data, test_sensor_data)

# Label columns
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels


# 2. Extracts only the measurements on the mean and standard deviation for each.

sensor_data_mean_std <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]

# 3. Uses descriptive activity names to name the activities in the data set

sensor_data_mean_std <- join(sensor_data_mean_std, activity_labels, by = "ActivityId", match = "first")
sensor_data_mean_std <- sensor_data_mean_std[,-1]

# 4. Appropriately labels the data set with descriptive names.
# Get rid of parentheses
names(sensor_data_mean_std) <- gsub('\\(|\\)',"",names(sensor_data_mean_std), perl = TRUE)
# Make easier to read valid names
names(sensor_data_mean_std) <- make.names(names(sensor_data_mean_std))

names(sensor_data_mean_std) <- gsub('Acc',"Acceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Gyro',"AngularSpeed",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Mag',"Magnitude",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^t',"TimeDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^f',"FrequencyDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\.mean',".Mean",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\.std',".StandardDeviation",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq\\.',"Frequency.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq$',"Frequency",names(sensor_data_mean_std))

# 5. Creates another tidy data set with the avg of each variable for each activity and each subject.
sensor_avg_by_act_sub = ddply(sensor_data_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "sensor_avg_by_act_sub.txt", row.names= FALSE)