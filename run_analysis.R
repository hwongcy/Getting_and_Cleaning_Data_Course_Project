# ==============================================================================
#
# run_analysis.R
#
# This program is used for the course project of the 
# "Getting and Cleaning Data" course.
#
# The objective of this program is 
#
# 1. Merges the training and the test sets to create a one data set.
# 2. Extracts only the measurements on the mean and standard deviation of each measurement.
# 3. Uses descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
#
# In addition, data will be downloaded and prepared before the above tasks to be
# performed. The new dataset will be saved in a data file "tidy_data.txt".
#
# ==============================================================================


# ===================
# 0. DATA PREPARATION
# ===================


# 1. Download the file from given URL

if(!file.exists("./data")) {
    dir.create("./data")
}

data.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(data.url, destfile = "./data/Dataset.zip", method = "curl")
unzip(zipfile = "./data/Dataset.zip", exdir = "./data")

# 2. Reading training Data

train.subject <- read.table("data/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
train.activity <- read.table("data/UCI HAR Dataset/train/y_train.txt", header = FALSE)
train.features <- read.table("data/UCI HAR Dataset/train/X_train.txt", header = FALSE)

# 3. Reading test Data

test.subject <- read.table("data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
test.activity <- read.table("data/UCI HAR Dataset/test/y_test.txt", header = FALSE)
test.features <- read.table("data/UCI HAR Dataset/test/X_test.txt", header = FALSE)


# =============================================================
# I. Merges the training and the test data stes to one data set
# =============================================================

# 1. subject data sets

# 1.1 Merge training and test data sets for subject

data.subject <- rbind(train.subject, test.subject)

# 1.2 Rename the column name of subject data set to "Subject"

colnames(data.subject) <- "Subject"

#2 activity data sets

# 2.1 Merge training and test data sets for activity

data.activity <- rbind(train.activity, test.activity)

# 2.2 Rename the column name of activity data set to "Activity"

colnames(data.activity) <- "Activity"

# 3. features data sets

# 3.1 Merge training and test data sets for features

data.features <- rbind(train.features, test.features)

# 3.2 Rename the column names of features data set according to "features.txt"

colnames.features <- read.table("data/UCI HAR Dataset/features.txt", header = FALSE)
colnames(data.features) <- t(colnames.features[2])

# 4. Merge subject, activity and features data sets into in data set 
#    and store in "complete.data"

data.all <- cbind(data.subject, data.activity, data.features)


# =====================================================================
# II. Extracts only the measurements of the mean and standard deviation
#     for each measurement with Subject and Activity
# =====================================================================

colMeanSD <- grep(".*Mean.*|.*Std.*", names(data.all), ignore.case = TRUE)

data.Extracted <- subset(data.all, select = c(1, 2, colMeanSD))


# =================================================================
# III. Use descriptive names to name the activities in the data set
# =================================================================

# 1. get the activity labels

label.activity <- read.table("data/UCI HAR Dataset/activity_labels.txt", header = FALSE)

# 2. since the "Activity" field is numeric, it is needed to converted to 
#    character before it could be labelled.

data.Extracted$Activity <- as.character(data.Extracted$Activity)

# 3. label the "Activity"

for(i in 1:6) {
    data.Extracted$Activity[data.Extracted$Activity==i] <- as.character(label.activity[i,2])
}

# 4. factorize the "Activity" 

data.Extracted$Activity <- as.factor(data.Extracted$Activity)


# =====================================================================
# IV. Appropriately labels the data set with descriptive variable names
# =====================================================================

# It is recommended to rename below abbreviations:
# i.    "t":        "Time"
# ii.   "f":        "Frequency"
# iii.  "Acc":      "Accelerometer"
# iv.   "Gyro":     "Gyroscope"
# v.    "BodyBody": "Body"
# vi.   "Mag":      "Magnitude"
# vii.  "angle":    "Angle"
# viii. "gravity":  "Gravity"
# xi.   "-mean()":  "Mean"
# x.    "-std()":   "STD"
# xi.   "-freq()":  "Frequency"

names(data.Extracted) <- gsub("^t", "Time", names(data.Extracted))
names(data.Extracted) <- gsub("^f", "Frequency", names(data.Extracted))
names(data.Extracted) <- gsub("Acc", "Accelerometer", names(data.Extracted))
names(data.Extracted) <- gsub("Gyro", "Gyroscope", names(data.Extracted))
names(data.Extracted) <- gsub("BodyBody", "Body", names(data.Extracted))
names(data.Extracted) <- gsub("Mag", "Magnitude", names(data.Extracted))
names(data.Extracted) <- gsub("angle", "Angle", names(data.Extracted))
names(data.Extracted) <- gsub("gravity", "Gravity", names(data.Extracted))
names(data.Extracted) <- gsub("-mean()", "Mean", names(data.Extracted), ignore.case = TRUE)
names(data.Extracted) <- gsub("-std()", "STD", names(data.Extracted), ignore.case = TRUE)
names(data.Extracted) <- gsub("-freq()", "Frequency", names(data.Extracted), ignore.case = TRUE)


# ===========================================================================
# V. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.
# ===========================================================================

# 1. set "Subject" as the factor variable

data.Extracted$Subject <- as.factor(data.Extracted$Subject)

# 2. calculate the average for each subject and activity

data.New <- aggregate(. ~Subject + Activity, data.Extracted, mean)
data.New <- data.New[order(data.New$Subject, data.New$Activity),]

# 3. create a new data set file

write.table(data.New, file = "tidy_data.txt", row.names = FALSE)



