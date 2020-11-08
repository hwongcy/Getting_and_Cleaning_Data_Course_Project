# Getting and Cleaning Data Course Project

## Purpose

The purpose of this project is used to demonstrate the ability to collect,
work with and clean data set. The goal is to prepare a new tidy data set that
can be used for later analysis.

In order to complete this project, it is asked to

    1. Merges the training and the test sets to create a one data set.
    2. Extracts only the measurements on the mean and standard deviation of 
       each measurement.
    3. Uses descriptive activity names to name the activities in the data set.
    4. Appropriately labels the data set with descriptive variable names.
    5. From the data set in step 4, creates a second, independent tidy data set 
       with the average of each variable for each activity and each subject.


## Data Preparation

Data to be used are collected from the accelerometers of the SamSung
Galaxy S smartphones which is provided from the course website as shown below:

<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

And the dataset will be downloaded by 

```{r data_download}
data.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(data.url, destfile = "./data/Dataset.zip", method = "curl")
unzip(zipfile = "./data/Dataset.zip", exdir = "./data")
```

According to the **README.txt** from the dataset, it is found that below data files 
will be used:

    - feature.txt:           List of all features
    - activity_labels.txt:   Activity names
    - train/X_train.txt:     Training dataset of features
    - train/y_train.txt:     Training dataset of activities
    - train/subject_train:   Training dataset of subjects
    - test/X_test.txt:       Test dataset of features
    - test/y_test.txt:       Test dataset of activities
    - test/subject_test.txt: Test dataset of subjects

Therefore, training and test dataset will be read as shown below:

```{r read_data}
train.subject <- read.table("data/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
train.activity <- read.table("data/UCI HAR Dataset/train/y_train.txt", header = FALSE)
train.features <- read.table("data/UCI HAR Dataset/train/X_train.txt", header = FALSE)
test.subject <- read.table("data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
test.activity <- read.table("data/UCI HAR Dataset/test/y_test.txt", header = FALSE)
test.features <- read.table("data/UCI HAR Dataset/test/X_test.txt", header = FALSE)
```


