# REAMDE of the Getting and Cleaning Data Course Project

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

Data to be used are collected from the accelerometers of the SamSung
Galaxy S smartphones which is provided from the course website as shown below:

<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>


## Data Preparation

Dataset will be downloaded and unzipped by *download.file* and *unzip* in the current directory.

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

and datasets will be retrieved by *read.table*. For example,

```{r read_data}
train.subject <- read.table("data/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
test.subject <- read.table("data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
```

## Merges the Training and the Test Sets to Create One Dataset

In order to merge all data into one dataset, training and test datasets will be 
first *rbind* according to subject, activity and features. Corresponding columns 
will be renamed to more descriptive names. Column names of the dataset of features
will be based on the **features.txt**. All *rbind* datasets will be *cbind* to
one dataset.

For example,

```{r merge_datasets}

data.subject <- rbind(train.subject, test.subject)
colnames(data.subject) <- "Subject"

data.all <- cbind(data.subject, data.activity, data.features)

```

## Extracts Only the Measurements on the Mean and Standard Deviation

In order to extract the measurements on the mean and standard deviation only, we will *grep* corresponding measurements with column names containing *Mean* or *Std* as shown below:

```{r extract_mean_and_sd}
colMeanSD <- grep(".*Mean.*|.*Std.*", names(data.all), ignore.case = TRUE)
data.Extracted <- subset(data.all, select = c(1, 2, colMeanSD))
```

## Uses Descriptive Activity Names in the Dataset

In order to assign descriptive activity names to the dataset, corresponding
activity names are needed to extract from **activity_labels.txt**.

Then, activity name will be labeled according to the activity value in the
dataset as shown below:

```{r assign_activity_labels}
for(i in 1:6) {
    data.Extracted$Activity[data.Extracted$Activity==i] <- as.character(label.activity[i,2])
    }
```

## Appropriately Labels the Dataset with Descriptive Variable Names

It is recommended to rename the variable names as shown below:

| Abbreviation | To Be Used/Replaced |
|--------------|---------------------|
| "t"          | "Time"              |
| "f"          | "Frequency"         |
| "Acc"        | "Accelerometer"     |
| "Gyro"       | "Gyroscope"         |
| "BodyBody"   | "Body"              |
| "Mag"        | "Magnitude"         |
| "angle"      | "Angle"             |
| "gravity"    | "Gravity"           |
| "-mean()"    | "Mean"              |
| "-std()"     | "STD"               |
| "-freq()"    | "Frequency"         |

The abbreviation will be pattern matched and replaced by using *gsub* in the dataset. For example,

```{r rename_the variables}
names(data.Extracted) <- gsub("^t", "Time", names(data.Extracted))
names(data.Extracted) <- gsub("Acc", "Accelerometer", names(data.Extracted))
names(data.Extracted) <- gsub("-mean()", "Mean", names(data.Extracted), ignore.case = TRUE)
```

## Create a Second, Independent Tidy Dataset with the Average of Each Variable for Each Activity and Each Subject

The varaible *Subject* in the dataset will be factorized first. Then, the *mean* of each variable for each subject and activity will be computed by using
*aggregate*.

```{r create_new_dataset}
data.Extracted$Subject <- as.factor(data.Extracted$Subject)
data.New <- aggregate(. ~Subject + Activity, data.Extracted, mean)
data.New <- data.New[order(data.New$Subject, data.New$Activity),]
```

Once the new tidy dataset is ready, it will be saved in a new data file in the current directory.

