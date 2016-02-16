# Getting and Cleaning Data Project

Andrés Martínez

# Source

You can the infomation about the project  on the website 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones




# The description of the data for this project can be found here.

* Data Set Information

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.


# Assigments

## 1. Merge the training and the test sets to create one data set.

 After setting the source directory for the files, read into tables the data located in

* features.txt
* activity_labels.txt
* subject_train.txt
* x_train.txt
* y_train.txt
* subject_test.txt
* x_test.txt
* y_test.txt

Assign column names and merge to create one data set.

## 2. Extract only the measurements on the mean and standard deviation for each measurement.

Create a logical vector that contains TRUE values for the ID, mean and stdev columns and FALSE values for the others. Subset this data to keep only the necessary columns.

## 3. Use descriptive activity names to name the activities in the data set

Merge data subset with the activityType table to include the descriptive activity names

## 4. Appropriately label the data set with descriptive activity names.

Use gsub function for pattern replacement to clean up the data labels.

## 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

for the project instructions, i need to produce only a data set with the average of each veriable for each activity and subject


Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

* gravityMean
* tBodyAccMean
* tBodyAccJerkMean
* tBodyGyroMean
* tBodyGyroJerkMean
