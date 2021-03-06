# Codebook for the project
As the logic of the script was explained in more detail in the **README** file below is just the basic information on the ingoing data and outgoing resultant tidy set.

## Data sources

The raw data was coming from the following sources:  
* Values of Varible Activity come from “Y_train.txt” and “Y_test.txt”
* values of Varible Subject come from “subject_train.txt” and subject_test.txt"
* Values of Varibles Features come from “X_train.txt” and “X_test.txt”
* Names of Varibles Features come from “features.txt”
* Levels of Varible Activity come from “activity_labels.txt”

Upon running a script contrained in **run-analysis** file (see more details in **README** file) we generated the tidy data set.

## Resultant data fields in the tidy set:
* "ActivityID" provides a numeric ID for the type of activity
* "ActivityType" provides a more descriptive textual information on the type of activity (e.g. WALKING)
* "ParticipantID"
* "timeBodyAccelerator-mean()-X"
* "timeBodyAccelerator-mean()-Y"
* "timeBodyAccelerator-mean()-Z"
* "timeGravityAccelerator-mean()-X"
* "timeGravityAccelerator-mean()-Y"
* "timeGravityAccelerator-mean()-Z"
* "timeBodyAcceleratorJerk-mean()-X"
* "timeBodyAcceleratorJerk-mean()-Y"
* "timeBodyAcceleratorJerk-mean()-Z"
* "timeBodyGyroscope-mean()-X"
* "timeBodyGyroscope-mean()-Y"
* "timeBodyGyroscope-mean()-Z"
* "timeBodyGyroscopeJerk-mean()-X"
* "timeBodyGyroscopeJerk-mean()-Y"
* "timeBodyGyroscopeJerk-mean()-Z"
* "timeBodyAcceleratorMagnitude-mean()"
* "timeGravityAcceleratorMagnitude-mean()"
* "timeBodyAcceleratorJerkMagnitude-mean()"
* "timeBodyGyroscopeMagnitude-mean()"
* "timeBodyGyroscopeJerkMagnitude-mean()"
* "frequencyBodyAccelerator-mean()-X"
* "frequencyBodyAccelerator-mean()-Y"
* "frequencyBodyAccelerator-mean()-Z"
* "frequencyBodyAcceleratorJerk-mean()-X"
* "frequencyBodyAcceleratorJerk-mean()-Y"
* "frequencyBodyAcceleratorJerk-mean()-Z"
* "frequencyBodyGyroscope-mean()-X"
* "frequencyBodyGyroscope-mean()-Y"
* "frequencyBodyGyroscope-mean()-Z"
* "frequencyBodyAcceleratorMagnitude-mean()"
* "frequencyBodyBodyAcceleratorJerkMagnitude-mean()"
* "frequencyBodyBodyGyroscopeMagnitude-mean()"
* "frequencyBodyBodyGyroscopeJerkMagnitude-mean()"
* "angle(tBodyAcceleratorMean,gravity)"
* "angle(tBodyAcceleratorJerkMean),gravityMean)"
* "angle(tBodyGyroscopeMean,gravityMean)"
* "angle(tBodyGyroscopeJerkMean,gravityMean)"
* "timeBodyAccelerator-std()-X"
* "timeBodyAccelerator-std()-Y"
* "timeBodyAccelerator-std()-Z"
* "timeGravityAccelerator-std()-X"
* "timeGravityAccelerator-std()-Y"
* "timeGravityAccelerator-std()-Z"
* "timeBodyAcceleratorJerk-std()-X"
* "timeBodyAcceleratorJerk-std()-Y"
* "timeBodyAcceleratorJerk-std()-Z"
* "timeBodyGyroscope-std()-X"
* "timeBodyGyroscope-std()-Y"
* "timeBodyGyroscope-std()-Z"
* "timeBodyGyroscopeJerk-std()-X"
* "timeBodyGyroscopeJerk-std()-Y"
* "timeBodyGyroscopeJerk-std()-Z"
* "timeBodyAcceleratorMagnitude-std()"
* "timeGravityAcceleratorMagnitude-std()"
* "timeBodyAcceleratorJerkMagnitude-std()"
* "timeBodyGyroscopeMagnitude-std()"
* "timeBodyGyroscopeJerkMagnitude-std()"
* "frequencyBodyAccelerator-std()-X"
* "frequencyBodyAccelerator-std()-Y"
* "frequencyBodyAccelerator-std()-Z"
* "frequencyBodyAcceleratorJerk-std()-X"
* "frequencyBodyAcceleratorJerk-std()-Y"
* "frequencyBodyAcceleratorJerk-std()-Z"
* "frequencyBodyGyroscope-std()-X"
* "frequencyBodyGyroscope-std()-Y"
* "frequencyBodyGyroscope-std()-Z"
* "frequencyBodyAcceleratorMagnitude-std()"
* "frequencyBodyBodyAcceleratorJerkMagnitude-std()"
* "frequencyBodyBodyGyroscopeMagnitude-std()"
* "frequencyBodyBodyGyroscopeJerkMagnitude-std()"
