# set the working directory for the project
myDataPath <- "./cleanProjectdata"
setwd(myDataPath)

# download the archived data to the project dir
if(!file.exists(myDataPath)){dir.create(myDataPath)}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileUrl,destfile="./cleanProjectdata/Dataset.zip")

# extract dataSet to the project dir
unzip(zipfile="./cleanProjectdata/Dataset.zip",exdir=myDataPath)

# From the readme file I can see that:
#Values of Varible Activity come from “Y_train.txt” and “Y_test.txt”
#values of Varible Subject come from “subject_train.txt” and subject_test.txt"
#Values of Varibles Features come from “X_train.txt” and “X_test.txt”
#Names of Varibles Features come from “features.txt”
#levels of Varible Activity come from “activity_labels.txt”


# import training set as separate vectors:
xTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
subTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# import testing set as separate vectors:
xTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
yTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
subTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# importing the list of all features:
features <- read.table('./UCI HAR Dataset/features.txt')

# importing activity labels:
actLabels = read.table('./UCI HAR Dataset/activity_labels.txt')


# give descriptive column names instead of default ones
colnames(xTrain) <- features$V2 
colnames(yTrain) <-"ActivityID"
colnames(subTrain) <- "ParticipantID"

colnames(xTest) <- features$V2 
colnames(yTest) <- "ActivityID"
colnames(subTest) <- "ParticipantID"
colnames(actLabels) <- c("ActivityID","ActivityType")

## bind all the vectors to the data frame for training and testing data accordingly
trainDF <- cbind(xTrain, yTrain, subTrain)
testDF <- cbind(xTest, yTest, subTest)

# and merge the two resulting data sets into the one
overallDF <- rbind(trainDF, testDF)

## extracting only the data for mean values and standard deviation
# we will get an integer vector indicating that this column contains substring "mean()"
# we also need to manually add additional vectors obtained by averaging the signals in a signal window sample (see features_info.txt).
# i.e. features from 555 to 558 according to file "features.txt"
isMean <- c(
  grep("mean()", names(overallDF), value = FALSE, fixed = TRUE),
  555:558)

# make the same vector for standard deviation only this time it's not necessary to manually add the features
isSD <- grep("std()", names(overallDF), value = FALSE, fixed = TRUE)

# make the subset consisting only of data fields identifying the activity and participants and data field containing info on mean and sd
subsetDF <- cbind(overallDF$ActivityID,
                  overallDF$ParticipantID,
                  overallDF[, isMean], 
                  overallDF[, isSD])

# rename some of the assigned colnames to increase tidyness
colnames(subsetDF)[colnames(subsetDF) == "overallDF$ActivityID"] <- "ActivityID"
colnames(subsetDF)[colnames(subsetDF) == "overallDF$ParticipantID"] <- "ParticipantID"

## add the text activity descriptions
subsetDF <- merge(subsetDF, actLabels, by="ActivityID", all.x=TRUE)

## label the data with descriptive variable names
names(subsetDF) <- gsub("Acc", "Accelerator", names(subsetDF))
names(subsetDF) <- gsub("Mag", "Magnitude", names(subsetDF))
names(subsetDF) <- gsub("Gyro", "Gyroscope", names(subsetDF))
names(subsetDF) <- gsub("^t", "time", names(subsetDF))
names(subsetDF) <- gsub("^f", "frequency", names(subsetDF))


# Creating a second, independent tidy data set with the average of each variable for each activity and each subject
# it is not necessary to use other packages built-in function aggregate is enough
# in this case we aggregate all data fields by each participant and each activity type
tidyDF <- aggregate(. ~ ActivityID + ParticipantID,
                    data=subsetDF, 
                    mean)

# need to drop Activity type from the tidy dataset as it was messed up during the aggregation process (as it's a string and not numberic)
tidyDF <- tidyDF[, !(colnames(tidyDF) %in% c("ActivityType"))]

# and now to get the correct activity type
tidyDF <- merge(tidyDF, actLabels, by="ActivityID", all.x=TRUE)

# order the set by participant ID and activity ID to make it tidy++
tidyDF <- tidyDF[order(tidyDF$ParticipantID, tidyDF$ActivityID),]

# write the resultant tidy dataset to the txt file
write.table(tidyDF, "tidyDF.txt", row.name=FALSE)
