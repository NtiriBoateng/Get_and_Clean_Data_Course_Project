## The codebook for the tidy data set
#### General info
##### You can read in the tidy data set tidy_data.txt into R via read.csv("./data/tidy_data.txt"). The so created object is a table with dimensions 181 x 81. The first column is the subject, i.e. the person who is using the device. The second column shows the activity. The residual columns show the accelerometer and gyroscope data. A list of these features can be found in the feature list.

### How the data are transformed by the script:
1.   The data are read in and simplified for processing
2.   We define the wanted features, concentrating on standard deviations and        means.
3.   Only the data relevant for the standard deviations and means are read in       to safe memory.
4.   Perform steps 1-3 for the test and the training set.
5.   Transform activity names into factors.
6.   Reshape the data so that subjects and activities become a variables of two      independent columns.
7.   Finally safe the data.


## Code Process Below

#Checking for the existence of the data directory
if(!file.exists("data")){
     dir.create("data")
}

#Download the zip file from the URL given
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/UCI_HAR_Dataset.zip", method = "curl")
unzip("./data/UCI_HAR_Dataset.zip")

#Reading and Converting the Data set for Features
Dataset_features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
Dataset_features <- as.character(Dataset_features[,2])

#Reading and Converting the Data set for Train
Dataset_Subject_Train <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')
Dataset_Y_Train <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
Dataset_X_Train <- read.table('./UCI HAR Dataset/train/X_train.txt')

#Combine all data sets in Train
Dataset_Train_Combine <- data.frame(Dataset_Subject_Train, Dataset_Y_Train, Dataset_X_Train)

#Giving names to the elements in the Train combined data set
names(Dataset_Train_Combine) <- c('Subject','Activity', 'Features')

#Reading and Converting the Data set for Test
Dataset_Subject_Test <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')
Dataset_Y_Test <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
Dataset_X_Test <- read.table('./UCI HAR Dataset/test/X_test.txt')

#Combine all data sets in Train
Dataset_Test_Combine <-  data.frame(Dataset_Subject_Test, Dataset_Y_Test, Dataset_X_Test)

#Giving names to the elements in the Train combined data set
names(Dataset_Test_Combine) <- c('Subject','Activity', 'Features')


#Merges the train and the test sets to create one data set
Combine_Train_and_Test_Dataset <- rbind(Dataset_Train_Combine, Dataset_Test_Combine)

# Question 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 

columnsWithMeanSTD  <- grep('mean|std', Dataset_features)
data_replace <- Combine_Train_and_Test_Dataset[,c(1,2,columnsWithMeanSTD + 2)]

# Question 3 - Use descriptive activity names to name the activities in the data set
Data_activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
Data_activity_labels<- as.character(Data_activity_labels[,2])

data_replace$Activity <- Data_activity_labels[data_replace$Activity]


# Question 4 - Appropriately labels the data set with descriptive variable names.
New_Name <- names(data_replace)
New_Name<-gsub("Acc", "Accelerometer", New_Name)
New_Name<-gsub("Gyro", "Gyroscope", New_Name)
New_Name<-gsub("BodyBody", "Body", New_Name)
New_Name<-gsub("Mag", "Magnitude", New_Name)
New_Name<-gsub("^t", "Time", New_Name)
New_Name<-gsub("^f", "Frequency", New_Name)
New_Name<-gsub("tBody", "TimeBody", New_Name)
New_Name<-gsub("-mean()", "Mean", New_Name, ignore.case = TRUE)
New_Name<-gsub("-std()", "STD", New_Name, ignore.case = TRUE)
New_Name<-gsub("-freq()", "Frequency", New_Name, ignore.case = TRUE)
New_Name<-gsub("angle", "Angle", New_Name)
New_Name<-gsub("gravity", "Gravity", New_Name)

names(data_replace) <- New_Name


# Question 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
Tidy_Data <- aggregate(data_replace[,3:81], by = list(activity = data_replace$Activity, subject = data_replace$Subject),FUN = mean)
write.table(x = Tidy_Data, file = "./data/tidy_data.txt", row.names = FALSE)







