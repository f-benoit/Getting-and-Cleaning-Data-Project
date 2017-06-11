library("data.table")
library("plyr")

##############################   Load data   ##############################
#Load activity_labels
activityLabels <- read.table("./project/UCI HAR Dataset/activity_labels.txt")

#Load features + filtered features (std and mean )
features <- read.table("./project/UCI HAR Dataset/features.txt")[,2]
filteredFeatures <- grepl("mean|std", features)

#Load test data
XTest <- read.table("./project/UCI HAR Dataset/test/X_test.txt")
yTest <- read.table("./project/UCI HAR Dataset/test/y_test.txt")
subjectTest <- read.table("./project/UCI HAR Dataset/test/subject_test.txt")

#Load train data
XTrain <- read.table("./project/UCI HAR Dataset/train/X_train.txt")
yTrain <- read.table("./project/UCI HAR Dataset/train/y_train.txt")
subjectTrain <- read.table("./project/UCI HAR Dataset/train/subject_train.txt")

##############################   Rename data   ##############################
#4 Appropriately labels the data set with descriptive variable names.
#for the sake of clarity I rename the column first
colnames(XTest)<-features
colnames(XTrain)<-features
colnames(yTest) <- "activityID"
colnames(yTrain) <- "activityID"
colnames(subjectTest)<-"subjectID"
colnames(subjectTrain)<-"subjectID"

##############################   Extract mean and standard deviation   #############################
# 2 Extracts only the measurements on the mean and standard deviation for each measurement.
# For the sake of performance and memory I remove all non std and mean columns before the merging
XTest<-XTest[,filteredFeatures]
XTrain<-XTrain[,filteredFeatures]



##############################   Merging   #############################
#combine the test data 
testData<-cbind(XTest,yTest,subjectTest)
#combine the training data
trainData<-cbind(XTrain,yTrain,subjectTrain)
#1 Merges the training and the test sets to create one data set.
data<-rbind(testData,trainData)
str(data)


##############################   Mapping activityID with acitivityLabel   ##############################   
#3 Uses descriptive activity names to name the activities in the data set
head(activityLabels)
colnames(activityLabels)<-c("activityID","activityLabel")
data<- join(data, activityLabels, by = "activityID", match = "first")



##############################   Tyding up features   ##############################
# 4 Appropriately labels the data set with descriptive variable names.
names(data)
#remove parenthesis
names(data) <- gsub("\\(|\\)", "", names(data), perl  = TRUE)
#Capitalize first letter after every "-"
names(data)<-gsub("(^|\\p{P})\\-*(.)", "\\1\\U\\2", names(data), perl=T)
#replace  dashes by underscores
names(data)<-gsub("-", "_", names(data))
#remove first underscores
names(data)<-sub("_", "", names(data))
str(data)


##############################   Aggregate data   ##############################
# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
dataAvg<-aggregate(.~ SubjectID + ActivityLabel, data, mean)
dataAvg <- dataAvg[order(dataAvg$SubjectID, dataAvg$ActivityLabel),]
head(dataAvg,12)

write.table(dataAvg, file = "./tidy_data.txt")
