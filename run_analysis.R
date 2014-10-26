#setwd("D:/Coursera/Data science Specialization/Getting and Cleaning Data/UCI HAR Dataset")

library(reshape2)  # used melt and dcast functions to obtain tidy data

#loaded features and picked variables containing names of only mean and std and not meanFreq
features <- read.table("features.txt")
features <- as.character(features$V2)
featuresSelection <- features[(grepl("mean()",features) | grepl("std()",features) ==TRUE)]
featuresSelectionFinal <- featuresSelection[!(grepl("meanFreq",featuresSelection))]



#loaded testing data and subsetted data by picking columns of only mean and std
test_data <- read.table("./test/X_test.txt")
names(test_data) <- features
test_data_ver1 <- test_data[,featuresSelectionFinal]

#loaded training data and subsetted data by picking columns of only mean and std
train_data <- read.table("./train/X_train.txt")
names(train_data) <- features
train_data_ver1 <- train_data[,featuresSelectionFinal]


#read activity labels corresponding to testing data and named the vector as activity
test_data_activity <- read.table("./test/y_test.txt")
names(test_data_activity) <- c("activity")

#read activity labels corresponding to Training data and named the vector as activity
train_data_activity <- read.table("./train/y_train.txt")
names(train_data_activity) <- c("activity")

#read Subjects data corresponding to Testing data and named the vector as subject
test_data_subject <-  read.table("./test/subject_test.txt")
names(test_data_subject) <- c("subject")

#read Subjects data corresponding to Training data and named the vector as subject
train_data_subject <- read.table("./train/subject_train.txt")
names(train_data_subject) <- c("subject")

#added columns subject and and activity to corresponding testing and training data subsets 
test_data_final <- cbind(test_data_subject,test_data_activity,test_data_ver1)
train_data_final <- cbind(train_data_subject,train_data_activity,train_data_ver1)

#combined training and testing datasets to get full data set
full_data <- rbind(test_data_final,train_data_final)

#deletig temporary objects from memory
rm(test_data_subject)
rm(test_data_activity)
rm(test_data_ver1)
rm(train_data_subject)
rm(train_data_activity)
rm(train_data_ver1)
rm(featuresSelection)
rm(test_data)
rm(train_data)


#Melted data and transformed to obtain tidy data of form: subject,activity, average(of each variable)
fullMelt <- melt(full_data,id=c("subject","activity"),measure.vars=featuresSelectionFinal)
tidy_data <- dcast(fullMelt,subject+activity~variable,mean)
 
#replaced activity code in the data with the activity descriptions
activity_label <-  read.table("./activity_labels.txt")
factorvar <- factor(tidy_data$activity)
levels(factorvar) <- activity_label[,2]
tidy_data$activity <- factorvar
dimensions <- dim(tidy_data)


#compressed numeric values in tidy data set to have only 3 significance digits
tidy_data[,3:dimensions[2]] <-signif(tidy_data[,3:dimensions[2]],digits=3)

#Exported data to pwd as Tidy_Data text file
write.table(tidy_data,file="Tidy_Data.txt",sep=",",col.names=TRUE,row.names=FALSE)

