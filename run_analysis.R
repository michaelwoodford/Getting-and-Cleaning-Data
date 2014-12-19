## Getting and cleaning data - run_analysis

## first, read in the data
#training_data <- read.csv("dataset/UCI HAR Dataset/train/subject_train.txt",sep="",header=FALSE)
#training_data2 <- read.csv("dataset/UCI HAR Dataset/train/X_train.txt",sep="",header=FALSE)
#training_data3 <- read.csv("dataset/UCI HAR Dataset/train/Y_train.txt",sep="",header=FALSE)

training_data <- read.csv("subject_train.txt",sep="",header=FALSE)
training_data2 <- read.csv("X_train.txt",sep="",header=FALSE)
training_data3 <- read.csv("Y_train.txt",sep="",header=FALSE)

#testing_data <- read.csv("dataset/UCI HAR Dataset/test/subject_test.txt",sep="",header=FALSE)
#testing_data2 <- read.csv("dataset/UCI HAR Dataset/test/X_test.txt",sep="",header=FALSE)
#testing_data3 <- read.csv("dataset/UCI HAR Dataset/test/Y_test.txt",sep="",header=FALSE)

testing_data <- read.csv("subject_test.txt",sep="",header=FALSE)
testing_data2 <- read.csv("X_test.txt",sep="",header=FALSE)
testing_data3 <- read.csv("Y_test.txt",sep="",header=FALSE)

#activities <- read.csv("dataset/UCI HAR Dataset/activity_labels.txt",sep="",header=FALSE)
#features <- read.csv("dataset/UCI HAR Dataset/features.txt",sep="",header=FALSE)

activities <- read.csv("activity_labels.txt",sep="",header=FALSE)
features <- read.csv("features.txt",sep="",header=FALSE)


## clean features dataset
features$V2 <- gsub("-mean","Mean",features$V2)
features$V2 <- gsub("-std","Std",features$V2)
features$V2 <- gsub("[-()]","",features$V2)

## colate into single data frame
training <- cbind(training_data2,training_data3,training_data)
testing <- cbind(testing_data2,testing_data3,testing_data)
col_data <- rbind(training,testing)

## clear space
rm(training_data,training_data2,training_data3,testing_data,testing_data2,testing_data3)


#to_match <- c("Mean","Std")
#matches <- unique(grep(paste(to_match,collapse="|"),features$V2,value=TRUE))
matches <- grep(".*Mean.*|.*Std.*",features$V2)
features <- features[matches,]

## Trim data to wanted columns only
col_data <- col_data[,c(matches,562,563)]

## Set the column names of the col_data to the values in features and 'activity', 'subject
colnames(col_data) <- c(features$V2,"Activity","Subject")

## Activity Labels - 1 WALKING 2 WALKING_UPSTAIRS 3 WALKING_DOWNSTAIRS 4 SITTING 5 STANDING 6 LAYING
## Replace activity integers with character description of activity
col_data$Activity <- activities[match(col_data$Activity,activities[,1]),2]



## order the activites alphabetically
activities <- activities[order(activities[,2]),]

## initialise the new data frame which has the mean values for each column
average_data <- data.frame(Subject = rep(min(col_data$Subject):max(col_data$Subject),each=nrow(activities)),
                       Activity = rep(activities[,2],times=(max(col_data$Subject)-min(col_data$Subject)+1)))

## Convert subject to a factor
col_data$Subject <- factor(col_data$Subject)

## Loop through each column except for activity and subject, create a mean matrix using tapply and convert to 
## a vector then use cbind to add to the new_data data frame
for (i in 1:(ncol(col_data)-2)){
  tempstore <- tapply(col_data[,i],list(col_data$Activity,col_data$Subject),mean)
  tempstore <- as.data.frame(as.vector(tempstore))
  colnames(tempstore) <- colnames(col_data)[i]
  
  average_data <- cbind(average_data,tempstore)
}

write.table(average_data,file="Getting and Cleaning Data - Average Data.txt",row.names=FALSE)
