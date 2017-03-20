library(dplyr)
library(plyr)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- file.path(getwd(), "fuci.zip")
download.file(url, f)

unzip("./fuci.zip",list = TRUE)

##read features and activity label data
features<-read.table("./UCI HAR Dataset/features.txt",header = FALSE)
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", sep=" ") 

##read test data
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header=FALSE) 
y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt", header=FALSE)  
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE) 

##read train data
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header=FALSE) 
y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt", header=FALSE) 
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE)  

## set the column names for each test data set
names(x_test)<-features$V2
setnames(y_test, c("V1"), c("activity_label_id"))
setnames(activity_labels, c("V1", "V2"), c("activity_label_id", "activity_label_desc"))
setnames(subject_test, c("V1"), c("subject"))

## set the column names for each test data set
names(x_train)<-features$V2
setnames(y_train, c("V1"), c("activity_label_id"))
setnames(subject_train, c("V1"), c("subject"))

## merge activity label data data
#task 3 : Uses descriptive activity names to name the activities in the data set
y_test_merged <- join(y_test, activity_labels)
y_train_merged <- join(y_train, activity_labels)

## merge test data
df <- data.frame(c("TEST"))
names(df) <- c("test_train")
test_data_merged <- cbind(df, cbind(cbind(subject_test,y_test_merged), x_test))
test_data_merged$test_train <- "TEST"
##merge train data
df <- data.frame(c("TRAIN"))
names(df) <- c("test_train")
train_data_merged <- cbind(df, cbind( cbind(subject_train,y_train_merged), x_train))
mutate(train_data_merged, test_train = "TRAIN")

#task 1 : merge the training and the test sets
train_test_merge <- rbind(test_data_merged,train_data_merged)
#task 2 : select only the mean and standard deviation for each measurement 
train_test_mean_std <- cbind( train_test_merge[, c("test_train", "subject", "activity_label_desc")], 
                              train_test_merge[ , grepl( "mean|std" , names(train_test_merge))])
#task 3 : Uses descriptive activity names to name the activities in the data set
# this was already done above when defining variables y_test_merged and y_train_merged

#task 4 : Appropriately labels the data set with descriptive variable names.
# this was already done in the code above
write.table(train_test_mean_std, './train_test_mean_std.txt', row.names = F)

#task 5 :From the data set in step 4, creates a second, independent tidy data set with the average 
## of each variable for each activity and each subject.
tidydata <- train_test_mean_std%>%group_by(subject,activity_label_desc)%>%summarise_all(mean)
tidydata$test_train <- NULL
## another way to do it; which is more code to type
##avg <- aggregate(x=train_test_mean_std, by=list(activities=train_test_mean_std$activity_label_desc, subj=train_test_mean_std$subject), FUN=mean)
##avg <- avg[, !(colnames(avg) %in% c("subj", "activity"))]
##View(avg)
write.table(tidydata, './subj_activity_mean.txt', row.names = F)



