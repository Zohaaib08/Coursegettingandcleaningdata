#Step1 readfiles and label data frames with appropriate variable names 

features <- read.table("features.txt",  stringsAsFactors=FALSE)
x_test <- read.table("./test/X_test.txt", sep = "", col.names = features[,2])
x_train <- read.table("./train/X_train.txt", sep = "", col.names = features[,2])
y_test <- read.table("./test/Y_test.txt", col.names = "Activity")
y_train <- read.table("./train/Y_train.txt", col.names = "Activity")
activitylab <- read.table("activity_labels.txt")
subject_test <- read.table("test/subject_test.txt", col.names = "Subject")
subject_train <- read.table("train/subject_train.txt", col.names = "Subject")

#Step2 Merging the training and the test sets to create one data set

combdata1 <- cbind(x_test,y_test)
combdata2 <- cbind(x_train,y_train)
combdata3 <- rbind(combdata1,combdata2)
x <- rbind(y_test,y_train)

#Step3 Extracting only the measurements on the mean and standard deviation for each measurement

library(dplyr)
combdata3 <- combdata3[,grep("mean|std",features[,2])]
subcomb <- rbind(subject_test,subject_train)
step2 <- cbind(combdata3,subcomb,x)


#Step4 Using descriptive activity names to name the activities in the data set

step2$Activity <- sub(1,"WALKING",step2$Activity)
step2$Activity <- sub(2,"WALKING_UPSTAIRS",step2$Activity)
step2$Activity <- sub(3,"WALKING_DOWNSTAIRS",step2$Activity)
step2$Activity <- sub(4,"SITTING",step2$Activity)
step2$Activity <- sub(5,"STANDING",step2$Activity)
step2$Activity <- sub(6,"LAYING",step2$Activity)

#Step5 Creating a second, independent tidy data set with the average of each variable for each activity and each subject.

step3 <- step2 %>% group_by(Subject) %>% group_by(Activity, .add = TRUE) %>% summarize_each(funs(mean))

