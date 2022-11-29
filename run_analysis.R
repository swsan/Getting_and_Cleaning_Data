library(dplyr)

#1. Merges the training and the test sets to create one data set.
df_Xtest <- read.table("./UCI HAR Dataset/test/X_test.txt",header = FALSE)
df_ytest <- read.table("./UCI HAR Dataset/test/y_test.txt",header = FALSE)
df_text_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)

df_Xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt",header = FALSE)
df_ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt",header = FALSE)
df_train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)

df_features <- read.table("./UCI HAR Dataset/features.txt",header = FALSE)
df_activity_level <- read.table("./UCI HAR Dataset/activity_labels.txt",header = FALSE)

df_test = cbind(df_ytest, df_text_subject, df_Xtest)
df_train = cbind(df_ytrain, df_train_subject, df_Xtrain)

df_tt <- rbind(df_test,df_train)

n <- c("label","SubID",df_features[,2])
names(df_tt) <- n

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
feature_selected <- n[grep ("label|SubID|mean.*|std.*",n)]
df_selected <- df_tt %>% select(all_of(feature_selected))


#3. Uses descriptive activity names to name the activities in the data set
measure <- factor(df_selected$label)
levels(measure) <- df_activity_level[,2]
df_selected$label <- measure

#4. Appropriately labels the data set with descriptive variable names. 
# already done at #1

#5. From the data set in step 4, creates a second, independent tidy data set with 
#    the average of each variable for each activity and each subject.
df_selected$SubID_Label <- paste(df_selected$SubID, df_selected$label)

n <- names(df_selected)
Col_Selected <- n[n != "SubID_Label" & n != "label" & n != "SubID"]
df_sum <- df_selected %>% group_by(SubID_Label) %>%
        summarise(across(Col_Selected, mean,na.rm=TRUE))

df_sum  %>% write.table(file="df_sum.txt",row.name=FALSE)


                   



