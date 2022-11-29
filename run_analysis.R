library(dplyr)

#1. Merges the training and the test sets to create one data set.
df_Xtest_tmp <- read.table("./UCI HAR Dataset/test/X_test.txt",header = FALSE)
df_ytest_tmp <- read.table("./UCI HAR Dataset/test/y_test.txt",header = FALSE)
df_Xtrain_tmp <- read.table("./UCI HAR Dataset/train/X_train.txt",header = FALSE)
df_ytrain_tmp <- read.table("./UCI HAR Dataset/train/y_train.txt",header = FALSE)

df_features <- read.table("./UCI HAR Dataset/features.txt",header = FALSE)
df_tt <- rbind(df_Xtest_tmp,df_Xtrain_tmp)

n <- df_features[,2]
names(df_tt ) <- n

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
feature_selected <- n[grep ("mean|std",n)]
df_selected <- df_tt %>% select(all_of(feature_selected))


#3. Uses descriptive activity names to name the activities in the data set
df_activity_level <- read.table("./UCI HAR Dataset/activity_labels.txt",header = FALSE)
df_yy <- rbind(df_ytest_tmp,df_ytrain_tmp)
names(df_yy) <- c("label")

measure <- factor(df_yy$label)
levels(measure) <- df_activity_level[,2]

df_measure <- data.frame(label = measure )
df_yselected <- cbind(df_measure,df_selected)

#4. Appropriately labels the data set with descriptive variable names. 
# already done at #1

#5. From the data set in step 4, creates a second, independent tidy data set with 
#    the average of each variable for each activity and each subject.

df_sum_by_y <- df_yselected %>% group_by(label) %>%
        summarise(across(everything(), list(mean)))
                   



