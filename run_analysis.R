library(data.table)
#library(dplyr)
setwd("./UCI HAR Dataset")

feat_tbl <- read.table("features.txt",stringsAsFactors = FALSE)
labels <- read.table("activity_labels.txt")

setwd("./train")

data_x_train <- read.table("X_train.txt")
data_y_train <- read.table("y_train.txt")
data_subj_train <- read.table("subject_train.txt")

setwd("../test")

data_x_test <- read.table("X_test.txt")
data_y_test <- read.table("y_test.txt")
data_subj_test <- read.table("subject_test.txt")

sub_y_test <- cbind(data_subj_test,data_y_test)
sub_y_x_test <-cbind(sub_y_test,data_x_test)
sub_y_train <- cbind(data_subj_train,data_y_train)
sub_y_x_train <-cbind(sub_y_train,data_x_train)
data_train_test <- rbind(sub_y_x_test, sub_y_x_train)


sub_feat_tbl <- feat_tbl$V2


feat_rw_mean <- grep("mean()", x = sub_feat_tbl)
feat_rw_std <- grep("std()", x = sub_feat_tbl)
feat_rw_mean_std <- sort(c(feat_rw_mean,feat_rw_std))
feat_rw_mean_std <- 2 + feat_rw_mean_std

data_mean_std <- data_train_test[c(1,2,feat_rw_mean_std)]


switch_num_lab <- function(num) {
        
        if (num == 1) {
                answer <- "WALKING"
        } else if ( num == 2) {
                answer <- "WALKING_UPSTAIRS"
        } else if (num == 3) {
                answer <- "WALKING_DOWNSTAIRS"
        } else if (num == 4) {
                answer <- "SITTING"
        } else if (num == 5) {
                answer <- "STANDING"
        } else if (num == 6) 
                answer <- "LAYING"
}

activity <- sapply(data_mean_std[,2],switch_num_lab)
data_mean_std[,2] <- activity

colnames(data_mean_std)[colnames(data_mean_std)=="V1.1"] <- "activity"
colnames(data_mean_std)[colnames(data_mean_std)=="V1"] <- "volunteer.id.num"
col_names <- feat_tbl$V2[c(feat_rw_mean_std - 2)]

library(data.table)
setnames(data_mean_std, old = colnames(data_mean_std[,3:81]),
         new = col_names)

names(data_mean_std) <- gsub("Acc", "acceleration", names(data_mean_std))
names(data_mean_std) <- gsub("^t", "time", names(data_mean_std))
names(data_mean_std) <- gsub("^f", "frequency", names(data_mean_std))
names(data_mean_std) <- gsub("BodyBody", "body", names(data_mean_std))
names(data_mean_std) <- gsub("Mag", "magnitude", names(data_mean_std))
names(data_mean_std) <- gsub("A", "acceleration", names(data_mean_std))
names(data_mean_std) <- tolower(names(data_mean_std))
names(data_mean_std) <- gsub("timeime", "time", names(data_mean_std))
names(data_mean_std) <- gsub("frequencyrequency",
                             "frequency", names(data_mean_std))

tidy_data_avg <- ddply(data_mean_std, c("volunteer.id.num"
                                        , "activity"), numcolwise(mean))

write.table(tidy_data_avg, file="tidydata.txt")


