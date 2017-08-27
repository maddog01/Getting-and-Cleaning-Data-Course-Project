This READ.ME.md document is to explain how all the scripts work and how
they are connected:

How the data was collected and units of measurement:
====================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING\_UPSTAIRS, WALKING\_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist.
====================================================================================================================================================================================================================================================================================

Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets,
=======================================================================================================================================================================================================================================================================================

where 70% of the volunteers was selected for generating the training data and 30% the test data.
================================================================================================

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components,
====================================================================================================================================================================================================================================================================================

was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by
================================================================================================================================================================================================================================================================================

calculating variables from the time and frequency domain.
=========================================================

(The phrase:The program, refers to the r program Final project.)
================================================================

The program first downloaded the zip file from
==============================================

unzip("C:/Users/mmads/Desktop/Getting and Cleaning Data/Final
Project/getdata%2Fprojectfiles%2FUCI HAR Dataset.zip")

And opens it in R-studio
========================

Then, the program opened the files, each as a data table and saved them under the following variable names:
===========================================================================================================

data\_x\_test \<- read.table(file = "C:/Users/mmads/Desktop/Getting and
Cleaning Data/Final Project/UCI HAR Dataset/test/X\_test.txt",

                       stringsAsFactors = FALSE)

data\_y\_test \<- read.table(file = "C:/Users/mmads/Desktop/Getting and
Cleaning Data/Final Project/UCI HAR Dataset/test/y\_test.txt",

                          stringsAsFactors = FALSE)

data\_subj\_test \<- read.table(file = "C:/Users/mmads/Desktop/Getting
and Cleaning Data/Final Project/UCI HAR Dataset/test/subject\_test.txt",

                          stringsAsFactors = FALSE)

data\_x\_train \<- read.table(file = "C:/Users/mmads/Desktop/Getting and
Cleaning Data/Final Project/UCI HAR Dataset/train/X\_train.txt",

                          stringsAsFactors = FALSE)

data\_y\_train \<- read.table(file = "C:/Users/mmads/Desktop/Getting and
Cleaning Data/Final Project/UCI HAR Dataset/train/y\_train.txt",

                          stringsAsFactors = FALSE)

data\_subj\_train \<- read.table(file = "C:/Users/mmads/Desktop/Getting
and Cleaning Data/Final Project/UCI HAR
Dataset/train/subject\_train.txt",

                             stringsAsFactors = FALSE)

Next, the program creates four different subsets for building one data frame which contains all the imported data.
==================================================================================================================

sub\_y\_test \<- cbind(data\_subj\_test,data\_y\_test)

sub\_y\_x\_test \<-cbind(sub\_y\_test,data\_x\_test)

sub\_y\_train \<- cbind(data\_subj\_train,data\_y\_train)

sub\_y\_x\_train \<-cbind(sub\_y\_train,data\_x\_train)

data\_train\_test \<- rbind(sub\_y\_x\_test, sub\_y\_x\_train)

Next the program imports the features file which has a table containing the variable names of the columns of our desired data frame.
====================================================================================================================================

feat\_tbl \<- read.table(file = "C:/Users/mmads/Desktop/Getting and
Cleaning Data/Final Project/UCI HAR Dataset/features.txt",

                       stringsAsFactors = FALSE)

sub\_feat\_tbl \<- feat\_tbl\$V2

Next, the program sifts through each observation and identifies the rows that contain either mean() or std().
=============================================================================================================

feat\_rw\_mean \<- grep("mean()", x = sub\_feat\_tbl)

feat\_rw\_std \<- grep("std()", x = sub\_feat\_tbl)

Next, the program saves the numbers from the feat\_rw\_mean and the feat\_rw\_std variables and saves those numbers as one vector.
==================================================================================================================================

It also adds 2 to each element so that the names of each variable will line up with their accompanying data in the data\_train\_test data frame and labels.
===========================================================================================================================================================

feat\_rw\_mean\_std \<- sort(c(feat\_rw\_mean,feat\_rw\_std))

feat\_rw\_mean\_std \<- 2 + feat\_rw\_mean\_std

Next, the program creates a subset of the data\_train\_test data frame.
=======================================================================

This subset is comprised only of the rows matching the feat\_rw\_mean\_std vector. This new data frame is labeled data\_mean\_std.
==================================================================================================================================

data\_mean\_std \<- data\_train\_test[c(1,2,feat\_rw\_mean\_std)]

Next, the program downloads the 6 labels describing each activity tested from the activity\_labels file.
========================================================================================================

labels \<- read.table(file = "C:/Users/mmads/Desktop/Getting and
Cleaning Data/Final Project/UCI HAR Dataset/activity\_labels.txt",

                     stringsAsFactors = FALSE)

Next, the program runs the switch\_num\_lab function which replaces the numerical value in column 2 of the data\_mean\_std with its appropriate activity description.
=====================================================================================================================================================================

        switch\_num\_lab &lt;- function(num)

                if (num == 1) {

                        answer &lt;- &quot;WALKING&quot;

                } else if ( num == 2) {

                        answer &lt;- &quot;WALKING\_UPSTAIRS&quot;

                } else if (num == 3) {

                        answer &lt;- &quot;WALKING\_DOWNSTAIRS&quot;

                } else if (num == 4) {

                        answer &lt;- &quot;SITTING&quot;

                } else if (num == 5) {

                        answer &lt;- &quot;STANDING&quot;

                } else if (num == 6)

                        answer &lt;- &quot;LAYING&quot;

        }

        activity &lt;- sapply(data\_mean\_std[,2],switch\_num\_lab)

        data\_mean\_std[,2] &lt;- activity

Next, the program replaces the names of the first 2 columns of the data set data\_mean\_std with the first column being named volunteer.id.num and the 2nd column being named activity.
=======================================================================================================================================================================================

colnames(data\_mean\_std)[colnames(data\_mean\_std)=="V1.1"] \<-
"activity"

colnames(data\_mean\_std)[colnames(data\_mean\_std)=="V1"] \<-
"volunteer.id.num"

Next, the program creates a subset of the feat\_tbl data frame called col\_names.
=================================================================================

This subset the corresponding names from the feat\_tbl variable.
================================================================

It uses the vector c(feat\_rw\_mean\_std -2) in order to decide which labels to use.
====================================================================================

The -2 is because the labelling will begin with the 3rd column of the data\_mean\_std data frame.
=================================================================================================

col\_names \<- feat\_tbl\$V2[c(feat\_rw\_mean\_std - 2)]

Next, the program calls on the package data.table and uses the command setnames to replace the names of columns 3 through 81 of the data frame data\_mean\_std with the corresponding names from the col\_names subset.
=======================================================================================================================================================================================================================

        library(data.table)

        setnames(data\_mean\_std, old = colnames(data\_mean\_std[,3:81]),

                new = col\_names)

Next, the program uses the gsub command to clean up the column names by clarifying all abbreviations, deleting repetition, and replacing all capital letters with lower letters instead.
========================================================================================================================================================================================

        names(data\_mean\_std) &lt;- gsub(&quot;Acc&quot;, &quot;acceleration&quot;, names(data\_mean\_std))

        names(data\_mean\_std) &lt;- gsub(&quot;^t&quot;, &quot;time&quot;, names(data\_mean\_std))

        names(data\_mean\_std) &lt;- gsub(&quot;^f&quot;, &quot;frequency&quot;, names(data\_mean\_std))

        names(data\_mean\_std) &lt;- gsub(&quot;BodyBody&quot;, &quot;body&quot;, names(data\_mean\_std))

        names(data\_mean\_std) &lt;- gsub(&quot;Mag&quot;, &quot;magnitude&quot;, names(data\_mean\_std))

        names(data\_mean\_std) &lt;- gsub(&quot;A&quot;, &quot;acceleration&quot;, names(data\_mean\_std))

        names(data\_mean\_std) &lt;- tolower(names(data\_mean\_std))

        names(data\_mean\_std) &lt;- gsub(&quot;timeime&quot;, &quot;time&quot;, names(data\_mean\_std))

        names(data\_mean\_std) &lt;- gsub(&quot;frequencyrequency&quot;,

                                     &quot;frequency&quot;, names(data\_mean\_std))

Next, the program uses the ddply command to take the data\_mean\_std data frame and split the data frame into subgroups according to the volunteer.id.num and then the activity.
================================================================================================================================================================================

It then finds the mean values for each column within each subgroup and combines the results into one new data frame labelled tidy\_data\_avg.
=============================================================================================================================================

        tidy\_data\_avg &lt;- ddply(data\_mean\_std, c(&quot;volunteer.id.num&quot;

                                                , &quot;activity&quot;), numcolwise(mean)

Finally, the program saves the tidy\_data\_avg data frame as a txt file labelled tidydata.txt.
==============================================================================================

        write.table(tidy\_data\_avg, file=&quot;tidydata.txt&quot;)
