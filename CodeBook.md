The purpose of this CodeBook.md document is to describe thee variables,
the data, and any transformations or work performed in the Final
Project.r program. First, the file calls on the package data.table Then,
the program opened the files, each as a data table and saved them under
the following variable names: Variables: feat\_tbl: the imported table
from the features file labels: imported data table from the
activity\_labels file data\_x\_train: imported data table from the
X\_train file data\_y\_train: imported data column from the y\_train
file data\_subj\_train: imported data column from the subject\_train
file data\_x\_test: imported data table from the X\_test file
data\_y\_test: imported data column from the y\_test file
data\_subj\_test: imported data column from the subject\_test file

Next, the program creates four different subsets for building one data
frame which contains all of the imported data. sub\_y\_test: the data
frame resulting from binding data\_subj\_test with data\_y\_test
sub\_y\_x\_test: the data frame resulting from binding sub\_y\_test with
data\_x\_test sub\_y\_train: the data frame resulting from binding
data\_subj\_train with data\_y\_train sub\_y\_x\_train: the data frame
resulting from binding sub\_y\_xt\_test with sub\_y\_x\_train
data\_train\_test: the data frame resulting from binding sub\_y\_x\_test
with sub\_y\_x\_train

Next the program uses the features file which has a table containing the
variable names of the columns of our desired data frame. sub\_feat\_tbl:
the 2nd column from the feat\_tbl variable

Next, the program sifts through each observation and identifies the rows
that contain either mean() or std(). feat\_rw\_mean: a numerical value
indicating the rows in the sub\_feat\_tbl containing the phrase mean().
feat\_rw\_std: a numerical value indicating the rows in the
sub\_feat\_tbl containing the phrase std().

Next, the program saves the numbers from the feat\_rw\_mean and the
feat\_rw\_std variables and saves those numbers as one vector. It also
adds 2 to each element so that the names of each variable will line up
with their accompanying data in the data\_train\_test data frame and
labels. feat\_rw\_mean\_std: a vector containing all of the values (each
increased by +2) from feat\_rw\_mean and feat\_rw\_std.

Next, the program creates a subset of the data\_train\_test data frame.
This subset is comprised only of the rows matching the
feat\_rw\_mean\_std vector. This new data frame is labeled
data\_mean\_std. data\_mean\_std: a subset of data\_train\_test
comprised of columns given selected from the column position numbers
equal to the vector c(1,2,feat\_rw\_mean\_std). Next, the program
downloads the 6 labels describing each activity tested from the
activity\_labels file. .

Next, the program runs the switch\_num\_lab function which replaces the
numerical value in column 2 of the data\_mean\_std with its appropriate
activity description and saves the result as the variable activity.
activity: the variable name for the single column summarizing the
appropriate activity description matching the numerical values from the
2nd column of the data\_mean\_std data frame. Next, the program replaces
the names of the first 2 columns of the data set data\_mean\_std with
the first column being named volunteer.id.num and the 2nd column being
named activity. Next, the program creates a subset of the feat\_tbl data
frame called col\_names. This subset the corresponding names from the
feat\_tbl variable. It uses the vector c(feat\_rw\_mean\_std -2) in
order to decide which labels to use. The -2 is because the labelling
will begin with the 3rd column of the data\_mean\_std data frame. Next,
the program calls on the package data.table and uses the command
setnames to replace the names of columns 3 through 81 of the data frame
data\_mean\_std with the corresponding names from the col\_names subset.
Next, the program uses the gsub command to clean up the column names by
clarifying all abbreviations, deleting repetition, and replacing all
capital letters with lower letters instead. Next, the program uses the
ddply command to take the data\_mean\_std data frame and split the data
frame into subgroups according to the volunteer.id.num and then the
activity. It then finds the mean values for each column within each
subgroup and combines the results into one new data frame labelled
tidy\_data\_avg. Tidy\_data\_avg: the name of the data frame summarizing
the results of the means of each column, which were first split into
subgroups according to the volunteer.id.num and the activity. Finally,
the program saves the tidy\_data\_avg data frame as a txt file labelled
tidydata.txt.
