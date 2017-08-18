---
title: "CodeBook"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
{% include navigation.html context="/" %}

new 

The experiments have been carried out with a group of 30 volunteers.

Each person performed six activities:
	(WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)

The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The given dataset included the following files:
==
- `features.txt`: List of all features.

- `train/X_train.txt`: Training set.

- `train/y_train.txt`: Training labels.

- `test/X_test.txt`: Test set.

- `test/y_test.txt`: Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- `train/subject_train.txt`: Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

==

All the data sets needed are read into memory in the first few lines of code. 

Initial task is to merge all appropriate datasets to create a master dataset.

First the testdata_Subject(`subject_test.txt`) and testdata_label(`y_test.txt`) was bounded by columns and finally bounded with readings data set(`X_test.txt`) to create final test master data.

Then the traindata_Subject(`subject_train.txt`) and traindata_label(`X_test.txt`) was bounded by columns and finally bounded with readings data set(`y_test.txt`) to create final train master data.

Finally, the both master data were bounded in rows to create the final MasterDataSet
-------
Tidy dataset demands the columns to be named properly. 

To create a column_names vector, the features were taken from `features.txt` file.
To name the two columns bounded in the final data set in previous step (testdata_Subject and testdata_label), two column names were appended at the beginning of the column_names vector.

The column_names vector was set as the colnames of MasterDataSet.

Tidy dataset demands the variables to be named properly.

The activity numbers (1..6) were replaced with meaningful activity names. 
--
Measurements only in the mean and standard deviation were taken and stored in the variable meanData.
Here mean data is any column with the name mean or std(standard deviation) in it. it can be at the beginning or middle of the name of reading.
--
To get the average of each activity of each subject, the meanDataSet was first subsetted into different combinations (180 combinations) of SubjectID + Activity pairs using spilt function.

using lapply the average was found for each combinations.

The resulting list was converted to data frame and stored in a file. 
