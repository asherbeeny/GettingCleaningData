# Data Set-up and Load: 
	## create a data directory if it does not exist, go there
	if(!file.exists("data")) dir.create("data")
	setwd("data")
	# setwd("D:/R/Getting and Cleaning Data/Peer3/data")

	## download and unzip the data file, remove the temporary zip file
	fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	download.file(fileUrl,"temp.zip")
	unzip("temp.zip")
	file.remove("temp.zip")

# 1. Merges the training and the test sets to create one data set.
	## load training data and the feature names
		### data and field names
		x_set_train <- read.table("UCI HAR Dataset/train/X_train.txt")
		x_names <- read.table("UCI HAR Dataset/features.txt")
		x_names_vect <- as.vector(as.matrix(x_names[,2]))
		names(x_set_train) <- x_names_vect
		### activities
		y_set_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
		names(y_set_train) <- c("Activity")
		### subjects
		s_set_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
		names(s_set_train) <- c("Subjects")
	
	## load test data	
		### data and field names
		x_set_test <- read.table("UCI HAR Dataset/test/X_test.txt")
		names(x_set_test) <- x_names_vect
		### activities
		y_set_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
		names(y_set_test) <- c("Activity")
		### subjects
		s_set_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
		names(s_set_test) <- c("Subjects")

	## combine data sets 
	x_set <- rbind(x_set_train, x_set_test)
	y_set <- rbind(y_set_train, y_set_test)
	s_set <- rbind(s_set_train, s_set_test)
	alldata <- cbind(x_set,y_set,s_set)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
	## extract only columns with mean() and std() in the name
	use_indx <- regexpr("*mean\\(\\)*",x_names_vect)>0 | regexpr("*std\\(\\)*",x_names_vect)>0
	Dataset_Means_SDs <- cbind(x_set[,use_indx],y_set,s_set)
	
	## the dataset Dataset_Means_SDs is the first "tidy dataset" needed
	
# 3. Uses descriptive activity names to name the activities in the data set
	## obtain Activity names and create a factor variable 
	y_factornames <- read.table("UCI HAR Dataset/activity_labels.txt")
	y_set_fact <- factor(as.vector(as.matrix(y_set[,1])),labels=y_factornames[,2])
	
	## replace the y-variable data in the overall and the Means-and-SDs datasets with the new factor variable
	alldata <- cbind(x_set,s_set,y_set_fact)
	names(alldata)[length(names(alldata))] <- "Activity"
	
	Dataset_Means_SDs <- cbind(x_set[,use_indx],s_set,y_set_fact)	
	names(Dataset_Means_SDs)[length(names(Dataset_Means_SDs))] <- "Activity"
	
# 4. Appropriately labels the data set with descriptive activity names. 
	# this has been handeled by applying the "features.txt" feature names across all datasets
	# it remains only to clean up the labels using some clever string parsing
	
	## desired output sample:
	### tBodyAcc-mean()-X
	### Mean of BodyAcc along X axis (time domain) 
	### fBodyAccJerk-std()-X
	### Standard Deviation of BodyAccJerk along X axis (freq domain)	
	temp_data <- x_set[,use_indx]
	oldnames <- names(temp_data)
	oldnames <- gsub("\\)","",gsub("\\(","",oldnames))
	oldnames <- strsplit(oldnames,"-")
	newnames = rep("colum name",length(oldnames));
	
	for (i in 1 : length(oldnames)) {
		tempvar <- oldnames[[i]][1]
		kpi <- oldnames[[i]][2]
		axs <- oldnames[[i]][3]
		
		if (substr(tempvar,1,1)=="t") {
			varname <- substr(tempvar, 2, nchar(tempvar))
			domain <-"(time domain)" 
		} else if (substr(tempvar,1,1)=="f") {
			varname <- substr(tempvar, 2, nchar(tempvar))
			domain <-"(frequency domain)"  
		} else {
			varname <- tempvar
			domain <- ""
		}
	
		if (!is.null(axs) & !is.na(axs)) {
		axs <- paste("along the", axs, "axis",sep=" ")
		} else {
		axs <- ""
		}
		
		if (kpi=="mean") {
		kpi <- "Mean of" 
		} else if (kpi=="std") {
		kpi <- "Standard Deviation of"
		}
	 
		colname <- paste(kpi, varname, axs, domain, sep=" ")
		newnames[i] <- colname
	}
	
	## apply the new parsed names to the dataset
	names(temp_data) <- newnames
	Dataset_Means_SDs <- cbind(temp_data,s_set,y_set_fact)	
	names(Dataset_Means_SDs)[length(names(Dataset_Means_SDs))] <- "Activity"
	
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject
	## since this is not specified, I'm going to assume that we want the average of all numerical varibles 
	## (not just the mean / std that we were limited to in the above dataset)
	## we'll create a matrix of all numerical variables as columns
	## and the activity - subject groups as rows

	splitup <- split(x_set,list(alldata$Activity,alldata$Subjects))
	size_x <- length(splitup)
	names_x <- names(splitup)
	size_y <- length(x_set)
	names_y <- names(x_set)
	
	Dataset_avg_byAct_bySubJ = matrix(rep(0,size_x*size_y),nrow=size_x,ncol=size_y)
	rownames(Dataset_avg_byAct_bySubJ) <- names_x
	colnames(Dataset_avg_byAct_bySubJ) <- names_y
	
	for (i in 1:length(splitup)) {
	Dataset_avg_byAct_bySubJ[i,] <- as.vector(sapply(splitup[[i]],mean,rm.na=TRUE))
	}
	
	## the dataset Dataset_avg_byAct_bySubJ is the second "tidy dataset" needed
	
# Export the datasets
	write.table(Dataset_Means_SDs, file="Dataset_Means_SDs.txt", sep=",", row.names = TRUE)
	write.table(Dataset_avg_byAct_bySubJ, file="Dataset_avg_byAct_bySubJ.txt", sep=",", row.names = TRUE, col.names = TRUE)
