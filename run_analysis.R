#Preprocessing#

fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

temp<-tempfile()

download.file(fileUrl,temp)

list_files<-unzip(temp)

list_files

unlink(temp)
##

#Uploading datasets into R#
X_train <- read.table("train/X_train.txt", header = F)

X_train_labels <- read.table("train/y_train.txt",header = F, colClasses = "character")

X_test <- read.table("test/X_test.txt", header = F)

X_test_labels <- read.table("test/y_test.txt",header = F, colClasses = "character")

features <- read.table("features.txt", header = F)

activity_labels <- read.table("activity_labels.txt", header = F, colClasses = "character")

subjects_train <- read.table("train/subject_train.txt", header = F, colClasses = "numeric")

subjects_test <- read.table("test/subject_test.txt", header = F, colClasses = "numeric")
##


#1.step#

X_merged <- merge(X_train, X_test, all = TRUE)

##

#2.step#

index_mean <- grep("[Mm]ean", features[,2])

index_std <- grep("[Ss]td", features[,2])

required_colomns <- c(index_mean, index_std)

required_colomns <- sort(required_colomns)

X_merged <- X_merged[,required_colomns]

##

#3.step#
label_f <- function(x){
        x <- activity_labels[x,2]
        x       
}

for(i in 1:length(X_train_labels[,1])){
        X_train_labels[i,1] <- label_f(X_train_labels[i,1])
}

for(i in 1:length(X_test_labels[,1])){
        X_test_labels[i,1] <- label_f(X_test_labels[i,1])
}

activities_merged <- rbind(X_train_labels, X_test_labels)

subjects <- rbind(subjects_train, subjects_test)

X_merged <- data.frame(subjects, activities_merged, X_merged)

##

#4th step#
required_names <- as.character(features[required_colomns,2])

additional <- c("Subjects", "Activity")

required_names <- c(additional, required_names)

names(X_merged) <- required_names

library(dplyr);X_merged <- arrange(X_merged, Subjects)

first_data_set <- X_merged


##

#5.step#
second_data_set <- X_merged

names(second_data_set) <- gsub(x = names(second_data_set), "\\(|\\)|-|,", "")

second_data_set <- mutate(second_data_set, Activity = factor(Activity))

library(reshape2)

melted_data_set <- melt(second_data_set, id = c("Subjects", "Activity"))

casted_data_set <- dcast(melted_data_set, Subjects + Activity ~ variable, mean)

second_data_set <- casted_data_set

second_data_set$Subjects <- as.numeric(as.character(second_data_set$Subjects))

second_data_set <- arrange(second_data_set, Subjects)

write.table(second_data_set, "tidy_data_set.txt", row.names = FALSE)
##