library(dplyr)

#--------------------------------------------------------------------------------
# Read feature names and activity names
#--------------------------------------------------------------------------------

# read feature names
features <- read.table("features.txt",header = F)$V2
features

# read activity labels and names
activity_labels <- read.table("activity_labels.txt", header = F)
names(activity_labels) <- c("Label","Activity")

#--------------------------------------------------------------------------------
# Read train files
#--------------------------------------------------------------------------------

# read time and frequency data 
x_train <- read.table("train/X_train.txt")
names(x_train) <- features # Add descriptive labels
head(x_train)

# read subject for each row
subject_train <- read.table("train/subject_train.txt")
names(subject_train) <- "Subject"
head(subject_train)

# read activity for each row
y_train <- read.table("train/y_train.txt")
names(y_train) <- "Activity Label"
head(y_train)


#--------------------------------------------------------------------------------
# Read test files
#--------------------------------------------------------------------------------

x_test <- read.table("test/X_test.txt")
names(x_test) <- features # Add descriptive labels
head(x_test)

# read subject for each row
subject_test <- read.table("test/subject_test.txt")
names(subject_test) <- "Subject"
head(subject_test)

# read activity for each row
y_test <- read.table("test/y_test.txt")
names(y_test) <- "Activity Label"
head(y_test)

#--------------------------------------------------------------------------------
# Combine
#--------------------------------------------------------------------------------

# Merge training and test sets

combined <- rbind(cbind(subject_train,y_train,x_train),
  cbind(subject_test,y_test,x_test))

dim(combined)


# extract columns for mean and stdev and keep the subject and activity label

combined_mean_sd <- combined[,c(T,T,grepl("mean\\(\\)|std\\(\\)",colnames(combined[,3:ncol(combined)])))]
dim(combined_mean_sd)

# Add activity names

combined_mean_sd_act <- merge(combined_mean_sd, activity_labels, by.x = "Activity Label", by.y = "Label", all.x = T)
head(combined_mean_sd_act)

# Descriptive labels - already added using names() when reading the data files
colnames(combined_mean_sd_act)

# Get average value per subject per activity
avg_sub_act <- combined_mean_sd_act %>% select(-`Activity Label`) %>% group_by(Subject,Activity) %>% summarise_if(is.numeric, mean, na.rm = TRUE) 

# Reshape and change column names
library(reshape2)
avg_sub_act_tidy <- melt(avg_sub_act,id=c("Subject","Activity")) %>% rename(Mean = value, Feature = variable) 

                         