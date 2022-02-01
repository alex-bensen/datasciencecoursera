#1. Merges the training and the test sets to create one data set

#Bringing in train data
path <- "~/datasciencecoursera/UCI HAR Dataset/train/X_train.txt"
train <- read.table(path)

pathy <- "~/datasciencecoursera/UCI HAR Dataset/train/y_train.txt"
trainy <- read.table(pathy)
colnames(trainy) <- c('Activity')

paths <- "~/datasciencecoursera/UCI HAR Dataset/train/subject_train.txt"
trains <- read.table(paths)
colnames(trains) <- c('Subject')

#Bringing in test data
path2 <- "~/datasciencecoursera/UCI HAR Dataset/test/X_test.txt"
test <- read.table(path2)

pathyte <- "~/datasciencecoursera/UCI HAR Dataset/test/y_test.txt"
testy <- read.table(pathyte)
colnames(testy) <- c('Activity')

pathste <- "~/datasciencecoursera/UCI HAR Dataset/test/subject_test.txt"
tests <- read.table(pathste)
colnames(tests) <- c('Subject')

#creating single column data frames
subject <- rbind(trains, tests)
activity <- rbind(trainy, testy)

#merging datasets

tot_data <- rbind(test, train)

#2. Extracts only the measurements on the mean and standard deviation for 
#   each measurement

column_names <- "~/datasciencecoursera/UCI HAR Dataset/features.txt"
column_names_vect <- scan(column_names, character(), quote = "")

#creates a character vector based on names from features.txt file
count = 0
for (i in column_names_vect) {
  count = count + 1
  if (count == 2) {
    vector <- c(vector, i)
    count = 0
  }
}
vector1 <- vector[c(1:561)]

#changes column names of dataframe
colnames(tot_data) <- vector1

#substes dataframe based on whether or not mean and std are found in colnames
tot_data <- tot_data[ , grepl( "mean|std", colnames(tot_data))]
tot_data <- cbind(subject, activity, tot_data)

#3 Uses descriptive activity names to name the activities in the data set
tot_data$Activity <- gsub('1', 'Walking', tot_data$Activity)
tot_data$Activity <- gsub('2', 'Walking_Upstairs', tot_data$Activity)
tot_data$Activity <- gsub('3', 'Walking_Downstairs', tot_data$Activity)
tot_data$Activity <- gsub('4', 'Sitting', tot_data$Activity)
tot_data$Activity <- gsub('5', 'Standing', tot_data$Activity)
tot_data$Activity <- gsub('6', 'Laying', tot_data$Activity)

#4 Appropriately labels the data set with descriptive variable names. 
colnames(tot_data) <- c('Subject', 'Activity',
                        'Body Accelaration mean (X)', 'Body Accelaration mean (Y)', 
                        'Body Accelaration mean (Z)', 'Body Accelaration std (X)',
                        'Body Accelaration std (Y)', 'Body Accelaration std (Z)',
                        'Gravity Accelaration mean (X)', 'Gravity Accelaration mean (Y)',
                        'Gravity Accelaration mean (Z)', 'Gravity Accelaration std (X)',
                        'Gravity Accelaration std (Y)', 'Gravity Accelaration std (Z)',
                        'Body Accelaration Jerk mean (X)', 'Body Accelaration Jerk mean (Y)',
                        'Body Accelaration Jerk mean (Z)', 'Body Accelaration Jerk std (X)',
                        'Body Accelaration Jerk std (Y)', 'Body Accelaration Jerk std (Z)', 
                        'Body Gyro Mean (X)',
                        'Body Gyro mean (Y)', 'Body Gyro mean (Z)',
                        'Body Gyro std (X)', 'Body Gyro std (Y)',
                        'Body Gyro std (Z)', 'Body Gyro Jerk mean (X)',
                        'Body Gyro Jerk mean (Y)', 'Body Gyro Jerk mean (Z)',
                        'Body Gyro Herk std (X)', 'Body Gyro Jerk std (Y)',
                        'Body Gyro Jerk std (Z)', 'Body Accelaration Mag mean',
                        'Body Accelaration Mag std', 'Gravity Mag mean', 
                        'Gravity Mag std', 'Body Accelaration Jerk Mag mean',
                        'Body Accelaration Jerk Mag std', 'Body Gyro Mag mean',
                        'Body Gyro Mag std', 'Body Gryo Jerk Mag mean',
                        'Body Gyro Jerk Mag std', 'Body Accelaration mean FFT (X)', 
                        'Body Accelaration mean FFT (Y)', 'Body Accelaration mean FFT (Z)',
                        'Body Accelaration std FFT (X)', 'Body Accelaration std FFT (Y)', 
                        'Body Accelaration std FFT (Z)', 'Body Accelaration mean frequency FFT (X)', 
                        'Body Accelaration mean frequency FFT (Y)', 'Body Accelaration mean frequency FFT (Z)',
                        'Body Accelaration Jerk mean FFT (X)', 'Body Accelaration Jerk mean FFT (Y)',
                        'Body Accelaration Jerk mean FFT (Z)', 'Body Accelaration Jerk std FFT (X)', 
                        'Body Accelaration Jerk std FFT (Y)', 'Body Accelaration Jerk std FFT (Z)',
                        'Body Accelaration Jerk mean frequency FFT (X)', 'Body Accelaration Jerk mean frequency  FFT (Y)',
                        'Body Accelaration Jerk mean frequency FFT (Z)', 'Body Gyro mean FFT (X)',
                        'Body Gyro mean FFT (Y)', 'Body Gyro mean FFT (Z)',
                        'Body Gyro std FFT (X)', 'Body Gyro std FFT (Y)', 
                        'Body Gyro std FFT (Z)', 'Body Gyro mean Frequency FFT (X)', 
                        'Body Gyro mean Frequency FFT (Y)', 'Body Gyro mean Frequency FFT (Z)',
                        'Body Accelaration Mag mean FFT', 'Body Accelaration Mag std FFT',
                        'Body Accelaration Mag mean Frequency FFT', 'Body Body Accelaration Jerk Mag mean FFT',
                        'Body Body Accelaration Jerk Mag std FFT', 'Body Body Accelaration Jerk Mag mean Frequency FFT',
                        'Body Body Gyro Mag mean FFT', 'Body Body Gyro Mag std FFT', 
                        'Body Body Gyro Mag mean Frequency FFT', 'Body Body Gyro Jerk Mag mean FFT',
                        'Body Body Gyro Jerk Mag std', 'Body Body Gyro Jerk Mag mean frequency') 

#5. From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject.

#activity factors
act_fact <- factor(tot_data$Activity,
                   labels = c('Walking', 'Walking_Upstairs', 'Walking_Downstairs',
                              'Sitting', 'Standing', 'Laying'))

#subject factors
sub_fact <- factor(tot_data$Subject,
                   labels = c(1:30))

#creating final data frame
final_data <- data.frame(matrix(ncol=36, nrow= 0))
colnames(final_data) <- c('Walking', 'Walking_Upstairs', 'Walking_Downstairs',
                          'Sitting', 'Standing', 'Laying', 1:30)

#for loop for activity factors
for (i in 3:length(colnames(tot_data))) {
    index <- tot_data[, i]
    mean_var <- tapply(index, act_fact, mean)
    final_data[nrow(final_data) +1, ] <- mean_var
  
}

#for loop for subject factors
for (i in 3:length(colnames(tot_data))) {
    index <- tot_data[, i]
    mean_var <- tapply(index, sub_fact, mean)
    final_data[nrow(final_data) +1, ] <- mean_var
}

Variables <-          c('Body Accelaration mean (X)', 'Body Accelaration mean (Y)', 
                        'Body Accelaration mean (Z)', 'Body Accelaration std (X)',
                        'Body Accelaration std (Y)', 'Body Accelaration std (Z)',
                        'Gravity Accelaration mean (X)', 'Gravity Accelaration mean (Y)',
                        'Gravity Accelaration mean (Z)', 'Gravity Accelaration std (X)',
                        'Gravity Accelaration std (Y)', 'Gravity Accelaration std (Z)',
                        'Body Accelaration Jerk mean (X)', 'Body Accelaration Jerk mean (Y)',
                        'Body Accelaration Jerk mean (Z)', 'Body Accelaration Jerk std (X)',
                        'Body Accelaration Jerk std (Y)', 'Body Accelaration Jerk std (Z)', 
                        'Body Gyro Mean (X)',
                        'Body Gyro mean (Y)', 'Body Gyro mean (Z)',
                        'Body Gyro std (X)', 'Body Gyro std (Y)',
                        'Body Gyro std (Z)', 'Body Gyro Jerk mean (X)',
                        'Body Gyro Jerk mean (Y)', 'Body Gyro Jerk mean (Z)',
                        'Body Gyro Herk std (X)', 'Body Gyro Jerk std (Y)',
                        'Body Gyro Jerk std (Z)', 'Body Accelaration Mag mean',
                        'Body Accelaration Mag std', 'Gravity Mag mean', 
                        'Gravity Mag std', 'Body Accelaration Jerk Mag mean',
                        'Body Accelaration Jerk Mag std', 'Body Gyro Mag mean',
                        'Body Gyro Mag std', 'Body Gryo Jerk Mag mean',
                        'Body Gyro Jerk Mag std', 'Body Accelaration mean FFT (X)', 
                        'Body Accelaration mean FFT (Y)', 'Body Accelaration mean FFT (Z)',
                        'Body Accelaration std FFT (X)', 'Body Accelaration std FFT (Y)', 
                        'Body Accelaration std FFT (Z)', 'Body Accelaration mean frequency FFT (X)', 
                        'Body Accelaration mean frequency FFT (Y)', 'Body Accelaration mean frequency FFT (Z)',
                        'Body Accelaration Jerk mean FFT (X)', 'Body Accelaration Jerk mean FFT (Y)',
                        'Body Accelaration Jerk mean FFT (Z)', 'Body Accelaration Jerk std FFT (X)', 
                        'Body Accelaration Jerk std FFT (Y)', 'Body Accelaration Jerk std FFT (Z)',
                        'Body Accelaration Jerk mean frequency FFT (X)', 'Body Accelaration Jerk mean frequency  FFT (Y)',
                        'Body Accelaration Jerk mean frequency FFT (Z)', 'Body Gyro mean FFT (X)',
                        'Body Gyro mean FFT (Y)', 'Body Gyro mean FFT (Z)',
                        'Body Gyro std FFT (X)', 'Body Gyro std FFT (Y)', 
                        'Body Gyro std FFT (Z)', 'Body Gyro mean Frequency FFT (X)', 
                        'Body Gyro mean Frequency FFT (Y)', 'Body Gyro mean Frequency FFT (Z)',
                        'Body Accelaration Mag mean FFT', 'Body Accelaration Mag std FFT',
                        'Body Accelaration Mag mean Frequency FFT', 'Body Body Accelaration Jerk Mag mean FFT',
                        'Body Body Accelaration Jerk Mag std FFT', 'Body Body Accelaration Jerk Mag mean Frequency FFT',
                        'Body Body Gyro Mag mean FFT', 'Body Body Gyro Mag std FFT', 
                        'Body Body Gyro Mag mean Frequency FFT', 'Body Body Gyro Jerk Mag mean FFT',
                        'Body Body Gyro Jerk Mag std', 'Body Body Gyro Jerk Mag mean frequency')

final_data <- cbind(Variables, final_data)

head(final_data)


