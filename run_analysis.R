## for data cleaning course project
## wenwen xu
run_analysis <- function() {
  cur_dir <- getwd()
  
  # all files' path
  feature_list_dir <- paste(cur_dir, "/wenwen_project/cleanDataProject/UCI_HAR_Dataset/features.txt", sep="")
  x_train_dir <- paste(cur_dir, "/wenwen_project/cleanDataProject/UCI_HAR_Dataset/train/X_train.txt", sep="")
  x_test_dir <- paste(cur_dir, "/wenwen_project/cleanDataProject/UCI_HAR_Dataset/test/X_test.txt", sep="")
  y_train_dir <- paste(cur_dir, "/wenwen_project/cleanDataProject/UCI_HAR_Dataset/train/y_train.txt", sep="")
  y_test_dir <- paste(cur_dir, "/wenwen_project/cleanDataProject/UCI_HAR_Dataset/test/y_test.txt", sep="")
  activity_list_dir <- paste(cur_dir, "/wenwen_project/cleanDataProject/UCI_HAR_Dataset/activity_labels.txt", sep="")
  
  feature_list <- read.table(feature_list_dir, sep="")
  
  x_train <- read.table(x_train_dir, sep="")
  x_test <- read.table(x_test_dir, sep="")
  
  y_train <- read.table(y_train_dir, sep="")
  y_test <- read.table(y_test_dir, sep="")  
  
  activity_list <- read.table(activity_list_dir, sep="")
  
  x_merge <- rbind(x_train, x_test)
  x_merge_copy <- x_merge
  y_merge <- rbind(y_train, y_test)
  
  y_convertActName <- add_activityNames(y_merge, activity_list)
  
  # attach decriptive feature name
  for (i in 1:dim(x_merge)[2]) {
    colnames(x_merge)[i] <- as.character(feature_list[i,2])
  }
  
  final_merge <- cbind(y_convertActName, x_merge)
  
  # attach standard devation to create a new table
  std_table <- matrix(nrow=2, ncol=dim(x_merge)[2])
  rownames(std_table) <- (c("mean", "std"))
  # names(std_table) <- names(final_merge)
  # colnames(std_table)[1] <- colnames(final_merge)[1]
  #
  for (i in 1:dim(std_table)[2]) {
    # assign same col names for merge
    
    std_table[1, i] <- mean(x_merge[,i])
    std_table[2, i] <- sd(x_merge[,i])
    #colnames(std_table)[i] <- colnames(as.matrix(x_merge))[i]
  }
  
  std_merge <- rbind(x_merge_copy, std_table)
  #temp_merge <- rbind(x_merge, std_table)
  # list(final_merge, std_merge)
  View(final_merge)
  View(std_merge)
}

add_activityNames <- function(actNum_table, actName_table) {
  convertName_table <- matrix(nrow=dim(actNum_table)[1], ncol=1)
  for (i in 1:dim(actNum_table)[1]) {
    convertName_table[i, 1] <- as.character(actName_table[as.numeric(actNum_table[i,1]), 2])
  }
  convertName_table
}

