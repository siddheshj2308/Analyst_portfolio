# *****************************************************************************
# *** 1.1 Loading libraries ****************
# *****************************************************************************
R.Version()$version.string
install.packages("pacman")
library(pacman)
p_load(dplyr)
p_load(lubridate)
# p_load(ggplot)     # visualization---------------????
p_load(ggplot2)    # Visualization 
p_load(caret)  #featurePlot,
p_load(lares)  #corelationplot
#install.packages("psych", dependencies = TRUE) 
#install.packages(c("mnormt", "foreign", "minqa", "nloptr", "numDeriv", "psych"), dependencies = TRUE)
p_load(psych)  #describe,
p_load(readxl)
p_load(lattice)
p_load(ggcorrplot)  # ggcorrplot
p_load(neuralnet)   #neuralnet
p_load(ROSE)    #oversample Balancing
#install.packages('BiocManager')
p_load(randomForest) # Load the randomForest package
# p_load(randomFOrestExplainer) # random forest explainer
p_load(partykit) # plot random forest tree 
p_load(car)
p_load(cluster)# Clustering
p_load(factoextra) # clustering algorithms & visualization
p_load(tidyr)
p_load(rpart)     # desicion tree Rpart  
p_load(rpart.plot) ## plot Desicion tree for Rpart
p_load(irr)
p_load(GGally)
remotes::install_github("cran/DMwR")
#remotes::install_github("cran/ggpubr")

p_load(DMwR)


# *****************************************************************************
# ************* 1.2 Loading Data **************
# *****************************************************************************
general_data <- read.csv("~/general_data.csv", header=TRUE)
employee_survey_data <- read.csv("~/employee_survey_data.csv", header=TRUE)
in_time <- read.csv("~/in_time.csv", header=TRUE)
out_time <- read.csv("~/out_time.csv", header=TRUE)
manager_survey_data <- read.csv("~/manager_survey_data.csv", header=TRUE)

# ****************************************************************************
# *** 1.3 Summary of Data *******************
# ****************************************************************************
head(general_data)
head(employee_survey_data)
head(in_time)
head(out_time)
head(manager_survey_data)

## finding NA values in Dataset
any(is.na(general_data))
any(is.na(employee_survey_data))

any(is.na(manager_survey_data))
any(is.na(in_time))
any(is.na(out_time)) 

## replacing NA values with character
if (any(is.na(employee_survey_data))) {
  # replace NA values with a specified string
  employee_survey_data[is.na(employee_survey_data)] <- as.character("")
}

if (any(is.na(general_data))) {
  # replace NA values with a specified string
  general_data[is.na(general_data)] <- as.character("")
}

# *************************************************************************
# ********** 1.4 Merging Dataset ***********
# *************************************************************************

merge_data <- merge(general_data,employee_survey_data,by = "EmployeeID")
merge_data <- merge(merge_data,manager_survey_data, by = "EmployeeID")

#convert null to time "2999-12-28 23:59:59"

if (any(is.na(in_time))) {
  # replace NA values with a specified string
  in_time[is.na(in_time)] <- "2999-12-28 23:59:59"
}

if (any(is.na(out_time))) {
  # replace NA values with a specified string
  out_time[is.na(out_time)] <- "2999-12-28 23:59:59"
}

any(is.na(in_time))
any(is.na(out_time))

# str for structure of Data
str(in_time)

# Function to calculate worked hours
worked_hours_fn <- function(in_col, out_col) {
  as.numeric(difftime(ymd_hms(out_col), ymd_hms(in_col), units = "hours"))
}

# Get the date column names
date_cols <- colnames(in_time)[-1]

# Join the in_time and out_time data frames
combined_data <- inner_join(in_time, out_time, by = "X", suffix = c("_in", "_out"))

# Initialize the worked_hours data frame with the X column and the same number of rows as combined_data
worked_hours <- data.frame(id = combined_data$X, stringsAsFactors = FALSE)

# Loop through each date column  calculate worked hours
for (date_col in date_cols) {
  in_col_name <- paste0(date_col, "_in")
  out_col_name <- paste0(date_col, "_out")
  worked_hours_col_name <- paste0("worked_hours_", gsub("\\.", "_", date_col))
  
  worked_hours[[worked_hours_col_name]] <- worked_hours_fn(combined_data[[in_col_name]], combined_data[[out_col_name]])
}
{
  # calculate total hours and working days for each id
  total_hours <- apply(worked_hours[, 2:ncol(worked_hours)], 1, sum)
  working_days <- apply(worked_hours[, 2:ncol(worked_hours)] != 0, 1, sum, na.rm = TRUE)
  
  # combine results into a data frame
  worked_hours_new <- data.frame(id = worked_hours$id, 
                                 total_hours = total_hours, 
                                 working_days = working_days)
}

{
  #merge the new generated df into the main df by EmployeeID = id
  
  merge_data <- merge(merge_data,worked_hours_new, by.x = "EmployeeID", by.y="id")
  
  # move Attrition from middle to the end
  
  new_merge_data <- select(merge_data, -Attrition, everything())
  
  Data <- new_merge_data
  
  any(is.na(Data))
}

# *********************************************************************
# ** Step 2 - Data preparation *****************************************
# **********************************************************************



## for changing the data type to numeric

for (i in 1:(ncol(Data)-1)) {
  if (is.character(Data[, i])) {
    for (j in 1:nrow(Data)) {
      ascis <- as.numeric(charToRaw(Data[j, i]))
      Data[j, i] <- sum(ascis)
    }
    Data[, i] <- as.numeric(Data[, i])
  }
}



# to use normalised data uncomment line-59???????
#Data[,1:(ncol(Data)-1)] <- scale(Data[,1:(ncol(Data)-1)])

#2. Create a whisker plot for each one of the input variables (at least 5) of the
#dataset in one image. HINT: Use par( ) and boxplot( ).
{
   par(mfrow=c(2,5))
  #par(mar = c(5, 5, 2, 2))
  boxplot(Data[,2], main=names(Data[2]))
  boxplot(Data[,3], main=names(Data[3]))
  boxplot(Data[,4], main=names(Data[4]))
  boxplot(Data[,5], main=names(Data[5]))
}

#3.Create a scatterplot matrix of the first 3 columns of the dataset. HINT: Use
#featurePlot( ).
# featurePlot(Data[,1:3],Data[,ncol(Data)], plot = if(is.factor(Data[,ncol(Data)]))
#   "strip" else "scatter")

#4.Create a histogram to observe the distribution using 1st, 6th, 12th, 13th, 15th
#variables of the dataset. HINT: Use hist( ).
{
   par(mfrow=c(2,5))
  # par(mar = c(5, 5, 2, 2))
  hist(Data[,2])
  hist(Data[,3])
  hist(Data[,4])
  hist(Data[,5])
}


# # after converting the whole dataset (except last column) to numeric, converting
#the last column to factor to be used as class label
# Data[,ncol(Data)] <- as.factor(Data[,ncol(Data)])

# rename Attrition to Y
names(Data)[names(Data) == "Attrition"] <- "Y"

#delete the columns only have one value and  the employeeID

Data_pre <- Data[, -which(names(Data) %in% c("StandardHours", "Over18", "EmployeeCount","EmployeeID"))]
prop.table(table(Data_pre$Y)) * 100
Data_pre$Y <- ifelse(Data_pre$Y == "Yes", 1, 0)
Data[,ncol(Data)] <- as.factor(Data[,ncol(Data)])

#Exploring relationships among features – the correlation matrix
{
  
  correlation_matrix <- cor(Data_pre[,1:ncol(Data_pre)])
  # Visualize the correlation matrix
  ggcorrplot(correlation_matrix)
}


# *********************************************************************
# ** Step 3 - Data balancing *****************************************
# **********************************************************************
# 
#Split the dataset into training and testing sets
Data_for_balancing <- Data_pre
Data_for_balancing$Y <- as.factor(Data_for_balancing$Y)

balancedTrainIndex <- createDataPartition(Data_for_balancing$Y, p = 0.7, list = FALSE, times = 1)
training_data <- Data_for_balancing[balancedTrainIndex, ]
testing_data <- Data_for_balancing[-balancedTrainIndex, ]
# Use SMOTE to balance the dataset
balanced_training <- SMOTE(Y ~ ., training_data, perc.over = 120)
prop.table(table(balanced_training$Y)) * 100

training_data$Y <- as.numeric(training_data$Y) -1
balanced_training$Y <- as.numeric(balanced_training$Y) -1
testing_data$Y <- as.numeric(testing_data$Y) -1
# train_balanced$Y <- ifelse(train_balanced$Y == "Yes", 1, 0)
# testing_data$Y <- ifelse(testing_data$Y == "Yes", 1, 0)
set.seed(12345)




# *********************************************************************
# ** Step 4 - Feature selection ***************************************
# *********************************************************************

#4.1 Correlation  



D<-Data_pre # ensure dataset got an index otherwise data is lost
D$Index <- NULL
D |> corr_var(Y, top=20)

# **************************************************************************
# ****** .3 Logistic Regression  ***********************
# **************************************************************************

log_reg_Data <- Data_pre
log_reg_Data$Y <- as.factor(log_reg_Data$Y)
log_reg_Data <- na.omit(log_reg_Data)


library(caret)
set.seed(123)

hr_logit <- glm(Y ~ ., data = log_reg_Data, family = "binomial")
summary(hr_logit)

exp(coef(hr_logit))

##################################################################

# **************************************************************************
# ****** Abstract dataset ***********************
# **************************************************************************

  abstracted_training_data <- training_data
  abstracted_training_data <- subset(abstracted_training_data, select = c("total_hours", "TotalWorkingYears","MaritalStatus", "Age",
                                                                        "YearsWithCurrManager", "YearsAtCompany",  "BusinessTravel" , "Y" ))

  abstracted_balanced_training_data <- balanced_training
  abstracted_balanced_training_data <- subset(abstracted_balanced_training_data, select = c("total_hours", "TotalWorkingYears","MaritalStatus", "Age",
                                          "YearsWithCurrManager", "YearsAtCompany",  "BusinessTravel" , "Y" ))
  abstracted_testing_data <- testing_data
  abstracted_testing_data <- subset(abstracted_testing_data, select = c("total_hours", "TotalWorkingYears","MaritalStatus", "Age",
                                                                          "YearsWithCurrManager", "YearsAtCompany",  "BusinessTravel" , "Y" ))

  # abstracted_training_data$Y <- as.numeric(abstracted_training_data$Y) -1
  # abstracted_testing_data$Y <- as.numeric(abstracted_testing_data$Y) -1
  
# {
#   backup <- Data_for_abstract
#   backup$Y <- ifelse(backup$Y == "Yes", 1, 0)
#   Data_new <- backup
#   Data_new <- subset(Data_new, select = c("total_hours", "TotalWorkingYears","MaritalStatus", "Age",
#                                           "YearsWithCurrManager", "YearsAtCompany",  "BusinessTravel" , "Y" ))
#   
#   str(Data_new)
#   # Data_new$Y <- ifelse(Data_new$Y == "Yes", 1, 0)
#   # Data_new <- subset(Data_new, select = c("total_hours", "TotalWorkingYears","MaritalStatus", "Age",
#   #            "YearsWithCurrManager", "YearsAtCompany",  "BusinessTravel" ,
#   #           "TrainingTimesLastYear", "working_days", "EnvironmentSatisfaction",
#   #          "YearsSinceLastPromotion", "PercentSalaryHike", "MonthlyIncome", 
#   #         "PerformanceRating", "NumCompaniesWorked", "Gender", "JobInvolvement",
#   #        "Education", "JobRole", "EducationField", "Y"))  
#   
#   
# }

#Exploring relationships among features – the correlation matrix

{
  Feature_Data <- Data_pre[-ncol(Data_pre)]
  correlation_matrix <- cor(Feature_Data)
  # Visualize the correlation matrix
  ggcorrplot(correlation_matrix)
}


  
  
  



# outpput
  
  # formatted_datetime <- format(Sys.time(), format = "%Y-%m-%d-%H-%M-%S")
  # 
  # logfile <- file("my_logfile5",formatted_datetime,".txt", open = "at")
  # 
  # # Redirect the console output to the log file
  # sink(logfile, type = "output")
  # sink(type = "output")

for (l in 1:3) {
  if (l == 1) {
    Data_new <- training_data
    actual_testing_data <- testing_data
    message("loop", l, "\n")
  } else if (l == 2) {
    Data_new <- abstracted_training_data
    actual_testing_data <- abstracted_testing_data
    message("loop", l, ",after feature selection.\n")
  } else {
    Data_new <- abstracted_balanced_training_data
    actual_testing_data <- abstracted_testing_data
    message("loop", l, ",after feature selection and balancing \n")
  }




# *******************************************
#  training a KNN model on the data
# *******************************************

training_data_percentages <- seq(from = 0.2, to = 0.8, length.out = 7)
message("KNN")
message("\tFold", "\t ", "\taccuracy", "\tkappa", "\t", "\tsensitivity" , "\tspecificity","\t" , "precision", "\trecall")

for (t in training_data_percentages){
  
  #message("**************★★★******************・")
  
  #message (" Training Data: " ,t*100, "| testing data:", (1-t)*100)
  
  control <- trainControl (method="repeatedcv", repeats = 10)
  
  metric <- "Accuracy"
  
  KNN_indx_Partition <- createDataPartition(Data_new$Y, p=t, list=FALSE) # index of training data
  KNN_training_data <- Data_new[KNN_indx_Partition,] # training dataset
  #KNN_test_data <- Data_new[-KNN_indx_Partition,] # testing dataset
  KNN_test_data <- actual_testing_data
   KNN_training_data[,ncol(KNN_training_data)] <- as.factor(KNN_training_data [,ncol (KNN_training_data) ])
  
  set.seed (123)
  
  fit.knn <- train (Y~., data=KNN_training_data, method="knn", metric=metric, trControl=control) 
  #message ("------------------")
  #message (" Training Performance")
  
  #message ("-------------------")
  #print (fit.knn)
  #plot (fit.knn)
   KNN_test_data [,ncol (KNN_test_data)] <- as.factor(KNN_test_data [,ncol (KNN_test_data)])
  
  Predict.knn <- predict (fit.knn, newdata = KNN_test_data [, 1:ncol (KNN_test_data)-1] )
  
  cm <- confusionMatrix(Predict.knn, KNN_test_data$Y) 
  #message ("-----------------")
  #message (" Testing Performance")
  
  
  message("\tFold-",t, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2),
                                     nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  #print (cm)
}


# *******************************************
#  training a Random Forest model on the data
# *******************************************

message("RF")
message("\tFold", "\t ", "\taccuracy", "\tkappa", "\t", "\tsensitivity" , "\tspecificity","\t" , "precision", "\trecall")


# creating a blank data frame to store performance metrics scores
pf = data.frame(matrix(
  vector(), 5, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc <- 0 # pfc - performance frame counter

FD <- 7
Folds <- createFolds(Data_new$Y, k = FD, list = TRUE, returnTrain = TRUE)



training_data_percentages <- seq(from = 0.2, to = 0.8, by = 0.1) # creating  sequence to represent training data ratio
# the below loop iterates to vary training data amount and check the model  performance
for (t in training_data_percentages){
  pfc <- pfc+1
  RF_indx_Partition <- createDataPartition(Data_new$Y, p=t, list=FALSE) # index of training data
  #indx_Partition <- createDataPartition(myrf_Data_new$Y, p=0.8, list=FALSE) # index of training data
  myrf_training_data <- Data_new[RF_indx_Partition,] # training dataset
  # myrf_testing_data <- Data_new[-RF_indx_Partition,] # testing dataset
  myrf_testing_data <- actual_testing_data
  #testing_data_new <- testing_data[, which(names(testing_data) %in% c("BusinessTravel", "Department","DistanceFromHome","working_days","Y"))]
  
  
  set.seed(123123)
  
  # Train the random forest model using the training data
  myrf <- randomForest(as.factor(myrf_training_data$Y) ~ ., data = myrf_training_data[,1:ncol(myrf_training_data)], ntree = 20, importance = TRUE)
  
  # Print the model summary
  #print(myrf)
  
  # Make predictions on the test data
  myrf_predictions <- predict(myrf,myrf_testing_data)
  
  #cm <- confusionMatrix(predictions, myrf_actual)
  
  #Predicted_Data <- data.frame(Predicted_Data)
  myrf_testing_data <- data.frame(myrf_testing_data)
  myrf_Predicted_Data <- data.frame(myrf_predictions)
  #class(neuralnet_Predicted_Data)
  myrf_actual <- factor(myrf_testing_data$Y)
  myrf_predicted <- factor(myrf_Predicted_Data[,1],levels = levels(myrf_actual))
  #cm <- confusionMatrix(neuralne_testing_data$Y, pred[,1])
  
  # pred_factor <- factor(pred[,1], levels = levels(neuralnet_actual))
  cm <- confusionMatrix(myrf_actual, myrf_predicted)
  
  #metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\tFold-",t, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2),
                                     nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created
  # ----------------
  pf[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =
                                 2)
  pf[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pf[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),
                                  nsmall = 2)
  pf[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2),
                                  nsmall = 2)
  pf[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall
                                = 2)
  pf[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("-----------------------------------------------------------------------------
----------------------------------------------------")

# ###################
# # fit <- rpart(Y ~ ., data = Data_new)
# #
# # rpart.plot(fit)
#  rf_model  <- randomForest(Y~., data = Data_new)
#
#   tree<- getTree(rf_model, k=1, labelVar = TRUE)
#   tree_party <- as.party(tree)
#   plot(tree_party)




#



# *******************************************
#  training a Neural model on the data
# *******************************************

message("ANN")
message("\tFold", "\t ", "\taccuracy", "\tkappa", "\t", "\tsensitivity" , "\tspecificity","\t" , "precision", "\trecall")


# creating a blank data frame to store performance metrics scores
pf = data.frame(matrix(
  vector(), 5, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc <- 0 # pfc - performance frame counter

FD <- 7

neuralnet_Data_new <- Data_new
neuralnet_testing_data <- actual_testing_data
# neuralnet_Data_new$Y <- as.numeric(neuralnet_Data_new$Y) -1
# neuralnet_testing_data$Y <- as.numeric(neuralnet_testing_data$Y) -1
# Data_new$Y <- ifelse(Data_new$Y == "1", 0, 1)
neuralnet_Data_new <- apply(neuralnet_Data_new, 2, function(x) (x - min(x)) / (max(x) - min(x)))
neuralnet_testing_data <- apply(neuralnet_testing_data, 2, function(x) (x - min(x)) / (max(x) - min(x)))
neuralnet_Data_new <- data.frame(neuralnet_Data_new)
Folds <- createFolds(neuralnet_Data_new$Y, k = FD, list = TRUE, returnTrain = TRUE)



training_data_percentages <- seq(from = 0.05, to = 0.08, by = 0.01) # creating  sequence to represent training data ratio
# the below loop iterates to vary training data amount and check the model  performance
for (t in training_data_percentages){
  pfc <- pfc+1
  neuralnet_indx_Partition <- createDataPartition(neuralnet_Data_new$Y, p=t, list=FALSE) # index of training data
  #indx_Partition <- createDataPartition(neuralnet_Data_new$Y, p=0.8, list=FALSE) # index of training data
  neuralnet_training_data <- neuralnet_Data_new[neuralnet_indx_Partition,] # training dataset
  #testing_data_new <- testing_data[, which(names(testing_data) %in% c("BusinessTravel", "Department","DistanceFromHome","working_days","Y"))]
  
  
  set.seed(123123)
  
  
  #TrainedNeuralNet <- neuralnet(Y~BusinessTravel+Department+DistanceFromHome+working_days, data = training_data, hidden = c(2,2,1),linear.output = FALSE)
  TrainedNeuralNet <- neuralnet(Y~ ., data = neuralnet_training_data,
                                hidden = c(2,2,2),linear.output = FALSE,err.fct = "sse",
                                lifesign = "minimal", stepmax = 10000000, threshold = 0.01,rep = 1)
  
  #plot(TrainedNeuralNet)
  
  #prediction(TrainedNeuralNet)
  
  
  #pred <- predict(TrainedNeuralNet, neuralne_testing_data)
  #table(neuralne_testing_data$Y, pred[, 1] > 0.5)
  
  #Predicting on test data
  
  #neuralnet_Predicted_Parameters <- compute(TrainedNeuralNet, testing_data_new)
  neuralnet_Predicted_Parameters <- compute(TrainedNeuralNet, neuralnet_testing_data)
  
  neuralnet_Predicted_Net_Results <- neuralnet_Predicted_Parameters$net.result
  #Predicted_Data <- sapply(Predicted_Net_Results,round,digits=0)
  neuralnet_Predicted_Data <- ifelse(neuralnet_Predicted_Net_Results[,1] > 0.5, 1, 0)
  
  # Converting predcited outcomes as data frame
  #Predicted_Data <- data.frame(Predicted_Data)
  neuralnet_testing_data <- data.frame(neuralnet_testing_data)
  neuralnet_Predicted_Data <- data.frame(neuralnet_Predicted_Data)
  #class(neuralnet_Predicted_Data)
  neuralnet_actual <- factor(neuralnet_testing_data$Y)
  neuralnet_predicted <- factor(neuralnet_Predicted_Data[,1],levels = levels(neuralnet_actual))
  #cm <- confusionMatrix(neuralne_testing_data$Y, pred[,1])
  
  # pred_factor <- factor(pred[,1], levels = levels(neuralnet_actual))
  cm <- confusionMatrix(neuralnet_actual, neuralnet_predicted)
  
  # length(neuralnet_actual)
  # length(neuralnet_predicted)
  # length(neuralnet_Predicted_Net_Results)
  # length(neuralnet_Predicted_Parameters)
  # print(cm)
  
  #print(TrainedNeuralNet)
  #length(actual)
  #length(predicted)
  
  #metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\tFold-",t, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2),
                                     nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created
  # ----------------
  pf[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =
                                 2)
  pf[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pf[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),
                                  nsmall = 2)
  pf[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2),
                                  nsmall = 2)
  pf[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall
                                = 2)
  pf[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}

plot(TrainedNeuralNet, rep = NULL, x.entry = NULL, x.out = NULL,
     radius = 0.15, arrow.length = 0.2, intercept = TRUE,
     intercept.factor = 0.4, information = TRUE, information.pos = 0.1,
     col.entry.synapse = "black", col.entry = "black",
     col.hidden = "black", col.hidden.synapse = "black",
     col.out = "black", col.out.synapse = "black",
     col.intercept = "blue", fontsize = 12, dimension = 6,
     show.weights = TRUE, file = NULL)

message("-----------------------------------------------------------------------------
----------------------------------------------------")












# *******************************************
# Decision tree training a model on the data
# *******************************************



# header line of on-screen performance metrics
message("Rpart")
message("\tFold", "\t ", "\taccuracy", "\tkappa", "\t", "\tsensitivity" , "\tspecificity","\t" , "precision", "\trecall")


# creating a blank data frame to store performance metrics scores
pf = data.frame(matrix(
  vector(), 5, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc <- 0 # pfc - performance frame counter

FD <- 7
# Data_new <- apply(Data_new, 2, function(x) (x - min(x)) / (max(x) - min(x)))
# Data_new <- data.frame(Data_new)
Folds <- createFolds(Data_new$Y, k = FD, list = TRUE, returnTrain = TRUE)



training_data_percentages <- seq(from = 0.2, to = 0.8, by = 0.1) # creating  sequence to represent training data ratio
# the below loop iterates to vary training data amount and check the model  performance
for (t in training_data_percentages){
  pfc <- pfc+1
  rpart_indx_Partition <- createDataPartition(Data_new$Y, p=t, list=FALSE) # index of training data
  # indx_Partition <- createDataPartition(Data_new$Y, p=0.7, list=FALSE) # index of training data
  rpart_training_data <- Data_new[rpart_indx_Partition,] # training dataset
  # rpart_testing_data <- Data_new[-rpart_indx_Partition,] # testing dataset
  rpart_testing_data <- actual_testing_data
  #testing_data_new <- testing_data[, which(names(testing_data) %in% c("BusinessTravel", "Department","DistanceFromHome","working_days","Y"))]
  
  
  set.seed(123123)
  
  trained_rpart <- rpart(Y ~ ., data = rpart_training_data, method = "class")
  
  
  # predictions <- predict(tree_model, testing_data, type = "class")
  rpart_Predicted_Data <- predict(trained_rpart, rpart_testing_data, type = "class")
  #table(rpart_testing_data$Y, pred[, 1] > 0.5)
  # confusion_matrix <- table(rpart_Predicted_Data, rpart_testing_data$Y)
  # print(confusion_matrix)
  #Predicting on test data
  
  
  
  # Converting predcited outcomes as data frame
  #Predicted_Data <- data.frame(Predicted_Data)
  rpart_testing_data <- data.frame(rpart_testing_data)
  rpart_Predicted_Data <- data.frame(rpart_Predicted_Data)
  #class(rpart_Predicted_Data)
  rpart_actual <- factor(rpart_testing_data$Y)
  rpart_predicted <- factor(rpart_Predicted_Data[,1],levels = levels(rpart_actual))
  #cm <- confusionMatrix(rpart_testing_data$Y, pred[,1])
  
  # pred_factor <- factor(pred[,1], levels = levels(rpart_actual))
  cm <- confusionMatrix(rpart_actual, rpart_predicted)
  
  #metrics from here
  
  # below message() function shows the performance metrics on-screen
  message("\tFold-",t, "\t ", format(round(cm[["overall"]][["Accuracy"]]*100, 2),
                                     nsmall = 2), "\t\t",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  
  # --------- assigning the performance metrics to the dataframe created
  # ----------------
  pf[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =
                                 2)
  pf[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pf[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),
                                  nsmall = 2)
  pf[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2),
                                  nsmall = 2)
  pf[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall
                                = 2)
  pf[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}
message("-----------------------------------------------------------------------------
----------------------------------------------------")

# Fit the decision tree
fit <- rpart(Y ~ ., data = Data_new)

rpart.plot(fit)





################Clustering###############
cluster_training_data <- Data_new
#cluster_testing_data <- corr_test_Data
any(is.na(cluster_training_data))
#any(is.na(cluster_testing_data))
# Perform clustering on the training set
Clustered_Data <- kmeans(cluster_training_data[, -ncol(cluster_training_data)], 2, nstart = ncol(cluster_training_data))

# Add cluster labels to the training set
cluster_training_data$cluster <- Clustered_Data$cluster

#check one center
Clustered_Data$size
Clustered_Data$centers
Clustered_Data$cluster

any(is.na(Clustered_Data))
#Attach labels
cluster_training_data$ClusterLabels <- Clustered_Data$cluster

# create subset of data based on clustering labels for further analysis
#Cluster_1 <- cluster_training_data[cluster_training_data$ClusterLabels == 1, ] # from cluster-1
#Cluster_2 <- cluster_training_data[cluster_training_data$ClusterLabels == 2, ] # from cluster-2

#Silhouette_Score <- silhouette(Clustered_Data$cluster, dist(training_data[,-31]))
#mean(Silhouette_Score[, 3])
#Silhouette_Score
#Silhouette_Score <- function(k){
#Clustered_Data_wd_multiple_cen <- kmeans(cluster_training_data[,-28], centers = k, nstart=28)
#sils <- silhouette(Clustered_Data_wd_multiple_cen$cluster, dist(cluster_training_data[,-28]))
#mean(sils[, 3])
#}

#k <- 2:10
#avg_sil <- sapply(k, Silhouette_Score)
#par(mfrow=c(1,1))
#plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette
#Scores', frame=FALSE)

################################################
#create data frame with 0 rows and 3 columns
pf <- data.frame(matrix(ncol = 2, nrow = 9))
#provide column names
colnames(pf) <- c('K', 'Silhouette')
pfc <- 0 # pfc - performance data frame counter

for (k in c(2:10)) {
  pfc <- pfc+1
  Clustered_Data_wd_multiple_cen <- kmeans(Data_new[,-ncol(Data_new)], centers = k, 
                                           nstart=ncol(Data_new))
  sils <- silhouette(Clustered_Data_wd_multiple_cen$cluster, 
                     dist(Data_new[,-ncol(Data_new)]))
  avg_sil <- mean(sils[, 3])
  pf[pfc,"K"] <- k
  pf[pfc,"Silhouette"] <- format(round(mean(sils[, 3]), 2), nsmall = 2)
}

plot(pf[,1], type='b', pf[,2], xlab='Number of clusters', ylab='Average Silhouette 
Scores', frame=FALSE)

grid(nx = NULL, ny = NULL,
     lty = 2, col = "gray", lwd = 2)
title("Relation between Number of Clusters with Silhouette Scores")
# new_Clustered_Data_wd_multiple_cen <- kmeans(trainData[,-28], centers = k, 
#                                          nstart=28)
# trainData$Y <- ifelse(trainData$Y == "Yes", 1, 0)
# fviz_cluster(new_Clustered_Data_wd_multiple_cen, data = trainData)

cluster_training_data$Y <- ifelse(cluster_training_data$Y == "Yes", 1, 0)

# identify columns with zero variance
zero_vars <- apply(cluster_training_data, 2, var)==0

# remove columns with zero variance
cluster_training_data <- cluster_training_data[,!zero_vars]

# generate cluster plot

cluster2plot <-fviz_cluster(Clustered_Data, data = cluster_training_data)

cluster2plot
# ____________________________________________________________________
# compute gap statistic
# _____________________________


}


