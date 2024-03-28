
# Load required libraries
install.packages(c("pacman", "tidyverse", "lubridate", "tm", "topicmodels", "sentimentr", "udpipe","fastDummies","dplyr"))
library(pacman)
p_load(tidyverse, lubridate, tm, topicmodels, sentimentr, udpipe,fastDummies,dplyr)
p_load(caret)
p_load(lares)  #corelationplot
p_load(ggcorrplot)  # ggcorrplot
p_load(neuralnet)   #neuralnet
p_load(ROSE)    #oversample Balancing
#install.packages('BiocManager')
p_load(randomForest) # Load the randomForest package
# p_load(randomFOrestExplainer) # random forest explainer
p_load(partykit) # plot random forest tree 
#remotes::install_github("cran/DMwR", force = TRUE)
#remotes::install_github("cran/ggpubr")
p_load(DMwR)
p_load(rpart)     # desicion tree Rpart  
p_load(rpart.plot) ## plot Desicion tree for Rpart
p_load(kernlab)

library(pacman)

p_load(tidyverse, lubridate, tm, topicmodels, sentimentr, udpipe,fastDummies,dplyr,caret,lares,randomForest,partykit)
# p_load(caret)
# p_load(lares)  #corelationplot
# p_load(ggcorrplot)  # ggcorrplot
#  p_load(neuralnet)   #neuralnet
#  p_load(ROSE)    #oversample Balancing
#  #install.packages('BiocManager')
#  p_load(randomForest) # Load the randomForest package
#  # p_load(randomFOrestExplainer) # random forest explainer
#  p_load(partykit) # plot random forest tree 
remotes::install_github("cran/DMwR", force = TRUE)
#   remotes::install_github("cran/ggpubr")
p_load(DMwR)
#   p_load(rpart)     # desicion tree Rpart  
#   p_load(rpart.plot) ## plot Desicion tree for Rpart


# Read the CSV file
# df <- read.csv("path_to_your_file.csv", stringsAsFactors = FALSE)
df <- read.csv("C:/Users/sj01148/OneDrive - University of Surrey/Documents/Disseratation/dissertation analysis/diss_data.csv", stringsAsFactors = FALSE)

# dataset information
str(df)


# Data Cleaning
# Convert date columns to Date format
df$ENQUIRY.DATE <- dmy(df$ENQUIRY.DATE)
df$OFFER.PROCESS.START.DATE <- dmy(df$OFFER.PROCESS.START.DATE)
df$OFFER.SENT.DATE <- dmy(df$OFFER.SENT.DATE)
df$First.Follow.Up..Date. <- dmy(df$First.Follow.Up..Date.)
df$Second.Follow.Up.Date <- dmy(df$Second.Follow.Up.Date)
df$Third.Follow.Up.Date <- dmy(df$Third.Follow.Up.Date)

# Feature Engineering
# Extract month from date_column and create a new column named 'month'
df$ENQUIRY.MONTH <- month(df$ENQUIRY.DATE)

print(df)

# Calculate number of Follow ups in dataset
df$Number.OF.Follow.Up <- ifelse(df$First.Follow.Up.Details== "CLOSED",1,
                                 ifelse(df$Second.Follow.Up.Details=="CLOSED",2,3))

# Enquiry start date to end in dataset
df$ENQUIRY.START.TO.END.IN.DAYS <- ifelse(df$Number.OF.Follow.Up== "1",as.numeric(df$First.Follow.Up..Date. - df$ENQUIRY.DATE),
                                          ifelse(df$Number.OF.Follow.Up=="2",as.numeric(df$Second.Follow.Up.Date - df$ENQUIRY.DATE),
                                                 as.numeric(df$Third.Follow.Up.Date - df$ENQUIRY.DATE)))


# Remove unnecessary columns
df <- df[, -which(names(df) %in% c("QUOTATION.NO", "First.Follow.Up..Date.", "First.Follow.Up.Details",
                                   "Second.Follow.Up.Date", "Second.Follow.Up.Details",
                                   "Third.Follow.Up.Date", "Third.Follow.Up.Details",
                                   "Status.Details","Equipment.Specification", "JOB.No.",
                                   "OFFER.PROCESS.START.DATE","ENQUIRY.DATE","OFFER.SENT.DATE",
                                   "ENQUIRY.TYPE","EQUIPMENT.MATERIAL","MODES.OF.ENQUIRY"))]

# Handle missing values (example: fill with NA)
df[is.na(df)] <- NA

str(df)

# Convert percentage column to numeric
df$Final.Discount.Offered <- as.numeric(gsub("%", "", df$Final.Discount.Offered)) / 100

# # Convert categorical variables to factors
# df$CLIENT <- as.factor(df$CLIENT)
# df$EQUIPMENT.TYPE <- as.factor(df$EQUIPMENT.TYPE)
# df$ENQUIRY.PROCESSED.BY <- as.factor(df$ENQUIRY.PROCESSED.BY)
# df$STATUS <- ifelse(df$STATUS == "Closed", 0, 1) # Convert to binary (0 for Closed, 0 otherwise)
# 
# # Load the required library
# #library(fastDummies)
# 
# # Convert categorical variables to dummies
# #df_dummies <- dummy_cols(df, select_columns = c("CLIENT", "EQUIPMENT.TYPE", "ENQUIRY.PROCESSED.BY"))
# 
# # Removing the original categorical columns to avoid multicollinearity
# #df_dummies <- training_data_dummies[, !(names(training_data_dummies) %in% c("CLIENT", "EQUIPMENT.TYPE", "ENQUIRY.PROCESSED.BY"))]
# 
# 
# 
# # *********************************************************************
# # ** Step 3 - Data balancing *****************************************
# # **********************************************************************
# # 
# #Split the dataset into training and testing sets
# Data_for_balancing <- df
# Data_for_balancing$STATUS <- as.factor(Data_for_balancing$STATUS)
# balancedTrainIndex <- createDataPartition(Data_for_balancing$STATUS, p = 0.7, list = FALSE, times = 1)
# 
# training_data <- Data_for_balancing[balancedTrainIndex, ]
# testing_data <- Data_for_balancing[-balancedTrainIndex, ]
# # Use SMOTE to balance the dataset
# balanced_training <- SMOTE(STATUS ~ ., training_data, perc.over = 1000, k=5)
# prop.table(table(balanced_training$STATUS)) * 100
# training_data <- balanced_training
# 
# # 3. Logistic Regression model
# 
# # Dataframe to store results
# results_logreg <- data.frame(
#   Model = character(),
#   Predictions = I(list()),
#   Confusion_Matrix = I(list())
# )
# 
# # Use CLIENT, EQUIPMENT.TYPE as predictors and STATUS as the outcome
# model <- glm(STATUS ~ EQUIPMENT.TYPE+QTY+Final.Discount.Offered+
#                ENQUIRY.PROCESSED.BY+ENQUIRY.MONTH+Number.OF.Follow.Up+
#                ENQUIRY.START.TO.END.IN.DAYS,data = training_data,
#              family = "binomial")
# summary(model)
# 
# #logistic_reg_model <- model
# 
# # 4. Check performance on the test set
# 
# # Predict probabilities
# predicted_probs <- predict(model, newdata = testing_data, type = "response")
# 
# # Convert probabilities to binary predictions
# threshold <- 0.5
# predicted_labels <- ifelse(predicted_probs > threshold, 1, 0)
# 
# # Confusion Matrix
# confusionMatrix(as.factor(predicted_labels), as.factor(testing_data$STATUS))
# 
# # # Add to results dataframe
# # results_logreg <- rbind(results_logreg, data.frame(
# #   Model = "Logistic Regression",
# #   Model = I(list(logistic_reg_model)),
# #   Confusion_Matrix = I(list(Confusion_Matrix))
# # ))
# 
# 
# # Random Forest model
# # Use the `model.matrix` function to convert categorical variables to a series of binary columns
# 
# 
# 
# # Now, fit the random forest model with the encoded data
# rf_model <- randomForest(STATUS ~ EQUIPMENT.TYPE+QTY+Final.Discount.Offered+
#                            ENQUIRY.PROCESSED.BY+ENQUIRY.MONTH+Number.OF.Follow.Up+
#                            ENQUIRY.START.TO.END.IN.DAYS, data = training_data, ntree = 100)
# 
# 
# # Predict on test set using Random Forest
# #rf_predictions <- predict(rf_model, testing_data)
# 
# predictions <- predict(rf_model, testing_data)
# 
# confusionMatrix(predictions, testing_data$STATUS)
# 
# # Check accuracy for Random Forest
# rf_accuracy <- sum(predictions == testing_data$STATUS) / nrow(testing_data)
# print(paste("Random Forest Accuracy:", rf_accuracy))
# 


#one hot encoding
data_encoded <- df %>%
  select(CLIENT, EQUIPMENT.TYPE, 
         STATUS) %>% 
  mutate(STATUS = ifelse(STATUS == "PO received", 1, 0)) 
data_matrix <- data_encoded %>%
  mutate_at(vars(-STATUS), as.factor) %>% 
  model.matrix(STATUS ~ ., data = .) %>% 
  as.data.frame()

# Add STATUS back to the encoded data
data_matrix$STATUS <- data_encoded$STATUS

unique(data_encoded$STATUS)
# scaling
data_matrix_without_status <- data_matrix %>% select(-STATUS)
scaled_matrix <- scale(data_matrix_without_status)

# Convert the scaled matrix to a dataframe
scaled_data <- as.data.frame(scaled_matrix)

# Assign the STATUS column
scaled_data$STATUS <- data_encoded$STATUS

#preprocessing data
scaled_data$`(Intercept)` <- NULL
names(scaled_data) <- gsub("[^a-zA-Z0-9_]", "_", names(scaled_data))
set.seed(123)
str(df)
#write_csv(scaled_data,"C:/Users/sj01148/OneDrive - University of Surrey/Documents/Disseratation/dissertation analysis/scaled_data.csv")

# *********************************************************************
# ** Step 3 - Data balancing *****************************************
# **********************************************************************
# 
#Split the dataset into training and testing sets
Data_for_balancing <- scaled_data
Data_for_balancing$STATUS <- as.factor(Data_for_balancing$STATUS)
balancedTrainIndex <- createDataPartition(Data_for_balancing$STATUS, p = 0.7, list = FALSE, times = 1)

training_data <- Data_for_balancing[balancedTrainIndex, ]
testing_data <- Data_for_balancing[-balancedTrainIndex, ]
# Use SMOTE to balance the dataset
#balanced_training <- SMOTE(STATUS ~ ., training_data, perc.over = 1000, k=5)
#prop.table(table(balanced_training$STATUS)) * 100
#training_data <- balanced_training
#set.seed(12345)

training_data$STATUS <- as.factor(training_data$STATUS)
testing_data$STATUS <- as.factor(testing_data$STATUS)

rf_model <- randomForest(STATUS ~ ., data=training_data, ntree=100)

predictions <- predict(rf_model, testing_data)

confusionMatrix(predictions, testing_data$STATUS)
#hyper parameters
# Example of tuning using different ntree values
results <- list()

trees <- c(50, 100, 150, 200)
for (t in trees) {
  model <- randomForest(STATUS ~ ., data=training_data, ntree=t)
  preds <- predict(model, testing_data)
  accuracy <- sum(preds == testing_data$STATUS) / length(testing_data$STATUS)
  results[[paste("ntree =", t)]] <- accuracy
}

print(results)



Data_new <- training_data
actual_testing_data <- testing_data


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
Folds <- createFolds(Data_new$STATUS, k = FD, list = TRUE, returnTrain = TRUE)



training_data_percentages <- seq(from = 0.2, to = 0.8, by = 0.1) # creating  sequence to represent training data ratio
# the below loop iterates to vary training data amount and check the model  performance
for (t in training_data_percentages){
  pfc <- pfc+1
  RF_indx_Partition <- createDataPartition(Data_new$STATUS, p=t, list=FALSE) # index of training data
  #indx_Partition <- createDataPartition(myrf_Data_new$STATUS, p=0.8, list=FALSE) # index of training data
  myrf_training_data <- Data_new[RF_indx_Partition,] # training dataset
  # myrf_testing_data <- Data_new[-RF_indx_Partition,] # testing dataset
  myrf_testing_data <- actual_testing_data
  #testing_data_new <- testing_data[, which(names(testing_data) %in% c("BusinessTravel", "Department","DistanceFromHome","working_days","Y"))]
  
  
  set.seed(123123)
  
  # Train the random forest model using the training data
  myrf <- randomForest(as.factor(myrf_training_data$STATUS) ~ ., data = myrf_training_data[,1:ncol(myrf_training_data)], ntree = 20, importance = TRUE)
  
  # Print the model summary
  #print(myrf)
  
  # Make predictions on the test data
  myrf_predictions <- predict(myrf,myrf_testing_data)
  
  #cm <- confusionMatrix(predictions, myrf_actual)
  
  #Predicted_Data <- data.frame(Predicted_Data)
  myrf_testing_data <- data.frame(myrf_testing_data)
  myrf_Predicted_Data <- data.frame(myrf_predictions)
  #class(neuralnet_Predicted_Data)
  myrf_actual <- factor(myrf_testing_data$STATUS)
  myrf_predicted <- factor(myrf_Predicted_Data[,1],levels = levels(myrf_actual))
  #cm <- confusionMatrix(neuralne_testing_data$STATUS, pred[,1])
  
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

# # Predictions for the entire dataset
# predictions_myrf <- predict(testing_data, scaled_data, type="response")
# testing_data$Predicted_Probability <- predictions
# 
# # Convert probabilities to labels (using 0.5 as threshold in this example)
# df$Predicted_Label <- ifelse(df$Predicted_Probability > 0.5, 1, 0)
# 
# # View the dataset with predictions
# head(df)



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
#neuralnet_Data_new$STATUS <- as.numeric(neuralnet_Data_new$STATUS) -1
#neuralnet_testing_data$STATUS <- as.numeric(neuralnet_testing_data$STATUS) -1
# Data_new$STATUS <- ifelse(Data_new$STATUS == "1", 0, 1)
#neuralnet_Data_new <- apply(neuralnet_Data_new, 2, function(x) (x - min(x)) / (max(x) - min(x)))
#neuralnet_testing_data <- apply(neuralnet_testing_data, 2, function(x) (x - min(x)) / (max(x) - min(x)))
#neuralnet_Data_new <- data.frame(neuralnet_Data_new)
Folds <- createFolds(neuralnet_Data_new$STATUS, k = FD, list = TRUE, returnTrain = TRUE)
#sum(is.na(neuralnet_Data_new))

#sum(is.infinite(neuralnet_Data_new))



training_data_percentages <- seq(from = 0.2, to = 0.8, by = 0.1) # creating  sequence to represent training data ratio
# the below loop iterates to vary training data amount and check the model  performance
for (t in training_data_percentages){{
  pfc <- pfc+1
  neuralnet_indx_Partition <- createDataPartition(neuralnet_Data_new$STATUS, p=t, list=FALSE) # index of training data
  #indx_Partition <- createDataPartition(neuralnet_Data_new$STATUS, p=0.8, list=FALSE) # index of training data
  neuralnet_training_data <- neuralnet_Data_new[neuralnet_indx_Partition,] # training dataset
  #testing_data_new <- testing_data[, which(names(testing_data) %in% c("BusinessTravel", "Department","DistanceFromHome","working_days","Y"))]
  
  
  set.seed(123123)
  
  
  #TrainedNeuralNet <- neuralnet(Y~BusinessTravel+Department+DistanceFromHome+working_days, data = training_data, hidden = c(2,2,1),linear.output = FALSE)
  TrainedNeuralNet <- neuralnet(STATUS~., data = neuralnet_training_data,
                                hidden = c(2,2,2),linear.output = FALSE,err.fct = "sse",
                                lifesign = "minimal", stepmax = 10000000, threshold = 0.01,rep = 1)
  
  #plot(TrainedNeuralNet)
  
  #prediction(TrainedNeuralNet)
  
  
  #pred <- predict(TrainedNeuralNet, neuralne_testing_data)
  #table(neuralne_testing_data$STATUS, pred[, 1] > 0.5)
  
  #Predicting on test data
  
  #neuralnet_Predicted_Parameters <- compute(TrainedNeuralNet, testing_data_new)
  neuralnet_Predicted_Parameters <- compute(TrainedNeuralNet, neuralnet_testing_data)
  
  neuralnet_Predicted_Net_Results <- neuralnet_Predicted_Parameters$net.result
  #Predicted_Data <- sapply(Predicted_Net_Results,round,digits=0)
  neuralnet_Predicted_Data <- ifelse(neuralnet_Predicted_Net_Results[,1] > 0.25, 1, 0)
  
  # Converting predcited outcomes as data frame
  #Predicted_Data <- data.frame(Predicted_Data)
  neuralnet_testing_data <- data.frame(neuralnet_testing_data)
  neuralnet_Predicted_Data <- data.frame(neuralnet_Predicted_Data)
  #class(neuralnet_Predicted_Data)
  neuralnet_actual <- factor(neuralnet_testing_data$STATUS)
  neuralnet_predicted <- factor(neuralnet_Predicted_Data[,1],levels = levels(neuralnet_actual))
  #cm <- confusionMatrix(neuralne_testing_data$STATUS, pred[,1])
  
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
}
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
Folds <- createFolds(Data_new$STATUS, k = FD, list = TRUE, returnTrain = TRUE)
training_data_percentages <- seq(from = 0.2, to = 0.8, by = 0.1) # creating  sequence to represent training data ratio
# the below loop iterates to vary training data amount and check the model  performance
for (t in training_data_percentages){
  pfc <- pfc+1
  rpart_indx_Partition <- createDataPartition(Data_new$STATUS, p=t, list=FALSE) # index of training data
  # indx_Partition <- createDataPartition(Data_new$STATUS, p=0.7, list=FALSE) # index of training data
  rpart_training_data <- Data_new[rpart_indx_Partition,] # training dataset
  # rpart_testing_data <- Data_new[-rpart_indx_Partition,] # testing dataset
  rpart_testing_data <- actual_testing_data
  #testing_data_new <- testing_data[, which(names(testing_data) %in% c("BusinessTravel", "Department","DistanceFromHome","working_days","Y"))]
  
  
  set.seed(123123)
  
  trained_rpart <- rpart(STATUS ~ ., data = rpart_training_data, method = "class")
  
  
  # predictions <- predict(tree_model, testing_data, type = "class")
  rpart_Predicted_Data <- predict(trained_rpart, rpart_testing_data, type = "class")
  #table(rpart_testing_data$STATUS, pred[, 1] > 0.5)
  # confusion_matrix <- table(rpart_Predicted_Data, rpart_testing_data$STATUS)
  # print(confusion_matrix)
  #Predicting on test data
  
  
  
  # Converting predcited outcomes as data frame
  #Predicted_Data <- data.frame(Predicted_Data)
  rpart_testing_data <- data.frame(rpart_testing_data)
  rpart_Predicted_Data <- data.frame(rpart_Predicted_Data)
  #class(rpart_Predicted_Data)
  rpart_actual <- factor(rpart_testing_data$STATUS)
  rpart_predicted <- factor(rpart_Predicted_Data[,1],levels = levels(rpart_actual))
  #cm <- confusionMatrix(rpart_testing_data$STATUS, pred[,1])
  
  # pred_factor <- factor(pred[,1], levels = levels(rpart_actual))
  cm <- confusionMatrix(rpart_actual, rpart_predicted)
  rpart_cm <- list(cm)
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
fit <- rpart(STATUS ~ ., data = Data_new)

rpart.plot(fit)


# *******************************************
# SVM model
# *******************************************

# header line of on-screen performance metrics
message("SVM")
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
Folds <- createFolds(Data_new$STATUS, k = FD, list = TRUE, returnTrain = TRUE)



training_data_percentages <- seq(from = 0.2, to = 0.8, by = 0.1) # creating  sequence to represent training data ratio
# the below loop iterates to vary training data amount and check the model  performance
for (t in training_data_percentages) {
  pfc <- pfc+1
  svm_indx_Partition <- createDataPartition(Data_new$STATUS, p=t, list=FALSE) # index of training data
  svm_training_data <- Data_new[svm_indx_Partition,] # training dataset
  svm_testing_data <- actual_testing_data
  
  constant_vars <- sapply(svm_training_data, function(x) length(unique(x)) == 1)
  names(svm_training_data)[constant_vars]
  svm_training_data <- svm_training_data[, !constant_vars]
  svm_testing_data <- svm_testing_data[, !constant_vars]
  
  
  
  TrainedClassifier <-
    ksvm(STATUS ~ .,
         data = svm_training_data,
         kernel = "vanilladot",
         C = 1)
  #Vanilla dot is the linear.SVM, Basically kernal specifies the function to be used in Algorithm
  #C controls the cost and to strike balance between minimizing the error and complexity of model. Larger C => Overfit
  Predicted_outcomes <-
    predict(TrainedClassifier, svm_testing_data[, 1:ncol(svm_testing_data) - 1])
  
  cm <- confusionMatrix(svm_testing_data$STATUS, Predicted_outcomes)
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


