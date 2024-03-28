# Load required libraries
install.packages(c("pacman", "tidyverse", "lubridate", "tm", "topicmodels",
                   "sentimentr", "udpipe","fastDummies","dplyr","caret",
                   "lares","randomForest","partykit"))
library(pacman)
p_load(tidyverse, lubridate, tm, topicmodels, sentimentr, udpipe,dplyr,caret,lares)
p_load(readxl)
p_load(ggplot2)
p_load(reshape2)
p_load(gridExtra)
p_load(plotly)
p_load(tidyr)
p_load(DMwR)
p_load(cluster)# Clustering
p_load(factoextra) # clustering algorithms & visualization
p_load(GGally)
p_load(dendextend)
p_load(dynamicTreeCut)
p_load(mclust)

# Define the path to your Excel file
file_path <- "C:/Users/sj01148/OneDrive - University of Surrey/Documents/Dissertation- shweta/Dataset/Supply_chain_logisitcs.xlsx"

# Get the names of all sheets in the Excel file
sheet_names <- excel_sheets(file_path)

# Read each sheet into a list
df_list <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
})

# Create a named list where each element is a dataframe
df_dict <- setNames(df_list, sheet_names)

# To access a dataframe, use df_dict$sheet_name, for example:
# df_dict$'First_Sheet_Name'

# Data Preparation and Transformation

# Loop through each dataframe in the list
for (df_name in names(df_dict)) {
  df <- df_dict[[df_name]]
  
  # Print the shape of the dataframe
  cat(df_name, "- shape:", dim(df), "\n")
  
  # Count duplicates and missing values
  duplicate_count <- sum(duplicated(df))
  missing_values_count <- sum(is.na(df))
  
  # Check and print if there are any duplicates or missing values
  if (duplicate_count > 0 || missing_values_count > 0) {
    cat(sprintf(">>>> %s - duplicates: %d; missing values: %d\n", df_name, duplicate_count, missing_values_count))
  }
}


# Drop duplicates from FreightRates dataframe
FreightRates <- df_dict[['FreightRates']] %>%
  distinct()

# Update the dataframe in the list
df_dict[['FreightRates']] <- FreightRates

# Loop through each dataframe in the list
for (df_name in names(df_dict)) {
  # Get the dataframe
  df <- df_dict[[df_name]]
  
  # Rename columns: replace spaces and '/' with underscores and convert to uppercase
  colnames(df) <- toupper(gsub(" ", "_", gsub("/", "_", colnames(df))))
  
  # Update the dataframe in the list
  df_dict[[df_name]] <- df
}


# Get the OrderList dataframe
orderList <- df_dict[['OrderList']]

# Merge OrderList with FreightRates and WhCosts
orderList <- merge(orderList, df_dict[['FreightRates']], by.x = c('CARRIER', 'ORIGIN_PORT', 'DESTINATION_PORT'), 
                   by.y = c('CARRIER', 'ORIG_PORT_CD', 'DEST_PORT_CD'), all.x = TRUE)
orderList <- merge(orderList, df_dict[['WhCosts']], by.x = 'PLANT_CODE', by.y = 'WH', all.x = TRUE)

# Calculate cost
orderList$COST <- (orderList$UNIT_QUANTITY * orderList$RATE) + (orderList$UNIT_QUANTITY * orderList$COST_UNIT)

# Data preparation and transformation for OrderList
orderList <- na.omit(orderList)  # Drop rows with missing values
orderList$ORDER_DATE <- ymd(orderList$ORDER_DATE)

# Assuming similar steps for ProductsPerPlant dataframe
productsPerPlant <- df_dict[['ProductsPerPlant']]
productsPerPlant <- na.omit(productsPerPlant)  # Drop rows with missing values

# Update dictionary values
df_dict[['OrderList']] <- orderList
df_dict[['ProductsPerPlant']] <- productsPerPlant

# Loop through each dataframe in the list and print its name and columns
for (df_name in names(df_dict)) {
  cat("Dataframe:", df_name, ">>>\n")
  cat("Columns:", colnames(df_dict[[df_name]]), "\n\n")
}

# Statistics and Visualisation

# Summary statistics for OrderList dataframe
summary(df_dict[['OrderList']])

# Summary statistics for FreightRates dataframe
summary(df_dict[['FreightRates']])

# Calculate correlation matrix for numeric columns in OrderList
correlation_matrix <- cor(df_dict[['OrderList']][sapply(df_dict[['OrderList']], is.numeric)], use="complete.obs")
round(correlation_matrix, 2)



# Prepare the correlation matrix for plotting
correlation_matrix <- cor(df_dict[['OrderList']][sapply(df_dict[['OrderList']], is.numeric)], use="complete.obs")
melted_correlation_matrix <- melt(correlation_matrix)

# Plot the heatmap
ggplot(melted_correlation_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix", x = "", y = "")

# Retrieve pricing strategy and calculate price elasticity and total historical revenue
pricing_strategy <- unique(df_dict[['FreightRates']]$MODE_DSC)
price_elasticity <- mean(df_dict[['FreightRates']]$RATE, na.rm = TRUE)
historical_revenue <- df_dict[['OrderList']]$UNIT_QUANTITY * df_dict[['OrderList']]$COST
total_revenue <- sum(historical_revenue, na.rm = TRUE)

# Print the results
cat("Pricing strategies:", toString(pricing_strategy), "\n")
cat("Average price elasticity:", price_elasticity, "\n")
cat("Total historical revenue:", total_revenue, "\n")


# Bar plot of service levels
p1 <- ggplot(df_dict[['OrderList']], aes(x = SERVICE_LEVEL)) +
  geom_bar() +
  labs(title = 'Distribution of Service Levels', x = 'Service Level', y = 'Count')

# Box plot of daily capacities
p2 <- ggplot(df_dict[['WhCapacities']], aes(y = DAILY_CAPACITY)) +
  geom_boxplot() +
  labs(title = 'Distribution of Daily Capacities', y = 'Daily Capacity')

# Combine the plots
grid.arrange(p1, p2, ncol = 2)

# Scatter plot of weight vs. shipping cost
ggplot(df_dict[['OrderList']], aes(x = WEIGHT, y = COST, color = CARRIER)) +
  geom_point() +
  labs(title = 'Weight vs. Shipping Cost', x = 'Weight', y = 'Cost')


# Data preparation for the plots
plant_counts <- as.data.frame(table(df_dict[['ProductsPerPlant']]$PLANT_CODE))
wh_costs <- df_dict[['WhCosts']]

# Plot Number of Products that each plant manufactures
p1 <- ggplot(plant_counts, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Number of Products that each plant manufactures", x = "Plant", y = "Number of Products")

# Plot Manufacturing Cost for each Plant
p2 <- ggplot(wh_costs, aes(x = WH, y = COST_UNIT)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Manufacturing Cost for each Plant", x = "Plant", y = "Cost per Unit")

# Combine the plots
grid.arrange(p1, p2, ncol = 2)

#Visualize the network/routing
# Load the PlantPorts dataframe
df_plant_ports <- df_dict[['PlantPorts']]

# Create the figure
fig <- plot_ly()

# Add the connections as traces
for (i in 1:nrow(df_plant_ports)) {
  row <- df_plant_ports[i, ]
  fig <- fig %>%
    add_trace(
      x = c(row$PLANT_CODE, row$PORT),
      y = c(1, 0),
      mode = 'lines+markers',
      type = 'scatter',
      marker = list(
        size = 10,
        symbol = 'circle'
      ),
      text = paste("Plant:", row$PLANT_CODE, "<br>Port:", row$PORT),
      hoverinfo = 'text'
    )
}

# Set up the layout
fig <- fig %>%
  layout(
    title = 'Plant and Port Connections',
    showlegend = FALSE,
    xaxis = list(title = 'PLANT_CODE - PORT', tickangle = -45),
    yaxis = list(title = '', showticklabels = FALSE, range = c(-0.2, 1.2))
  )

# Show the interactive plot
fig

# Create a list of all the supply nodes
supply_nodes <- unique(df_dict[['WhCosts']]$WH)

# Create a dictionary for the number of units of supply for each supply node
supply_dict <- setNames(numeric(length(supply_nodes)), supply_nodes)
for (node in supply_nodes) {
  total_capacity <- sum(df_dict[['WhCapacities']]$DAILY_CAPACITY[df_dict[['WhCapacities']]$PLANT_ID == node], na.rm = TRUE)
  supply_dict[node] <- total_capacity
}

# Create a list of all demand nodes
demand_nodes <- unique(df_dict[['OrderList']]$DESTINATION_PORT)

# Create a dictionary for the number of units of demand for each demand node
demand_dict <- setNames(numeric(length(demand_nodes)), demand_nodes)
for (dest_port in demand_nodes) {
  unit_quantity <- sum(df_dict[['OrderList']]$UNIT_QUANTITY[df_dict[['OrderList']]$DESTINATION_PORT == dest_port], na.rm = TRUE)
  demand_dict[dest_port] <- unit_quantity
}



###############################################################################
######## Models #####################################################
###############################################################################

# Load the necessary library
p_load(stats)

# Assuming 'OrderList' is the dataframe to be used
OrderList <- df_dict[['OrderList']]

# Using your existing linear regression model
model <- lm(COST ~ WEIGHT + UNIT_QUANTITY, data = OrderList)

# Model Summary
summary(model)

# Predictions
predictions <- predict(model, OrderList)

# Calculate residuals
residuals <- OrderList$COST - predictions

# Create a dataframe for plotting
plot_data <- data.frame(Actual = OrderList$COST, Predicted = predictions, Residuals = residuals)

# Plot of Actual vs Predicted
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  ggtitle("Actual vs Predicted Costs: Linear Regression Model") +
  xlab("Actual Cost") +
  ylab("Predicted Cost")

# Plot of Residuals
ggplot(plot_data, aes(x = Predicted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals vs Predicted Values") +
  xlab("Predicted Cost") +
  ylab("Residuals")

# Calculate performance metrics
rmse <- sqrt(mean(residuals^2))
mae <- mean(abs(residuals))
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")

# Export results
write.csv(plot_data, "linear_regression_results.csv")


# Load necessary libraries

p_load(randomForest)
p_load(e1071)
p_load(xgboost)
p_load(nnet) # For neural network


# Assuming 'orderList' and 'freightRates' have been read into R already
# comparison between Xgboost and neural network 
# 1. Data Cleaning and Preprocessing
# Clean column names
orderList <- orderList %>% rename_with(~ gsub(" ", "_", .))
FreightRates <- FreightRates %>% rename_with(~ gsub(" ", "_", .))

# Convert date to proper format
orderList$ORDER_DATE <- as.Date(orderList$ORDER_DATE, format = "%Y-%m-%d")

index <- createDataPartition(orderList$COST, p = 0.8, list = FALSE)
train_Data <- orderList[index, ]
test_data <- orderList[-index, ]
# Convert factors to numeric if applicable
numeric_train_data <- train_Data
numeric_test_data <- test_data
numeric_train_data[sapply(numeric_test_data, is.factor)] <- lapply(numeric_test_data[sapply(numeric_test_data, is.factor)], as.numeric)

# Drop non-numeric columns
numeric_train_data <- numeric_train_data[sapply(numeric_train_data, is.numeric)]
numeric_test_data <- numeric_test_data[sapply(numeric_test_data, is.numeric)]


# # XGBoost Model
params <- list(booster = "gbtree", objective = "reg:linear", eta = 0.01, gamma = 0, max_depth = 6, 
               min_child_weight = 1, subsample = 0.5, colsample_bytree = 0.5)
# Prepare data for XGBoost
xgb_train <- xgb.DMatrix(data = as.matrix(numeric_train_data[-which(names(numeric_train_data) == "COST")]), 
                       label = numeric_train_data$COST)
xgb_model <- xgb.train(params, xgb_train, nrounds = 100)


# Make predictions and evaluate the model
xgb_test <- xgb.DMatrix(data = as.matrix(numeric_test_data[,-which(names(numeric_test_data) == "COST")]))
predictions <- predict(xgb_model, xgb_test)
results <- data.frame(Actual = test_data$COST, Predicted = predictions)

# Performance Evaluation: Using RMSE and MAE
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
mae <- mean(abs(results$Actual - results$Predicted))

# Business Impact Analysis
# The improved accuracy of cost prediction using XGBoost can lead to better budgeting and resource allocation in supply chain management.

# Comparative Analysis
# The XGBoost model's performance, indicated by RMSE and MAE, will be compared to baseline models such as linear regression in future analyses.

# Visualization of Predicted vs Actual Costs
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  ggtitle("Predicted vs Actual Costs") +
  xlab("Actual Cost") +
  ylab("Predicted Cost")

# Unique Contributions
# The application of XGBoost in this context represents a more sophisticated approach compared to traditional linear models in supply chain cost prediction.

# Final Summary
# The XGBoost model demonstrates a high level of accuracy in predicting costs, which can significantly contribute to more effective supply chain management.

# Save/Export Results
write.csv(results, "xgboost_predictions.csv")

# Model Complexity Analysis
# XGBoost Model Parameters: 
print(params)

# Output RMSE and MAE
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")

# comparison XGboost with nural network


# Preparing data for neural network
nn_train_data <- as.data.frame(numeric_train_data)
nn_test_data <- as.data.frame(numeric_test_data)

# Convert target variable to a factor for classification, if necessary
# nn_train_data$COST <- as.factor(nn_train_data$COST)

# Define neural network model
nn_model <- nnet(COST ~ ., data = nn_train_data, size = 10, decay = 5e-4, maxit = 200)

# Predictions from the neural network
nn_predictions <- predict(nn_model, newdata = nn_test_data, type = "raw")

# Evaluate neural network model performance
# Adjust according to your model's nature (classification or regression)
nn_performance <- postResample(nn_predictions, nn_test_data$COST)

# Print neural network model performance
print(nn_performance)

# Load necessary libraries
p_load(keras)


# Assuming 'nn_model' is your trained neural network model
# And 'test_data' is your test dataset

# Predict using the neural network model
nn_predictions <- predict(nn_model, as.matrix(nn_test_data[,-which(names(nn_test_data) == "COST")]))

# Combine actual and predicted values
results <- data.frame(Actual = nn_test_data$COST, Predicted = nn_predictions)

# Visualization of Predicted vs Actual Costs
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  ggtitle("Predicted vs Actual Costs: Neural Network Model") +
  xlab("Actual Cost") +
  ylab("Predicted Cost")

# Output model's summary (assumes 'nn_model' is your model variable)
summary(nn_model)

# Output RMSE and MAE
rmse <- sqrt(mean((results$Predicted - results$Actual)^2))
mae <- mean(abs(results$Predicted - results$Actual))
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")

# Save/Export Results
write.csv(results, "nn_model_predictions.csv")



##########################################################################################################
###### K means Clustering #####################################
###########################################################################################################


# Load necessary libraries
p_load(tidyverse)
p_load(cluster)

# Assuming 'orderList' is your dataset
data_for_clustering <- select(orderList, -c(PLANT_CODE, CARRIER, ORIGIN_PORT, DESTINATION_PORT, SERVICE_LEVEL, SVC_CD, MODE_DSC, CARRIER_TYPE))

# Inspect the data types of each column
sapply(data_for_clustering, class)

# Alternatively, you can use str() for a more detailed view
str(data_for_clustering)

# Identify non-numeric columns
non_numeric_cols <- sapply(data_for_clustering, function(col) !is.numeric(col))
names(data_for_clustering)[non_numeric_cols]

# Remove non-numeric columns (if any) from data_for_clustering
data_for_clustering <- data_for_clustering[, !non_numeric_cols]

# Alternatively, convert them to numeric if appropriate
# data_for_clustering$some_column <- as.numeric(as.factor(data_for_clustering$some_column))

# Now scale the data
data_scaled <- scale(data_for_clustering)


# Determine the optimal number of clusters
wss <- sapply(1:10, function(k) {
  kmeans(data_scaled, centers = k, nstart = 20)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K", 
     ylab = "Total within-clusters sum of squares")

# Choose the optimal number of clusters (let's assume it's 3 for this example)
set.seed(123) # for reproducibility
kmeans_result <- kmeans(data_scaled, centers = 4, nstart = 20)

# Add the cluster assignment to the original data
orderList$cluster <- kmeans_result$cluster

# Examine the clusters
table(orderList$cluster)

#################################################################################
# new addition 
#cross validation for kmeans cluster
# Assuming 'orderList' is your dataset
data_for_clustering_1 <- select(orderList, -c(PLANT_CODE, CARRIER, ORIGIN_PORT, DESTINATION_PORT, SERVICE_LEVEL, SVC_CD, MODE_DSC, CARRIER_TYPE))

# Inspect the data types of each column
sapply(data_for_clustering_1, class)

# Alternatively, you can use str() for a more detailed view
str(data_for_clustering_1)

# Identify non-numeric columns
non_numeric_cols <- sapply(data_for_clustering_1, function(col) !is.numeric(col))
names(data_for_clustering_1)[non_numeric_cols]

# Remove non-numeric columns (if any) from data_for_clustering
data_for_clustering_1 <- data_for_clustering_1[, !non_numeric_cols]

# Alternatively, convert them to numeric if appropriate
# data_for_clustering$some_column <- as.numeric(as.factor(data_for_clustering$some_column))

# Now scale the data
data_scaled <- scale(data_for_clustering_1)

set.seed(123) # for reproducibility

# Splitting the dataset into training and testing sets
index <- createDataPartition(orderList$cluster, p = 0.8, list = FALSE)
train_set <- data_scaled[index, ]
test_set <- data_scaled[-index, ]

# Ensure that train_set and test_set are dataframes
train_set <- as.data.frame(train_set)
test_set <- as.data.frame(test_set)


# Apply K-means clustering on the training set
kmeans_train_result <- kmeans(train_set, centers = 4, nstart = 20)

# Add the cluster assignment to the training set
train_set$cluster <- kmeans_train_result$cluster

# Assign test set data to nearest cluster centers
get_nearest_cluster <- function(point, centers) {
  distances <- apply(centers, 1, function(center) sum((point - center)^2))
  return(which.min(distances))
}

test_set_clusters <- apply(test_set, 1, function(row) get_nearest_cluster(row, kmeans_train_result$centers))

# Add the predicted cluster assignments to the test set
test_set$predicted_cluster <- test_set_clusters

# Calculate the adjusted Rand index
adjusted_rand_index <- adjustedRandIndex(test_set$cluster, test_set$predicted_cluster)

# Print the adjusted Rand index
print(adjusted_rand_index)


################################################################################
str(orderList)

# Scatter plot of WEIGHT vs COST colored by cluster
ggplot(orderList, aes(x = WEIGHT, y = COST, color = as.factor(cluster))) +
  geom_point() +
  labs(title = "Scatter Plot of Weight vs Cost", x = "Weight", y = "Cost") +
  scale_color_discrete(name = "Cluster") +
  theme_minimal()

#2. Cluster Profiles with Boxplots
# Boxplot of COST for each cluster
ggplot(orderList, aes(x = as.factor(cluster), y = COST)) +
  geom_boxplot() +
  labs(title = "Cost Distribution by Cluster", x = "Cluster", y = "Cost") +
  theme_minimal()

#3. Bar Plot of Cluster Sizes
# Count the number of observations in each cluster
cluster_sizes <- table(orderList$cluster)

# Bar plot of cluster sizes
barplot(cluster_sizes, main = "Cluster Sizes", xlab = "Cluster", ylab = "Count")
#4 Cluster Centroids
# Assuming 'kmeans_result' contains your k-means result
centroids <- kmeans_result$centers

p_load(GGally)

# Select a subset of variables for the pairs plot
selected_data <- orderList[c("WEIGHT", "COST", "TPT_DAY_CNT", "cluster")]

# Convert cluster to a factor for better visualization
selected_data$cluster <- as.factor(selected_data$cluster)

# Create the pairs plot
ggpairs(selected_data, 
        aes(color = cluster, alpha = 0.4), 
        lower = list(continuous = wrap("points", size = 0.5)),
        diag = list(continuous = wrap("densityDiag")))
#########################################################################

# Load necessary libraries
p_load(tidyverse)
p_load(cluster)
p_load(factoextra)
p_load(GGally)
p_load(plotly)

# Your existing data preparation and clustering code...

# Enhanced Clustering Visualization

# Heatmap of Cluster Centroids
heatmap_data <- as.data.frame(kmeans_result$centers)
colnames(heatmap_data) <- colnames(data_for_clustering)
heatmap_data$cluster <- rownames(heatmap_data)
heatmap_data_long <- gather(heatmap_data, key = "variable", value = "value", -cluster)

ggplot(heatmap_data_long, aes(x = variable, y = reorder(cluster, value), fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Heatmap of Cluster Centroids", x = "", y = "Cluster")

# PCA for Clustering Visualization
pca_result <- prcomp(data_for_clustering, scale. = TRUE)
pca_data <- data.frame(pca_result$x[,1:2])
pca_data$cluster <- orderList$cluster

ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(cluster))) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "PCA Plot of Clusters", x = "PC1", y = "PC2")

# Interactive Cluster Scatter Plot
plot_ly(pca_data, x = ~PC1, y = ~PC2, type = 'scatter', mode = 'markers',
        color = ~as.factor(cluster), marker = list(size = 10)) %>%
  layout(title = "Interactive PCA Plot of Clusters")

# Advanced Cluster Interpretation and Insights

# Calculate and print cluster summaries for business insights
cluster_summaries <- aggregate(data_for_clustering, by = list(cluster = orderList$cluster), mean)
print(cluster_summaries)

# Cluster Profiles with Boxplots - Enhanced
# Boxplot of COST for each cluster with added statistics
ggplot(orderList, aes(x = as.factor(cluster), y = COST)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  theme_minimal() +
  labs(title = "Enhanced Cost Distribution by Cluster", x = "Cluster", y = "Cost")

# Cluster Sizes with Annotations
cluster_sizes <- table(orderList$cluster)
barplot(cluster_sizes, main = "Cluster Sizes", xlab = "Cluster", ylab = "Count")
text(x = 1:length(cluster_sizes), y = cluster_sizes, label = cluster_sizes, pos = 3, cex = 0.8, col = "blue")

# Pairs Plot with Cluster Information
ggpairs(selected_data, 
        aes(color = cluster, alpha = 0.4), 
        lower = list(continuous = wrap("points", size = 0.5)),
        diag = list(continuous = wrap("densityDiag")),
        upper = list(continuous = wrap("cor", size = 3)))


#########################################################################
########## modelling ####################################
#########################################################################

# Optimization of Supply Chain Routes- check this one 
p_load(lpSolve)


# you have these data frames or similar structures

supply_data <- df_dict[['WhCapacities']] # Dataframe with supply info
demand_data <- df_dict[['OrderList']] # Dataframe with demand info

# Calculate total supply for each plant
total_supply <- aggregate(DAILY_CAPACITY ~ PLANT_ID, data = supply_data, sum)

# Calculate total demand for each destination
total_demand <- aggregate(UNIT_QUANTITY ~ DESTINATION_PORT, data = demand_data, sum)

# Construct the cost matrix
# You need to create a matrix where each cell represents the cost to ship from each plant to each destination
# This is an example, replace with your actual cost calculation
cost_matrix <- matrix(runif(nrow(total_supply) * nrow(total_demand), 1, 5), 
                      nrow = nrow(total_supply), 
                      ncol = nrow(total_demand))

# Convert supply and demand to vectors
supply_vector <- total_supply$DAILY_CAPACITY
demand_vector <- total_demand$UNIT_QUANTITY

# Calculate total supply and total demand
total_supply_amount <- sum(total_supply$DAILY_CAPACITY)
total_demand_amount <- sum(total_demand$UNIT_QUANTITY)



# Balance supply and demand by adding a dummy node if needed
if(total_supply_amount > total_demand_amount) {
  # Add a dummy demand node
  extra_demand <- total_supply_amount - total_demand_amount
  total_demand <- rbind(total_demand, data.frame(DESTINATION_PORT = "Dummy", UNIT_QUANTITY = extra_demand))
} else if(total_supply_amount < total_demand_amount) {
  # Add a dummy supply node
  extra_supply <- total_demand_amount - total_supply_amount
  total_supply <- rbind(total_supply, data.frame(PLANT_ID = "Dummy", DAILY_CAPACITY = extra_supply))
}

# Create the cost matrix, supply vector, and demand vector
cost_matrix <- matrix(runif(nrow(total_supply) * nrow(total_demand), 1, 5), 
                      nrow = nrow(total_supply), 
                      ncol = nrow(total_demand))
supply_vector <- total_supply$DAILY_CAPACITY
demand_vector <- total_demand$UNIT_QUANTITY

# Create a matrix for the constraints
# Rows: number of supply + number of demand constraints
# Columns: number of routes (should match the number of elements in cost_matrix)
num_supply <- length(supply_vector)
num_demand <- length(demand_vector)
num_routes <- nrow(total_supply) * nrow(total_demand)
constraint_matrix <- matrix(0, nrow = num_supply + num_demand, ncol = num_routes)

# Supply constraints
for(i in 1:num_supply) {
  start_index <- (i - 1) * num_demand + 1
  end_index <- i * num_demand
  constraint_matrix[i, start_index:end_index] <- 1
}

# Demand constraints
for(j in 1:num_demand) {
  indices <- seq(j, num_routes, by = num_demand)
  constraint_matrix[num_supply + j, indices] <- 1
}

# Constraint directions
constraint_directions <- c(rep("<=", num_supply), rep("=", num_demand))

# Combine supply and demand into a single vector
constraints_vector <- c(supply_vector, demand_vector)

# Solve the linear programming problem
lp_solution <- lp("min", cost_matrix, constraint_matrix, constraint_directions, constraints_vector)

# Check the solution status and optimized routes
lp_solution$status
lp_solution$solution

# Load necessary libraries
p_load(lpSolve)
p_load(igraph)
p_load(ggplot2)
p_load(plotly)


# Check if the solution is optimal
if(lp_solution$status == 0) {
  # Extract the optimized solution
  optimized_solution <- matrix(lp_solution$solution, nrow = nrow(total_supply), ncol = nrow(total_demand))
  
  # Create a data frame for network visualization
  network_data <- expand.grid(Supply = total_supply$PLANT_ID, Demand = total_demand$DESTINATION_PORT)
  network_data$Flow <- as.vector(optimized_solution)
  network_data <- network_data[network_data$Flow > 0, ]
  
  # Network Visualization
  g <- graph_from_data_frame(network_data, directed = TRUE)
  plot(g, edge.label = E(g)$Flow)
  
  # Interactive Network Plot
  plot_ly(data = network_data, x = ~Supply, y = ~Demand, text = ~paste("Flow:", Flow),
          type = "scatter", mode = "markers+lines", color = ~Flow, marker = list(size = 10)) %>%
    layout(title = "Optimized Supply Chain Routes")
  
  # Cost Analysis
  total_cost_matrix <- cost_matrix * optimized_solution
  cost_data_long <- melt(total_cost_matrix)
  names(cost_data_long) <- c("Supply", "Demand", "Cost")
  
  ggplot(cost_data_long, aes(x = Supply, y = Demand, fill = Cost)) +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(title = "Transportation Cost Matrix", x = "Supply Point", y = "Demand Point")
  
  # Supply-Demand Balance Visualization
  total_supply_vector <- rowSums(optimized_solution)
  total_demand_vector <- colSums(optimized_solution)
  
  supply_pie <- data.frame(Point = total_supply$PLANT_ID, Quantity = total_supply_vector)
  demand_pie <- data.frame(Point = total_demand$DESTINATION_PORT, Quantity = total_demand_vector)
  
  ggplot(supply_pie, aes(x = "", y = Quantity, fill = Point)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = "Supply Distribution")
  
  ggplot(demand_pie, aes(x = "", y = Quantity, fill = Point)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = "Demand Distribution")
} else {
  print("Optimization was not successful.")
}


###############################################################################

###################################################################

#Warehouse Efficiency

colnames(df_dict[["WhCosts"]])
colnames(df_dict[["WhCapacities"]])

# Renaming column in WhCapacities to match WhCosts
df_dict[["WhCapacities"]] <- rename(df_dict[["WhCapacities"]], WH = PLANT_ID )

# Assuming 'wh_costs' and 'wh_capacities' are your datasets
data_warehouse <- merge(df_dict[["WhCosts"]], df_dict[["WhCapacities"]], by = "WH")

# Scale the data
data_scaled <- scale(data_warehouse[, -1]) # Exclude non-numeric columns

# Hierarchical clustering
hclust_result <- hclust(dist(data_scaled))
plot(hclust_result)

# Load necessary libraries
p_load(dplyr)
p_load(ggplot2)
p_load(factoextra)
p_load(heatmaply)

# Your existing code for merging and scaling warehouse data...

# Improved Hierarchical Clustering Dendrogram
hclust_result <- hclust(dist(data_scaled))
clusters <- cutree(hclust_result, k = 4) # Assuming 4 clusters
dend <- as.dendrogram(hclust_result)
labels_colors(dend) <- clusters
plot(dend, main = "Hierarchical Clustering of Warehouses")

# Heatmap of Warehouse Costs and Capacities
heatmaply(data_scaled, k_col = 2, k_row = 2, 
          labels_row = data_warehouse$WH,
          main = "Heatmap of Warehouse Costs and Capacities", 
          xlab = "Variables", ylab = "Warehouses")

# Cluster Analysis
data_warehouse$Cluster <- as.factor(clusters)
cluster_summary <- data_warehouse %>%
  group_by(Cluster) %>%
  summarize(Average_Cost = mean(COST_UNIT), Average_Capacity = mean(DAILY_CAPACITY))

# Visualization of Cluster Analysis
ggplot(cluster_summary, aes(x = Cluster, y = Average_Cost, fill = Cluster)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Cost per Cluster", x = "Cluster", y = "Average Cost")

ggplot(cluster_summary, aes(x = Cluster, y = Average_Capacity, fill = Cluster)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Capacity per Cluster", x = "Cluster", y = "Average Capacity")

# Scatter Plot of Warehouses by Cluster
ggplot(data_warehouse, aes(x = COST_UNIT, y = DAILY_CAPACITY, color = Cluster)) +
  geom_point() +
  labs(title = "Warehouse Cost vs Capacity by Cluster", x = "Cost per Unit", y = "Daily Capacity")



############################################################################

# Customer Service Levels

# Assuming 'orderList' is your dataset
lm_model <- lm(COST ~ SERVICE_LEVEL + WEIGHT + SHIP_AHEAD_DAY_COUNT + SHIP_LATE_DAY_COUNT, data = orderList)
summary(lm_model)

# Load necessary libraries
p_load(ggplot2)
p_load(gridExtra)
p_load(ggfortify)


# Visualization of Actual vs. Predicted Costs
orderList$Predicted_Cost <- predict(lm_model, orderList)
ggplot(orderList, aes(x = COST, y = Predicted_Cost)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("Actual vs Predicted Costs") +
  xlab("Actual Cost") +
  ylab("Predicted Cost")

# Visualization of Residuals
autoplot(lm_model, which = 1, data = orderList, smooth.colour = "blue")

# Visualization of Regression Line for Each Predictor
p1 <- ggplot(orderList, aes(x = WEIGHT, y = COST)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Cost vs Weight", x = "Weight", y = "Cost")

p2 <- ggplot(orderList, aes(x = SHIP_AHEAD_DAY_COUNT, y = COST)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Cost vs Ship Ahead Day Count", x = "Ship Ahead Day Count", y = "Cost")

p3 <- ggplot(orderList, aes(x = SHIP_LATE_DAY_COUNT, y = COST)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Cost vs Ship Late Day Count", x = "Ship Late Day Count", y = "Cost")

# Combine the plots
grid.arrange(p1, p2, p3, ncol = 2)

# Bar Plot of Coefficients (Importance of Predictors)
coef_df <- as.data.frame(summary(lm_model)$coefficients)
ggplot(coef_df, aes(x = rownames(coef_df), y = Estimate)) +
  geom_bar(stat = "identity") +
  labs(title = "Importance of Predictors in Linear Model", x = "Predictors", y = "Coefficients") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##################################################################################

#Predictive Analytics
# Splitting the dataset into training and testing sets
index <- createDataPartition(orderList$cluster, p = 0.8, list = FALSE)
rf_train_set <- orderList[index, ]
rf_test_set <- orderList[-index, ]
# Random Forest model
# Random Forest model with reduced complexity
rf_model <- randomForest(COST ~ WEIGHT + UNIT_QUANTITY + SERVICE_LEVEL, 
                         data = rf_train_set, 
                         ntree = 50,  # reduce the number of trees
                         maxnodes = 30)  # limit the size of each tree

rf_predictions <- predict(rf_model, rf_test_set)

# Evaluate model performance
mean((rf_predictions - rf_test_set$COST)^2) # Mean Squared Error

# Calculate Additional Performance Metrics
mse <- mean((rf_predictions - rf_test_set$COST)^2)
rmse <- sqrt(mse)
mae <- mean(abs(rf_predictions - rf_test_set$COST))

# Print Performance Metrics
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")

# Visualization of Actual vs Predicted Costs
rf_test_set$Predicted_Cost <- rf_predictions
ggplot(rf_test_set, aes(x = COST, y = Predicted_Cost)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("Actual vs Predicted Costs - Random Forest") +
  xlab("Actual Cost") +
  ylab("Predicted Cost")

# Residual Plot
rf_test_set$Residuals <- rf_test_set$COST - rf_test_set$Predicted_Cost
ggplot(rf_test_set, aes(x = Predicted_Cost, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals vs Predicted Costs") +
  xlab("Predicted Cost") +
  ylab("Residuals")

###################################################################################

#Carrier Performance Analysis

new_data1 <- OrderList

# Group by carriers and summarize
carrier_performance <- aggregate(cbind(SHIP_AHEAD_DAY_COUNT,SHIP_LATE_DAY_COUNT, COST) ~ CARRIER, data = new_data1, mean)

# View summarized data
carrier_performance

# Comparative Bar Plots for Average Ship Ahead and Late Day Counts
carrier_performance_long <- carrier_performance %>%
  pivot_longer(cols = c(SHIP_AHEAD_DAY_COUNT, SHIP_LATE_DAY_COUNT), 
               names_to = "Metric", values_to = "Value")

ggplot(carrier_performance_long, aes(x = CARRIER, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Ship Ahead and Late Day Counts by Carrier", x = "Carrier", y = "Average Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box Plots for Shipping Performance
new_data1_long <- new_data1 %>%
  select(CARRIER, SHIP_AHEAD_DAY_COUNT, SHIP_LATE_DAY_COUNT) %>%
  pivot_longer(cols = c(SHIP_AHEAD_DAY_COUNT, SHIP_LATE_DAY_COUNT), 
               names_to = "Metric", values_to = "Value")

ggplot(new_data1_long, aes(x = CARRIER, y = Value, fill = Metric)) +
  geom_boxplot() +
  labs(title = "Shipping Performance Distribution by Carrier", x = "Carrier", y = "Performance Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter Plot of Cost vs. Shipping Performance
ggplot(new_data1, aes(x = SHIP_AHEAD_DAY_COUNT, y = COST, color = CARRIER)) +
  geom_point() +
  labs(title = "Cost vs. Ship Ahead Day Count by Carrier", x = "Ship Ahead Day Count", y = "Cost") +
  theme_minimal()

# Similarly, you can create a scatter plot for Ship Late Day Count vs. Cost
ggplot(new_data1, aes(x = SHIP_LATE_DAY_COUNT, y = COST, color = CARRIER)) +
  geom_point() +
  labs(title = "Cost vs. Ship Late Day Count by Carrier", x = "Ship Late Day Count", y = "Cost") +
  theme_minimal()


##################################################################################

# time series analysis

p_load(forecast)

# Assuming 'orderList' has a time component
ts_data <- ts(orderList$COST, frequency = 12) # Monthly data

# Fit a time series model
ts_model <- auto.arima(ts_data)
# Forecast Future Values
ts_forecast <- forecast(ts_model, h = 12) # Forecasting for next 12 months


# Assuming 'orderList' has a time component
ts_data <- ts(orderList$COST, frequency = 12) # Monthly data

# Plot Original Time Series
ggplot() +
  geom_line(aes(x = time(ts_data), y = ts_data), color = "blue") +
  labs(title = "Monthly Cost Time Series", x = "Time", y = "Cost")

# Plot Forecast with Confidence Intervals
autoplot(ts_forecast) +
  labs(title = "Forecast of Monthly Costs", x = "Time", y = "Cost")

# Check Residuals of the Model
checkresiduals(ts_model)


##################################################################################

