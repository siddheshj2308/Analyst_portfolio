# *****************************************************************************
# ************* 1.2 Loading Data **************
# *****************************************************************************
scenario_1 <- read.csv("C:/Users/sj01148/OneDrive - University of Surrey/Documents/operation analytics/Individual Assignment/excels/Scenario_1.csv", header=TRUE)
data_1 <- scenario_1

# *****************************************************************************
# ************* 1.3 Data Cleaning **************
# *****************************************************************************

# Remove records with missing values in a specific variable
data_1 <- data_1[!is.na(data_1$Enter.ID.database), ]

# Save the modified dataset
write.csv(data_1, "scenario1_refined_dataset.csv", row.names = FALSE)

# replacing NA values with 0
data_1[is.na(data_1)] <- 0

#renaming the column names
data_1$Enter.Rapid.DNA.machine <- data_1$Enter.collect
data_1$Exit.Rapid.DNA.machine <- data_1$Exit.Weds.DNA.machine

#delete un necessary columns
data_1 <- data_1[, -which(names(data_1) %in% c("Exit.Weds.collect", "Enter.Weds.DNA.machine",
                                         "Exit.Weds.DNA.machine","Enter.validation.process2",
                                         "Exit.Friday.collection","Enter.Friday.DNA.machine",
                                         "Exit.Friday.DNA.machine","Enter.collect","Exit.collect"))]


# *****************************************************************************
# ************* 1.4 creating new variables **************
# *****************************************************************************

# Create a new variable by subtracting variables
data_1$Que.for.CSI.visit.1 <- data_1$CSI.visit.starts - data_1$Call.to.police

# Create a new variable by subtracting variables
data_1$Que.data.input.1 <- data_1$Data.input.begins - data_1$CSI.vist.ends

# # Create a new variable by subtracting variables
data_1$Que.Van.picks.up.1 <- data_1$Van.picks.up.sample - data_1$Data.input.ends

# Create a new variable by subtracting variables
data_1$Que.Sample.prep.1 <- data_1$Sample.prep.begins - data_1$Sample.arrives.at.lab

# Create a new variable by subtracting variables
data_1$Que.for.DNA.Machine.1 <- data_1$Enter.Rapid.DNA.machine - data_1$Sample.prep.ends

# Create a new variable by subtracting variables
data_1$Que.validation.1 <- data_1$Enter.validation.process-data_1$Exit.Rapid.DNA.machine

# Create a new variable by subtracting variables
data_1$Que.Enter.ID.database.1 <- data_1$Enter.ID.database - data_1$Exit.validation

## Create a new variable by subtracting variables
data_1$Total.Time.In.System.1 <- data_1$Enter.ID.database-data_1$Crime.committed


# *****************************************************************************
# ************* 1.6 Visualisaion **************
# *****************************************************************************

# Filter original dataset df into just the original scenario
orig <- data_1

orig[is.na(orig)] <- 0

# Produce a nice plot of the Que for CSI visit
ggplot(orig, aes(x=Que.for.CSI.visit.1)) + 
  geom_histogram(bins = 60) + 
  geom_vline(xintercept = mean(orig$Que.for.CSI.visit.1), color = 'red') +
  geom_vline(xintercept = quantile(orig$Que.for.CSI.visit.1, 0.05), 
             color = 'red', linetype='dotted') +
  geom_vline(xintercept = quantile(orig$Que.for.CSI.visit.1, 0.95), 
             color = 'red', linetype='dotted') +
  theme_minimal() + 
  xlab("Que for CSI visit (minutes)") + ylab("Frequency") + 
  theme(text = element_text(size = 16))

# Produce a nice plot of the Que for CSI visit
ggplot(orig, aes(x=Que.validation.1)) + 
  geom_histogram(bins = 50) + 
  geom_vline(xintercept = mean(orig$Que.validation), color = 'red') +
  geom_vline(xintercept = quantile(orig$Que.validation, 0.05), 
             color = 'red', linetype='dotted') +
  geom_vline(xintercept = quantile(orig$Que.validation, 0.95), 
             color = 'red', linetype='dotted') +
  theme_minimal() + 
  xlab("Que for Data Input (minutes)") + ylab("Frequency") + 
  theme(text = element_text(size = 16))
# *****************************************************************************
# ************* 1.8 Tornado PlOt **************
# *****************************************************************************
#Corr7 = cor(Que.Enter.ID.database, Total.Time.In.System

library(tidyverse)

# Generate correlation coefficients for each input against output (total weeks)
corrs <- data_1 %>%
  summarise(Corr = cor(Que.for.CSI.visit.1, Total.Time.In.System.1),
            Corr2 = cor(Que.data.input.1, Total.Time.In.System.1),
            Corr3 = cor(Que.Sample.prep.1, Total.Time.In.System.1),
            Corr4 = cor(Que.for.DNA.Machine.1, Total.Time.In.System.1),
            Corr5 = cor(Que.validation.1, Total.Time.In.System.1))%>%
  pivot_longer(cols = starts_with("Corr"), names_to = "Activity", values_to = "Corr")

# Create the tornado plot
corrs %>%
  mutate(Activity = factor(Activity, levels = c("Corr", "Corr2", "Corr3", "Corr4", "Corr5")),
         Activity = recode(Activity, Corr = "CSI Visit",
                           Corr2 = "Data Input",
                           Corr3 = "Sample Prep",
                           Corr4 = "DNA machine",
                           Corr5 = "Validation")) %>%
  ggplot(aes(x = Activity, y = Corr)) +
  geom_bar(stat = "identity", fill = "#6A8D98", alpha = 0.6, width = 0.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

library(dplyr)

# Define the columns for which you want to calculate the summary
columns <- c("Que.for.CSI.visit", "Que.Data.input", "Que.Van.picks.up", "Que.Sample.prep",
             "Que.collect", "Que.validation", "Que.Enter.ID.database")

# Initialize an empty list to store the results
summary_list <- list()

# Loop through the columns and calculate the summary statistics
for (col in columns) {
  summary_data <- data %>%
    summarize(
      variable = col,
      mean_value = mean(!!sym(col)),
      percentile_5 = quantile(!!sym(col), 0.05),
      percentile_95 = quantile(!!sym(col), 0.95)
    )
  
  # Store the summary data in the list
  summary_list[[col]] <- summary_data
}

# Combine the summary data frames into one table
summary_table <- bind_rows(summary_list)

# Print the summary table
print(summary_table)

# *****************************************************************************
# ************* 1.9 5th and 95th percentile **************
# *****************************************************************************

# Define the columns for which you want to calculate the summary
columns <- c("Que.for.CSI.visit.1", "Que.data.input.1", "Que.Van.picks.up.1", "Que.Sample.prep.1",
             "Que.for.DNA.Machine.1", "Que.validation.1", "Que.Enter.ID.database.1")

# Initialize an empty list to store the results
summary_list <- list()

# Loop through the columns and calculate the summary statistics
for (col in columns) {
  summary_data <- data_1 %>%
    summarize(
      variable = col,
      mean_value = mean(!!sym(col)),
      percentile_5 = quantile(!!sym(col), 0.05),
      percentile_95 = quantile(!!sym(col), 0.95)
    )
  
  # Store the summary data in the list
  summary_list[[col]] <- summary_data
}

# Combine the summary data frames into one table
summary_table <- bind_rows(summary_list)

# Print the summary table
print(summary_table)

