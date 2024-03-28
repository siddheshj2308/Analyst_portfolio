# *****************************************************************************
# ************* 1.2 Loading Data **************
# *****************************************************************************
scenario_4 <- read.csv("C:/Users/sj01148/OneDrive - University of Surrey/Documents/operation analytics/Individual Assignment/excels/Scenario_4.csv", header=TRUE)
data_4 <- scenario_4

# *****************************************************************************
# ************* 1.3 Data Cleaning **************
# *****************************************************************************

# Remove records with missing values in a specific variable
data_4 <- data_4[!is.na(data_4$Enter.ID.database), ]

# Save the modified dataset
write.csv(data_4, "scenario2_refined_dataset.csv", row.names = FALSE)

# replacing NA values with 0
data_4[is.na(data_4)] <- 0

#renaming the column names
data_4$CSI.vist.ends <- data_4$Data.input.ends
data_4$Enter.Rapid.DNA.machine <- data_4$Enter.collect
data_4$Exit.Rapid.DNA.machine <- data_4$Exit.Weds.DNA.machine

#delete un necessary columns
data_4 <- data_4[, -which(names(data_4) %in% c("Data.input.begins","Data.input.ends","Exit.Weds.collect",
                                         "Exit.Friday.collection","Exit.Weds.DNA.machine","Exit.Friday.DNA.machine",
                                         "Enter.Weds.DNA.machine","Enter.Friday.DNA.machine"))]


# *****************************************************************************
# ************* 1.4 creating new variables **************
# *****************************************************************************

# Create a new variable by subtracting variables
data_4$Que.for.CSI.visit.4 <- data_4$CSI.visit.starts - data_4$Call.to.police

# # Create a new variable by subtracting variables
data_4$Que.Van.picks.up.4 <- data_4$Van.picks.up.sample - data_4$CSI.vist.ends

# Create a new variable by subtracting variables
data_4$Que.Sample.prep.4 <- data_4$Sample.prep.begins - data_4$Sample.arrives.at.lab

# Create a new variable by subtracting variables
data_4$Que.for.DNA.Machine.4 <- data_4$Enter.Rapid.DNA.machine - data_4$Sample.prep.ends

# Create a new variable by subtracting variables
data_4$Que.validation.4 <- data_4$Enter.validation.process-data_4$Exit.Rapid.DNA.machine

# Create a new variable by subtracting variables
data_4$Que.Enter.ID.database.4 <- data_4$Enter.ID.database - data_4$Exit.validation

## Create a new variable by subtracting variables
data_4$Total.Time.In.System.4 <- data_4$Enter.ID.database-data_4$Crime.committed


# *****************************************************************************
# ************* 1.6 Visualisaion **************
# *****************************************************************************

# Filter original dataset df into just the original scenario
orig <- data_4

orig[is.na(orig)] <- 0

# Produce a nice plot of the Que for CSI visit
ggplot(orig, aes(x=Que.for.CSI.visit.4)) + 
  geom_histogram(bins = 60) + 
  geom_vline(xintercept = mean(orig$Que.for.CSI.visit.4), color = 'red') +
  geom_vline(xintercept = quantile(orig$Que.for.CSI.visit, 0.05), 
             color = 'red', linetype='dotted') +
  geom_vline(xintercept = quantile(orig$Que.for.CSI.visit.4, 0.95), 
             color = 'red', linetype='dotted') +
  theme_minimal() + 
  xlab("Que for CSI visit (minutes)") + ylab("Frequency") + 
  theme(text = element_text(size = 16))

# Produce a nice plot of the Que for CSI visit
ggplot(orig, aes(x=Que.for.DNA.Machine.4)) + 
  geom_histogram(bins = 50) + 
  geom_vline(xintercept = mean(orig$Que.for.DNA.Machine.4), color = 'red') +
  geom_vline(xintercept = quantile(orig$Que.for.DNA.Machine.4, 0.05), 
             color = 'red', linetype='dotted') +
  geom_vline(xintercept = quantile(orig$Que.for.DNA.Machine.4, 0.95), 
             color = 'red', linetype='dotted') +
  theme_minimal() + 
  xlab("Que for DNA (minutes)") + ylab("Frequency") + 
  theme(text = element_text(size = 16))
# *****************************************************************************
# ************* 1.8 Tornado PlOt **************
# *****************************************************************************
#Corr7 = cor(Que.Enter.ID.database, Total.Time.In.System

library(tidyverse)

# Generate correlation coefficients for each input against output (total weeks)
corrs <- data_4 %>%
  summarise(Corr = cor(Que.for.CSI.visit.4, Total.Time.In.System.4),
            Corr2 = cor(Que.Van.picks.up.4, Total.Time.In.System.4),
            Corr3 = cor(Que.Sample.prep.4, Total.Time.In.System.4),
            Corr4 = cor(Que.validation.4, Total.Time.In.System.4))%>%
  pivot_longer(cols = starts_with("Corr"), names_to = "Activity", values_to = "Corr")

# Create the tornado plot
corrs %>%
  mutate(Activity = factor(Activity, levels = c("Corr", "Corr2", "Corr3", "Corr4")),
         Activity = recode(Activity, Corr = "CSI Visit",
                           Corr2 = "Courier",
                           Corr3 = "Sample Prep",
                           Corr4 = "Validation")) %>%
  ggplot(aes(x = Activity, y = Corr)) +
  geom_bar(stat = "identity", fill = "#6A8D98", alpha = 0.6, width = 0.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# *****************************************************************************
# ************* 1.9 5th and 95th percentile **************
# *****************************************************************************

# Define the columns for which you want to calculate the summary
columns <- c("Que.for.CSI.visit.4", "Que.Van.picks.up.4", "Que.Sample.prep.4",
             "Que.for.DNA.Machine.4", "Que.validation.4", "Que.Enter.ID.database.4")

# Initialize an empty list to store the results
summary_list <- list()

# Loop through the columns and calculate the summary statistics
for (col in columns) {
  summary_data <- data_4 %>%
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

