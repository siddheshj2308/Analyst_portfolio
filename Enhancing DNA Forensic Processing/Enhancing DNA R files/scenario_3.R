# *****************************************************************************
# ************* 1.2 Loading Data **************
# *****************************************************************************
scenario_3 <- read.csv("C:/Users/sj01148/OneDrive - University of Surrey/Documents/operation analytics/Individual Assignment/excels/Scenario_3.csv", header=TRUE)
data_3 <- scenario_3

# *****************************************************************************
# ************* 1.3 Data Cleaning **************
# *****************************************************************************

# Remove records with missing values in a specific variable
data_3 <- data_3[!is.na(data_3$Enter.ID.database), ]

# Save the modified dataset
write.csv(data_3, "scenario2_refined_dataset.csv", row.names = FALSE)

# replacing NA values with 0
data_3[is.na(data_3)] <- 0

#renaming the column names
data_3$CSI.vist.ends <- data_3$Data.input.ends
data_3$Enter.Rapid.DNA.machine <- data_3$Enter.collect
data_3$Exit.Rapid.DNA.machine <- data_3$Exit.Weds.DNA.machine

#delete un necessary columns
data_3 <- data_3[, -which(names(data_3) %in% c("Data.input.begins","Data.input.ends","Exit.Weds.collect",
                                         "Exit.Friday.collection","Exit.Weds.DNA.machine","Exit.Friday.DNA.machine",
                                         "Enter.Weds.DNA.machine","Enter.Friday.DNA.machine"))]


# *****************************************************************************
# ************* 1.4 creating new variables **************
# *****************************************************************************

# Create a new variable by subtracting variables
data_3$Que.for.CSI.visit.3 <- data_3$CSI.visit.starts - data_3$Call.to.police

# # Create a new variable by subtracting variables
data_3$Que.Van.picks.up.3 <- data_3$Van.picks.up.sample - data_3$CSI.vist.ends

# Create a new variable by subtracting variables
data_3$Que.Sample.prep.3 <- data_3$Sample.prep.begins - data_3$Sample.arrives.at.lab

# Create a new variable by subtracting variables
data_3$Que.for.DNA.Machine.3 <- data_3$Enter.Rapid.DNA.machine - data_3$Sample.prep.ends

# Create a new variable by subtracting variables
data_3$Que.validation.3 <- data_3$Enter.validation.process-data_3$Exit.Rapid.DNA.machine

# Create a new variable by subtracting variables
data_3$Que.Enter.ID.database.3 <- data_3$Enter.ID.database - data_3$Exit.validation

## Create a new variable by subtracting variables
data_3$Total.Time.In.System.3 <- data_3$Enter.ID.database-data_3$Crime.committed


# *****************************************************************************
# ************* 1.6 Visualisaion **************
# *****************************************************************************

# Filter original dataset df into just the original scenario
orig <- data_3

orig[is.na(orig)] <- 0

# Produce a nice plot of the Que for CSI visit
ggplot(orig, aes(x=Que.for.CSI.visit.3)) + 
  geom_histogram(bins = 60) + 
  geom_vline(xintercept = mean(orig$Que.for.CSI.visit..3), color = 'red') +
  geom_vline(xintercept = quantile(orig$Que.for.CSI.visit.3, 0.05), 
             color = 'red', linetype='dotted') +
  geom_vline(xintercept = quantile(orig$Que.for.CSI.visit.3, 0.95), 
             color = 'red', linetype='dotted') +
  theme_minimal() + 
  xlab("Que for CSI visit (minutes)") + ylab("Frequency") + 
  theme(text = element_text(size = 16))

# Produce a nice plot of the Que for CSI visit
ggplot(orig, aes(x=Que.for.DNA.Machine.3)) + 
  geom_histogram(bins = 50) + 
  geom_vline(xintercept = mean(orig$Que.Data.input.3), color = 'red') +
  geom_vline(xintercept = quantile(orig$Que.Data.input.3, 0.05), 
             color = 'red', linetype='dotted') +
  geom_vline(xintercept = quantile(orig$Que.Data.input.3, 0.95), 
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
corrs <- data_3 %>%
  summarise(Corr = cor(Que.for.CSI.visit.3, Total.Time.In.System.3),
            Corr2 = cor(Que.Van.picks.up.3, Total.Time.In.System.3),
            Corr3 = cor(Que.Sample.prep.3, Total.Time.In.System.3),
            Corr4 = cor(Que.for.DNA.Machine.3, Total.Time.In.System.3),
            Corr5 = cor(Que.validation.3, Total.Time.In.System.3))%>%
  pivot_longer(cols = starts_with("Corr"), names_to = "Activity", values_to = "Corr")

# Create the tornado plot
corrs %>%
  mutate(Activity = factor(Activity, levels = c("Corr", "Corr2", "Corr3", "Corr4", "Corr5")),
         Activity = recode(Activity, Corr = "CSI Visit",
                           Corr2 = "Courier",
                           Corr3 = "Sample Prep",
                           Corr4 = "Machine",
                           Corr5 = "Validation")) %>%
  ggplot(aes(x = Activity, y = Corr)) +
  geom_bar(stat = "identity", fill = "#6A8D98", alpha = 0.6, width = 0.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# *****************************************************************************
# ************* 1.9 5th and 95th percentile **************
# *****************************************************************************

# Define the columns for which you want to calculate the summary
columns <- c("Que.for.CSI.visit.3", "Que.Van.picks.up.3", "Que.Sample.prep.3",
             "Que.for.DNA.Machine.3", "Que.validation.3", "Que.Enter.ID.database.3")

# Initialize an empty list to store the results
summary_list <- list()

# Loop through the columns and calculate the summary statistics
for (col in columns) {
  summary_data <- data_3 %>%
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

