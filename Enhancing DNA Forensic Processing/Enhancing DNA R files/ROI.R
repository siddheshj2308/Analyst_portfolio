library(ggplot2)
library(tidyr)

# Step 1: Merge the datasets (assuming datasets are named data, data_1, data_2, data_3, data_4, data_5)
merged_data <- bind_rows(data, data_1, data_2, data_3, data_4, data_5)

# Step 2: Reshape the data and rename the columns
reshaped_data <- pivot_longer(merged_data,
                              cols = c(Total.Time.In.System, Total.Time.In.System.1,
                                       Total.Time.In.System.2, Total.Time.In.System.3,
                                       Total.Time.In.System.4, Total.Time.In.System.5),
                              names_to = "Dataset", values_to = "Value") %>%
  mutate(Dataset = case_when(
    Dataset == "Total.Time.In.System" ~ "current",
    Dataset == "Total.Time.In.System.1" ~ "scenario 1",
    Dataset == "Total.Time.In.System.2" ~ "scenario 2",
    Dataset == "Total.Time.In.System.3" ~ "scenario 3",
    Dataset == "Total.Time.In.System.4" ~ "scenario 4",
    Dataset == "Total.Time.In.System.5" ~ "scenario 5"
  ))

# Step 3: Create the plot with facet_wrap and custom colors
ggplot(reshaped_data, aes(x = Value, fill = Dataset)) +
  geom_histogram(bins = 10, color = "black", alpha = 0.7, na.rm = TRUE) +
  geom_vline(aes(xintercept = 1440), color = "violet", linetype = "solid") +  # Add threshold line at 1 day (1440 minutes)
  geom_vline(aes(xintercept = 2880), color = "purple", linetype = "solid") +  # Add threshold line at 2 days (2880 minutes)
  facet_wrap(~ Dataset, nrow = 1) +
  labs(title = "Comparison of times in system",
       x = "Times In System",
       y = "Frequency",
       caption = "lines: Violet - Day 1, Purple - Day 2") +  # Add caption with labels
  theme_minimal() +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33"))

#******************************************************************************
#******** calculate ROI ************************************************
#******************************************************************************* 


# *****************************************************************************
# ************* 1.9 5th and 95th percentile **************
# *****************************************************************************

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

# Calculate ROI for values above the threshold lines
roi_values <- summary_table %>%
  mutate(
    roi_day1 = if_else(mean_value > 1440, (mean_value - 1440) / 1440 * 100, 0),
    roi_day2 = if_else(mean_value > 2880, (mean_value - 2880) / 2880 * 100, 0)
  )

# Print the ROI values
print(roi_values)

