
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
  facet_wrap(~ Dataset, nrow = 1) +
  labs(title = "Comparison of times in system across ",
       x = "Times In System",
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33"))

#******************************************************************************
#********
#******************************************************************************* 








#current scenario 1 scenario 2 scenario 3 scenario 4 scenario 5
