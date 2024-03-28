# *****************************************************************************
# *** 1.1 Loading libraries ****************
# *****************************************************************************
install.packages("pacman")
library(pacman)
p_load(dplyr)
p_load(lubridate)
p_load(ggplot2)    # Visualization 
p_load(forcats)
p_load(caret)  #featurePlot,
p_load(psych)  #describe,
p_load(readxl)
p_load(lattice)
p_load(tidyverse)

# *****************************************************************************
# ************* 1.2 Loading Data **************
# *****************************************************************************
current_process <- read.csv("C:/Users/sj01148/OneDrive - University of Surrey/Documents/operation analytics/Individual Assignment/excels/current_process_time_spreadsheet.csv", header=TRUE)
data <- current_process
# *****************************************************************************
# ************* 1.3 Data Cleaning **************
# *****************************************************************************
# Remove records with missing values in a specific variable
data <- data[!is.na(data$Enter.ID.database), ]

# Save the modified dataset
write.csv(data, "current_refined_dataset.csv", row.names = FALSE)

# replacing NA values with 0
data[is.na(data)] <- 0

#create new column by adding two column
data$Exit.collect <- data$Exit.Weds.collect+ data$Exit.Friday.collection

data$Enter.DNA.machine <- data$Enter.Weds.DNA.machine+ data$Enter.Friday.DNA.machine

data$Exit.DNA.machine <- data$Exit.Weds.DNA.machine+ data$Exit.Friday.DNA.machine

data$Enter.validation <- data$Enter.validation.process+ data$Enter.validation.process2

# *****************************************************************************
# ************* 1.4 creating new variables **************
# *****************************************************************************
# Create a new variable by subtracting variables
data$Que.for.CSI.visit <- data$CSI.visit.starts - data$Call.to.police

# Create a new variable by subtracting variables
data$Que.Data.input <- data$Data.input.begins - data$CSI.vist.ends

# # Create a new variable by subtracting variables
data$Que.Van.picks.up <- data$Van.picks.up.sample - data$Data.input.ends

# Create a new variable by subtracting variables
data$Que.Sample.prep <- data$Sample.prep.begins - data$Sample.arrives.at.lab

# Create a new variable by subtracting variables
data$Que.collect <- data$Enter.collect - data$Sample.prep.ends

# # turn the columns to numeric
# data$Enter.validation <- as.numeric(data$Enter.validation)
# data$Exit.DNA.machine <- as.numeric(data$Exit.DNA.machine)

# Create a new variable by subtracting variables
data$Que.validation <- data$Enter.validation-data$Exit.DNA.machine

# Create a new variable by subtracting variables
data$Que.Enter.ID.database <- data$Enter.ID.database - data$Exit.validation

## Create a new variable by subtracting variables
data$CSI.Visit <- data$CSI.vist.ends-data$CSI.visit.starts

## Create a new variable by subtracting variables
data$Data.input <- data$Data.input.ends-data$Data.input.begins

## Create a new variable by subtracting variables
data$Transport.Time <- data$Sample.arrives.at.lab-data$Van.picks.up.sample

## Create a new variable by subtracting variables
data$Sample.prep <- data$Sample.prep.ends-data$Sample.prep.begins

## Create a new variable by subtracting variables
data$DNA.machine <- data$Exit.DNA.machine- data$Enter.DNA.machine

# ## Create a new variable by subtracting variables
# data$DNA.machine <- as.numeric(data$Exit.DNA.machine)- as.numeric(data$Enter.DNA.machine)

## Create a new variable by subtracting variables
data$validation <- data$Exit.validation-data$Enter.validation

## Create a new variable by subtracting variables
data$Total.Time.In.System <- data$Enter.ID.database-data$Crime.committed

# *****************************************************************************
# ************* 1.6 Visualisaion **************
# *****************************************************************************

# Filter original dataset df into just the original scenario
orig <- data

orig[is.na(orig)] <- 0

# Produce a nice plot of the Que for CSI visit
ggplot(orig, aes(x=Que.for.CSI.visit)) + 
  geom_histogram(bins = 60) + 
  geom_vline(xintercept = mean(orig$Que.for.CSI.visit), color = 'red') +
  geom_vline(xintercept = quantile(orig$Que.for.CSI.visit, 0.05), 
             color = 'red', linetype='dotted') +
  geom_vline(xintercept = quantile(orig$Que.for.CSI.visit, 0.95), 
             color = 'red', linetype='dotted') +
  theme_minimal() + 
  xlab("Que for CSI visit (minutes)") + ylab("Frequency") + 
  theme(text = element_text(size = 16))

# ggplot(orig, aes(x = Que.Data.input)) + 
#   geom_histogram(bins = 50) + 
#   geom_vline(xintercept = mean(orig$Que.Data.input), color = 'red') +
#   geom_vline(xintercept = quantile(orig$Que.Data.input, 0.05), color = 'red', linetype = 'dotted') +
#   geom_vline(xintercept = quantile(orig$Que.Data.input, 0.95), color = 'red', linetype = 'dotted') +
#   geom_text(
#     aes(x = Que.Data.input, y = 0, label = ..count..),
#     stat = "count",
#     vjust = -1.5,    size = 4,
#     color = "black"
#   ) +
#   theme_minimal() + 
#   xlab("Que for Data Input (minutes)") + ylab("Frequency") + 
#   theme(text = element_text(size = 16))


# Combine the variables into a single column
orig_long <- tidyr::pivot_longer(orig, cols = c(Que.for.CSI.visit, Que.Data.input, Que.Sample.prep,
                                                Que.validation, Que.Enter.ID.database, Que.collect))

# Define custom colors for each variable
custom_colors <- c("skyblue", "pink", "lightgreen", "orange", "purple", "yellow")

# Create a comparative plot using facet_wrap
comparative_plot <- ggplot(orig_long, aes(x = value, fill = name)) +
  geom_histogram(bins = 10, color = "black", position = "identity") +
  geom_vline(aes(xintercept = mean(value)), color = 'red') +
  geom_vline(aes(xintercept = quantile(value, 0.05)), color = 'red', linetype = 'dotted') +
  geom_vline(aes(xintercept = quantile(value, 0.95)), color = 'red', linetype = 'dotted') +
  labs(title = "Comparative Histograms",
       x = "Time (minutes)",
       y = "Frequency") +
  facet_wrap(~ name, nrow = 2, ncol = 3) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(text = element_text(size = 12))

# Print the comparative plot
print(comparative_plot)


# *****************************************************************************
# ************* 1.8 Tornado PlOt **************
# *****************************************************************************


library(tidyverse)

# Generate correlation coefficients for each input against output (total weeks)
corrs <- data %>%
  summarise(Corr = cor(Que.for.CSI.visit, Total.Time.In.System),
            Corr2 = cor(Que.Data.input, Total.Time.In.System),
            Corr4 = cor(Que.Sample.prep, Total.Time.In.System),
            Corr5 = cor(Que.collect, Total.Time.In.System),
            Corr6 = cor(Que.validation, Total.Time.In.System),
            Corr7 = cor(Que.Enter.ID.database, Total.Time.In.System)) %>%
  pivot_longer(cols = starts_with("Corr"), names_to = "Activity", values_to = "Corr")

# Create the tornado plot with conditional color filling
corrs %>%
  mutate(Activity = factor(Activity, levels = c("Corr", "Corr2", "Corr4", "Corr5", "Corr6", "Corr7")),
         Activity = recode(Activity, Corr = "CSI Visit",
                           Corr2 = "Data Input",
                           Corr4 = "Sample Prep",
                           Corr5 = "Collect",
                           Corr6 = "Validation",
                           Corr7 = "Enter ID Database"),
         Fill = ifelse(Corr > 0, "#6A8D98", "#E15759")) %>%
  ggplot(aes(x = Activity, y = Corr, fill = Fill)) +
  geom_bar(stat = "identity", alpha = 0.6, width = 0.4) +
  coord_flip() +
  xlab("") +
  scale_fill_identity() +
  theme_bw()


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









# # *****************************************************************************
# # ************* 1.7 Spider PlOt **************
# # *****************************************************************************
# # Create percentiles for each input
# library(dplyr)
# # CSI.Visit<-  Data.input+Transport.Time+Sample.prep+DNA.machine+validation+Total.Time.In.System 
# #################################################################################
# 
# 
# # # Create percentiles for each input
#  df2 <- as.data.frame(matrix(NA, 5077, 7))
#  colnames(df2) <- c("CSI.Visit", "Data.input", "Transport.Time", "Sample.prep", "DNA.machine", "validation", "Total.Time.In.System")
#  
#  for (i in 1:7){
#    name <- colnames(df2)[i]
#    df2[,i] <- data %>% 
#      select(i) %>%
#      mutate(quantile = ntile(data[,i], 20)) %>% 
#      select(quantile)
#  }
#  print(df2)
# # # Create array of median Total.Weeks at each input percentile
#  df3 <- as.data.frame(matrix(NA, 20, 8))
#  colnames(df3) <- c("Percentile", "CSI.Visit", "Data.input", "Transport.Time", "Sample.prep", 
#                     "DNA.machine", "validation", "Total.Time.In.System")
#  
#  df3$Percentile <- seq(5,100,5)
#  for (i in 1:20) {
#    for (j in 2:8) {
#      included <- df2[,j-1] == i
#      df3[i,j] <- median(data$Total.Time.In.System[included])
#    }
#  }
# # Create a flatfile dataframe from which you can plot the spiderplot using ggplot2
# median.minutes <- df3 %>% pivot_longer(!Percentile, names_to = "Activity", values_to = 
#                                        "mean.Tot.minutes")
# # Transpose df3 and reshape it to long format
# df_long <- df3 %>% 
#   pivot_longer(cols = -Percentile, names_to = "Activity", values_to = "median.minutes")
# 
# # Create spider plot
# spiderplot <- ggplot(df_long, aes(x = Percentile, y = median.minutes, color = Activity)) +
#   geom_line() +
#   scale_colour_brewer(palette = "Set1") +
#   labs(x = "Percentile", y = "Median.minutes") +
#   theme_minimal()
# 
# # Display the spider plot
# print(spiderplot)



# #################################################################################
# ##Create percentiles for each input
# df2 <- as.data.frame(matrix(NA, 5077, 7))
# colnames(df2) <- c("CSI.Visit", "Data.input", "Transport.Time", "Sample.prep", "DNA.machine", "validation", "Total.Time.In.System")
# 
# for (i in 1:20) {
#   unique_vals <- unique(data[, i])
#   n_quantiles <- min(length(unique_vals), 5077)
#   df2[, i] <- ntile(pull(data, colnames(data)[i]), n_quantiles)
# }
# 
# df2 <- df2[!duplicated(df2$Total.Time.In.System), ]
# 
# ##Create array of median Total.time in system at each input percentile
# df3 <- as.data.frame(matrix(NA, 20, 8))
# colnames(df3) <- c("Percentile", "CSI.Visit", "Data.input", "Transport.Time", "Sample.prep", "DNA.machine", "validation", "Total.Time.In.System")
# df3$Percentile <- seq(5, 100, 5)
# 
# for (i in 1:20) {
#   for (j in 3:9) {
#     included <- df2[, j] == i
#     df3[i, j - 1] <- median(orig$Total.Time.In.System[included])
#   }
# }
# 
# ##Create a flatfile dataframe from which you can plot the spiderplot using ggplot2
# median.minutes <- df3 %>%
#   pivot_longer(!Percentile, names_to = "Activity", values_to = "mean.total.time.in.minute")
# 
# library(ggplot2)
# library(RColorBrewer)
# library(tidyverse)
# 
# ##Transpose df3 and reshape it to long format
# df_long <- df3 %>%
#   pivot_longer(cols = -Percentile, names_to = "Activity", values_to = "MedianTotalminutes")
# 
# ##Create spider plot
# spiderplot <- ggplot(df_long, aes(x = Percentile, y = MedianTotalminutes, color = Activity)) +
#   geom_line() +
#   scale_colour_brewer(palette = "Set1") +
#   labs(x = "Percentile", y = "MedianTotalminutes") +
#   theme_minimal()
# 
# print(spiderplot)


#############################################################################

# visualisation to choose from
##########################################################################
# #Box Plot
# ggplot(orig, aes(x = "", y = Que.Data.input)) +
#   geom_boxplot() +
#   xlab("") + ylab("Que for Data Input (minutes)") +
#   theme_minimal() +
#   theme(text = element_text(size = 16))
# #Density Plot
# ggplot(orig, aes(x = Que.Data.input)) +
#   geom_density(fill = "blue", alpha = 0.5) +
#   xlab("Que for Data Input (minutes)") + ylab("Density") +
#   theme_minimal() +
#   theme(text = element_text(size = 16))
# #Violin Plot
# ggplot(orig, aes(x = "", y = Que.Data.input)) +
# geom_violin(fill = "lightblue", alpha = 0.7) +
#   geom_boxplot(width = 0.2, fill = "white", color = "black") +
#   xlab("") + ylab("Que for Data Input (minutes)") +
#   theme_minimal() +
#   theme(text = element_text(size = 16))
# # Bar Plot
# ggplot(orig, aes(x = Que.Data.input)) +
#   geom_bar(binwidth=30) +
#   geom_vline(xintercept = mean(orig$Que.Data.input), color = 'red') +
#   geom_vline(xintercept = quantile(orig$Que.Data.input, 0.05), 
#              color = 'red', linetype = 'dotted') +
#   geom_vline(xintercept = quantile(orig$Que.Data.input, 0.95), 
#              color = 'red', linetype = 'dotted') +
#   theme_minimal() + 
#   xlab("Que for Data Input (minutes)") + ylab("Frequency") + 
#   theme(text = element_text(size = 16))
# # frequency polygon
# ggplot(orig, aes(x = Que.Data.input)) +
#   geom_freqpoly() +
#   geom_vline(xintercept = mean(orig$Que.Data.input), color = 'red') +
#   geom_vline(xintercept = quantile(orig$Que.Data.input, 0.05), 
#              color = 'red', linetype = 'dotted') +
#   geom_vline(xintercept = quantile(orig$Que.Data.input, 0.95), 
#              color = 'red', linetype = 'dotted') +
#   theme_minimal() + 
#   xlab("Que for Data Input (minutes)") + ylab("Frequency") + 
#   theme(text = element_text(size = 16))
# #dot plot
# ggplot(orig, aes(x = Que.Data.input)) +
#   geom_dotplot(binwidth = 3) +  # Change the binwidth value to 2
#   geom_vline(xintercept = mean(orig$Que.Data.input), color = 'red') +
#   geom_vline(xintercept = quantile(orig$Que.Data.input, 0.05), 
#              color = 'red', linetype = 'dotted') +
#   geom_vline(xintercept = quantile(orig$Que.Data.input, 0.95), 
#              color = 'red', linetype = 'dotted') +
#   theme_minimal() + 
#   xlab("Que for Data Input (minutes)") + ylab("Frequency") + 
#   theme(text = element_text(size = 16))


