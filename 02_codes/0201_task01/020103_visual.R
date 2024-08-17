

#-----------------------------------------------------------------------------------------------------------
# Project: P3 Assessment 
# 
# Purpose: This script is to complete step 02 for task 01 ie c
#          calculate weighted averages for on-track and off-track countries
# 
# Date: 16 August 2024
#-----------------------------------------------------------------------------------------------------------

df <- read_csv(file.path(outputData, "task01_weighted_data.csv"))

# Filter the data for the first indicator
MNCH_ANC4_data <- indicator_population_stats %>%
  filter(str_detect(indicator, 'MNCH_ANC4'))

# Create the first bar plot for MNCH_ANC4
MNCH_ANC4_plot <- ggplot(MNCH_ANC4_data, aes(x = factor(`Status.U5MR`, levels = c("Achieved", "On Track", "Acceleration Needed")), y = mean, fill = `Status.U5MR`)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_low, ymax = mean_upp), width = 0.2, color = "black") +
  scale_fill_manual(values = c("Achieved" = "darkgreen", "On Track" = "green", "Acceleration Needed" = "yellow")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + 
  labs(
    #title = "Percentage of women (aged 15-49 years) attended at least four times during pregnancy by any provider",
    x = "Country IGME classification to meet SDG target",
    y = "Mean Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  
  )


# Filter the data for the second indicator
MNCH_SAB_data <- indicator_population_stats %>%
  filter(str_detect(indicator, 'MNCH_SAB'))

# Create the bar plot for MNCH_SAB
MNCH_SAB_plot <- ggplot(MNCH_SAB_data, aes(x = factor(`Status.U5MR`, levels = c("Achieved", "On Track", "Acceleration Needed")), y = mean, fill = `Status.U5MR`)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_low, ymax = mean_upp), width = 0.2, color = "black") +
  scale_fill_manual(values = c("Achieved" = "darkgreen", "On Track" = "green", "Acceleration Needed" = "yellow")) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  
  labs(
    #title = "Percentage of deliveries attended by skilled health personnel",
    x = "Country IGME classification to meet SDG target",
    y = "Mean Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" 
  )

