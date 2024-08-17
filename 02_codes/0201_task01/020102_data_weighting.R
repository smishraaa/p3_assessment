#-----------------------------------------------------------------------------------------------------------
# Project: P3 Assessment 
# 
# Purpose: This script is to complete step 02 for task 01 ie 
#          calculate weighted averages for on-track and off-track countries
# 
# Date: 16 August 2024
#-----------------------------------------------------------------------------------------------------------

## Calculate the bith ratio by IGME classification groups
# Running a combined code is giving the incoorect sum of total births
# Therefore, we will calculate the birth ratio for each indicator separately and then combine the results

# Load the master data
master_data <- read_csv(file.path(outputData, "task01_master_data.csv"))

# First indicator: subsetting data
indic1 <- "MNCH_ANC4: Antenatal care 4+ visits - percentage of women (aged 15-49 years) attended at least four times during pregnancy by any provider"  # Replace with the actual name of the first indicator"
master_data_indic1 <- master_data %>%
  filter(indicator == indic1) %>%  # Filter for the first indicator
  select(iso3_country_code, Status.U5MR, total_birth, indicator)


# total sum of births for the first indicator to double check 
total_birth_sum_indic1 <- sum(master_data_indic1$total_birth, na.rm = TRUE)

# total sum of births by Status.U5MR for
total_birth_by_status_indic1 <- master_data_indic1 %>%
  group_by(Status.U5MR, indicator) %>%
  summarise(total_birth_sum = sum(total_birth, na.rm = TRUE)) %>%
  mutate(
    total_birth_indic1 = total_birth_sum_indic1,
    projected_birth_ratio = total_birth_sum / total_birth_indic1
  ) 

#######
#  second indicator
indic2 <- "MNCH_SAB: Skilled birth attendant - percentage of deliveries attended by skilled health personnel"  # Replace with the actual name of the second indicator
master_data_indic2 <- master_data %>%
  filter(indicator == indic2) %>%  # Filter for the first indicator
  select(iso3_country_code, Status.U5MR, total_birth, indicator)


# total sum of births for the first indicator to double check 
total_birth_sum_indic2 <- sum(master_data_indic2$total_birth, na.rm = TRUE)

# total sum of births by Status.U5MR for the second indicator
total_birth_by_status_indic2 <- master_data_indic2 %>%
  group_by(Status.U5MR, indicator) %>%
  summarise(total_birth_sum = sum(total_birth, na.rm = TRUE)) %>%
  mutate(
    total_birth_indic2 = total_birth_sum_indic2,
    projected_birth_ratio = total_birth_sum / total_birth_indic2
  )%>% rename(
    total_birth_sum_indic2 = total_birth_sum,
    projected_birth_ratio_indic2 = projected_birth_ratio
  )

# Bring  dataframes containing birth ratio of each indicator by different category together
combined_indicators <- bind_rows(total_birth_by_status_indic1, total_birth_by_status_indic2)

# Calcuate weighted average and combine with birth ratio 
indicator_population_stats <-
  read_csv(file.path(outputData, "task01_master_data.csv")) %>%
  as_survey(weight = total_birth) %>%
  group_by(indicator, Status.U5MR) %>%
  summarise(mean = survey_mean(observation_value, vartype = "ci")) %>%
  left_join(combined_indicators, by = c("indicator", "Status.U5MR"))  %>%
  mutate_if(is.numeric, round, digits = 4)

write_csv(indicator_population_stats, file.path(outputData, "task01_weighted_data.csv"))
