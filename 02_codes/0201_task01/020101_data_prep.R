#-----------------------------------------------------------------------------------------------------------
# Project: P3 Assessment 
# 
# Purpose: This script completes step 1 and 2 of  task 1 i.e. cleans and merged the data.
#          filters for most recent values anc calculates the weighted estimates
# 
# Date: 16 August 2024
#-----------------------------------------------------------------------------------------------------------
# 
# Please run user_profile script or if username is set, run the following code
# source(user_profile.R)
# source(user_packages.R)

# Load the cnt_status.csv file and rename the column
# Extracting ISO code as common identifier across files

indicator_status <- read_csv(file.path(inputData, "cnt_status.csv")) %>%
  rename(iso3_country_code = ISO3Code)

# extract unique iso to analyse only unicef countries
unicef_iso3_country_code <- indicator_status %>% distinct(iso3_country_code) %>% pull(iso3_country_code)

# Load wpp.csv, clean and filter the data
df_birth_data <- read_csv(file.path(inputData, "wpp.csv"), skip = 1) %>%
  filter(Year == 2022, `ISO3 Alpha-code` %in% unicef_iso3_country_code) %>%
  select(`ISO3 Alpha-code`, `Births (thousands)`) %>%
  rename(iso3_country_code = `ISO3 Alpha-code`, total_birth = `Births (thousands)`) %>%
  mutate(total_birth = str_replace_all(total_birth, " ", "") %>% as.numeric())

# clean dw data
# dw data does not include totals coloumn, total is conditional on residence and wealth 'totals'

# Function to extract the string after the colon and remove special characters
extract_after_colon <- function(text) {
  str_trim(sub("^.*:\\s*([^:]+)", "\\1", text))
}

# Load dw_inds.csv and clean column names
df_dw_indicator <- read_csv(file.path(inputData, "dw_data.csv")) %>%
  rename_with(~ extract_after_colon(.x) %>% str_to_lower() %>% str_replace_all(" ", "_")) %>%
  mutate(iso3_country_code = str_extract(geographic_area, "^[^:]+")) %>%
  filter(iso3_country_code %in% unicef_iso3_country_code) %>%
  select(iso3_country_code, indicator, time_period, current_age, residence, wealth_quintile, observation_value) %>%
  group_by(iso3_country_code, indicator, current_age, residence, wealth_quintile) %>%
  filter(time_period == max(time_period)) %>%
  ungroup()


# Merge the dataframes
task01_clean_merged_data <- df_dw_indicator %>%
  full_join(indicator_status, by = "iso3_country_code") %>%
  full_join(df_birth_data, by = "iso3_country_code")

# filter data to only include totals
task01_clean_merged_filtered_data <- task01_clean_merged_data %>%
  filter(!is.na(total_birth), !is.na(indicator)) %>%
  filter(residence == '_T: Total' & wealth_quintile == '_T: Total' & current_age == 'Y15T49: 15 to 49 years old')


# Save the merged data to a CSV file
write_csv(task01_clean_merged_filtered_data, file.path(outputData, "task01_master_data.csv"))


