#-----------------------------------------------------------------------------------------------------------
# Project: P3 Assessment 
# 
# Purpose: This script is to preapre the master data for task_02
# 
# Date: 16 August 2024
#-----------------------------------------------------------------------------------------------------------

# !!!  PROFILE AND PACKAGES MUST BE LOADED BEFORE RUNNING ANY OTHER SCRIPT IN THE REPO !!!
# Please run user_profile script or if username is set, run the following code
# source(user_profile.R)
# source(user_packages.R)

# read input file
zwe_mics_2019 <- read_csv(file.path(inputData, "Zimbabwe_children_under5_interview.csv"))

# Recoding variables, creating ECDI domains and responding to HTML task 2.d
zwe_mics_2019 <- zwe_mics_2019 %>% mutate(
  ec6_new = case_when(EC6 == 1 ~ 1, TRUE ~ 0),
  ec7_new = case_when(EC7 == 1 ~ 1, TRUE ~ 0),
  ec8_new = case_when(EC8 == 1 ~ 1, TRUE ~ 0),
  ec9_new = case_when(EC9 == 1 ~ 1, TRUE ~ 0), # item phrased negatively so reverse coding
  ec10_new = case_when(EC10 == 2 ~ 1, TRUE ~ 0),
  ec11_new = case_when(EC11 == 1 ~ 1, TRUE ~ 0),
  ec12_new = case_when(EC12 == 1 ~ 1, TRUE ~ 0),
  ec13_new = case_when(EC13 == 1 ~ 1, TRUE ~ 0),
  ec14_new = case_when(EC14 == 2 ~ 1, TRUE ~ 0), # item phrased negatively so reverse coding
  ec15_new = case_when(EC15 == 2 ~ 1, TRUE ~ 0), # item phrased negatively so reverse coding
  
  # task html 2.d
  mean_across_items = rowMeans(cbind(ec6_new, ec7_new, ec8_new, ec9_new, ec10_new,
                                     ec11_new, ec12_new, ec13_new, ec14_new, ec15_new),
                               na.rm = TRUE),
  
  # coding for ECDI now
  # calculating number of tasks met per domain
  
  # Literacy items
  sum_lit = ec6_new + ec7_new,
  # Numeracy tasks: sum_num is ec7_new as there is only one numeracy item, thereofre not creating another variable
  sum_lit_num = ec6_new + ec7_new + ec8_new,
  sum_physical = ec9_new + ec10_new,
  sum_learn = ec11_new + ec12_new,
  
  # socio-emotional items
  sum_socio_emo = ec13_new + ec14_new + ec15_new,
  
  # variables whether the child achieves domain cut-offs
  lit_num = case_when(sum_lit_num >= 2 ~ 1, TRUE ~ 0), # atleast 2 of 3 met
  physical = case_when(sum_physical >= 1 ~ 1, TRUE ~ 0), # atleast 1 of 2 met
  learn = case_when(sum_learn >= 1 ~ 1, TRUE ~ 0), # atleast 1 of 2 met
  socio_em = case_when(sum_socio_emo >= 2 ~ 1, TRUE ~ 0), # atleast 2 of 3 met
  
  # counting number of domains met by each child
  domains_met = lit_num + physical + learn + socio_em,
  
  one_met = case_when(domains_met == 1 ~ 1, TRUE ~ 0 ),
  two_met = case_when(domains_met == 2 ~ 1, TRUE ~ 0 ),
  three_met = case_when(domains_met == 3 ~ 1, TRUE ~ 0 ),
  four_met = case_when(domains_met == 4 ~ 1, TRUE ~ 0 ),
  
  # ecdi score as calculated by mics zimbabwe 
  
  develop = case_when(domains_met == 3 | domains_met == 4 ~ 1, TRUE ~ 0),
  
  # age in months
  interview_date = ymd(interview_date),
  child_birthday = ymd(child_birthday),
  age_months = round(interval(child_birthday, interview_date) / months(1))
  )

# saving the output
write_csv(zwe_mics_2019, file.path(outputData, "task02_master_data.csv"))
