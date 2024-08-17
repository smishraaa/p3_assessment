#-----------------------------------------------------------------------------------------------------------
# Project: P3 Assessment 
# 
# Purpose: This script is to run the summary statistic analysis
# 
# Date: 16 August 2024
#-----------------------------------------------------------------------------------------------------------

### Part 1 analysis: Summary Statistics

# load the data
df_task02_master <- read_csv(file.path(outputData, "task02_master_data.csv"))

# Summarize total observations and missing values by child's age
summary_with_na <- df_task02_master %>%
  group_by(child_age_years) %>%
  summarise(
    total_observations = n(),
    child_birthday_na = sum(is.na(child_birthday))
  ) %>%
  mutate(
    non_missing_child_birthday = total_observations - child_birthday_na,
    missing_values = ifelse(child_birthday_na > 0, TRUE, FALSE)
  )

# Create an enhanced table
task02_table1 <- summary_with_na %>%
  kable(
    caption = "Summary of Observations by Child's Age with Missing Values",
    col.names = c("Child Age (Years)", "Total Observations in the dataset", "Obs misssing in 'child_birtday'", "Non-missing obs in 'child_birthday'", "Missing Values"),
    align = 'c'
  ) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(0, bold = TRUE) %>%  
  column_spec(3, bold = TRUE, color = "red")  

# 26 observations missing for age_months due to missing birthdate
# dropping these observations


# Task 02: Table 02 is created in markdown


#### Task 02: Table 3, create per task summary and add observations

## load the data 
df_task02_master <- read_csv(file.path(outputData, "task02_master_data.csv"))

# use filtered data
df_task02_filtered <- df_task02_master %>% 
  drop_na(develop) %>%
  drop_na(age_months)

# function to calculate mean by each task
calculate_percentage <- function(df, variable) {
  df %>%
    dplyr::group_by(child_age_years) %>%
    dplyr::summarize(
      percent = round(mean(.data[[variable]] == 1, na.rm = TRUE) * 100, 1),
      .groups = 'drop'
    )
}

# including observations in the table
age_group_counts <- df_task02_filtered %>%
  dplyr::group_by(child_age_years) %>%
  dplyr::summarize(N = n(), .groups = 'drop')


variables <- c('ec6_new', 'ec7_new', 'ec8_new', 'ec9_new', 'ec10_new', 'ec11_new', 'ec12_new', 'ec13_new', 'ec14_new', 'ec15_new')

# summary statistics
summary_stats <- lapply(variables, function(var) {
  calculate_percentage(df_task02_filtered, var)
})

# combing summary state with observations
summary_stats_df <- Reduce(function(x, y) merge(x, y, by = "child_age_years", all = TRUE), summary_stats)
summary_stats_df <- merge(age_group_counts, summary_stats_df, by = "child_age_years")

# Coloumn labels
col_names <- c("Child Age (in years)", "Number of Observations",
               "Identify or name at least ten letters of the alphabet (EC6)", 
               "Read at least four simple, popular words (EC7)",
               "Know the name and recognize the symbol of all numbers from 1 to 10 (EC8)", 
               "Pick up a small object (EC9)", 
               "Not be 'sometimes too sick to play' (EC10)", 
               "Follow simple directions (EC11)", 
               "Follow direction independently (EC12)",
               "Get along with other children (EC13)",
               "Not kick, bite, or hit other children or adults (EC14)", 
               "Not get distracted easily (EC15)")

names(summary_stats_df) <- col_names

#  summary table
summary_stats_table <- summary_stats_df %>%
  kable(caption = "Summary Statistics by Child Age Years", align = 'c', escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(0, bold = TRUE, color = "black", background = "lightgray") %>%
  add_header_above(c(" " = 1, " " = 1, "Percent reported to be able to" = 10), background = "darkgrey")


######### Fig 1: line plot monthly trend domain and overall

## load data and filter out na's following protocol
df_task02_master <- read_csv(file.path(outputData, "task02_master_data.csv"))

# use filtered data
df_task02_filtered <- df_task02_master %>% 
  drop_na(develop) %>%
  drop_na(age_months)


#  6 months age cohort to create facets
df_task02_filtered <- df_task02_filtered %>%
  mutate(cohort = factor(cut(age_months, 
                             breaks = c(35, 41, 47, 53, 60), 
                             labels = c("36-41", "42-47", "48-53", "54-60"),
                             include.lowest = TRUE),
                         levels = c("36-41", "42-47", "48-53", "54-60")))

# check the cohort distribution
# (table(df_task02_filtered$age_months))

# calculating mean per domain, ECDI score and HTML-based index
monthly_summary <- df_task02_filtered %>%
  group_by(cohort, age_months) %>%
  summarise(
    prop_lit_num = mean(lit_num, na.rm = TRUE),
    prop_physical = mean(physical, na.rm = TRUE),
    prop_learn = mean(learn, na.rm = TRUE),
    prop_socio_em = mean(socio_em, na.rm = TRUE),
    prop_develop = mean(develop, na.rm = TRUE),
    prop_mean_across_items = mean(mean_across_items, na.rm = TRUE),
  ) %>%
  pivot_longer(cols = starts_with("prop_"), names_to = "domain", values_to = "proportion")

# defining labels
domain_labels <- c(
  "prop_mean_across_items" = "Score calculated per HTML 2d",
  "prop_develop" = "MICS ECDI Score",
  "prop_lit_num" = "Literacy and Numeracy Domain",
  "prop_physical" = "Physical Domain",
  "prop_socio_em" = "Social-Emotional Domain",
  "prop_learn" = "Learning Domain")

# order labels for correct legend sequence following SFR reporting
monthly_summary$domain <- factor(monthly_summary$domain, levels = names(domain_labels))

# figure 
task02_fig1 <- ggplot() +
  geom_line(data = monthly_summary %>% filter(domain == "prop_develop"),
            aes(x = age_months, y = proportion, color = domain, group = domain),
            linewidth = 1.2) + 
  geom_line(data = monthly_summary %>% filter(domain != "prop_develop"),
            aes(x = age_months, y = proportion, color = domain, group = domain),
            linewidth = 0.4) +  
  geom_point(data = monthly_summary,
             aes(x = age_months, y = proportion, color = domain, group = domain),
             size = 2) +
  labs(title = "Proportion of 3 to 4 year olds developmentally on track, overall, by domain and age in months in Zimbabwe",
       x = "Age in Months", y = "Proportion",
       #color = "The share of children 3 to 4 developmentally on track",
       caption = "Data source: Authors' calculation using Zimbabwe MICS 2019") +
  theme_minimal() +
  scale_color_manual(values = c("cyan", "darkblue", "#b3cde3", "#ccebc5", "#fed9a6", "#8dd3c7"),
                     labels = domain_labels) +
  scale_x_continuous(breaks = seq(min(monthly_summary$age_months), max(monthly_summary$age_months), by = 2)) +  # Adjust x-axis breaks
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 1, by = 0.25)) + 
  guides(color = guide_legend(nrow = 2)) +  
  theme(
    legend.position = "bottom",  
    plot.title = element_text(hjust = 0.5, size = 10),
    panel.grid.major.y = element_line(color = "black", linewidth = 0.5)) + 
  facet_wrap(~ cohort, scales = "free_x", nrow = 1)


####### Table 4
## load data and filter out na's following protocol
df_task02_master <- read_csv(file.path(outputData, "task02_master_data.csv"))

# use filtered data
df_task02_filtered <- df_task02_master %>% 
  drop_na(develop) %>%
  drop_na(age_months)

# Run OLS regression of mean_across_items on age_months
ols_model <- lm(mean_across_items ~ age_months, data = df_task02_filtered)

# Tidy the results to get a clean summary
tidy_results <- tidy(ols_model)

# Extract only the coefficient for age_months
age_coef <- tidy_results %>%
  filter(term == "age_months")

# Add R squared and number of observations
model_summary <- glance(ols_model)
summary_table <- age_coef %>%
  mutate(R_squared = model_summary$r.squared,
         Observations = model_summary$nobs)  # Using 'nobs' to get number of observations


task02table4 <- stargazer(ols_model, type = "html", 
                    title = "OLS Regression Results", 
                    align = TRUE,
                    single.row = TRUE,
                    header = FALSE,
                    initial.zero = FALSE,
                    no.space = TRUE,
                    digits = 2,
                    dep.var.labels.include = FALSE,
                    covariate.labels = c("Age in Months"),
                    omit.stat = c("LL", "ser", "f", "aic", "bic")
)
                    #out = "/Users/granth/Documents/Projects/sakshi/alt/DATA/table.html")  # Specify path if saving output

      
      
      
####### figure 2: Sankey diagram linking domains with number of targets met

## load data and filter out na's following protocol
df_task02_master <- read_csv(file.path(outputData, "task02_master_data.csv"))

# use filtered data
df_task02_filtered <- df_task02_master %>% 
        drop_na(develop) %>%
        drop_na(age_months)
      
# Define nodes
nodes <- data.frame(name = c("On track: Literacy/Numeracy domain", 
                             "On track: Physical domain", "On track: Learning domain",
                             "On track: Socio-Emotional domain","On track: One domain", 
                             "On track: Two domains", "On track: Three domains", 
                             "On track: Four domains"))

# Define links between nodes
links <- data.frame(
  source = c(0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
  target = c(4, 5, 6, 7, 4, 5, 6, 7, 4, 5, 6, 7, 4, 5, 6, 7),
  value = c(
    sum(df_task02_filtered$one_met[df_task02_filtered$lit_num == 1]),
    sum(df_task02_filtered$two_met[df_task02_filtered$lit_num == 1]),
    sum(df_task02_filtered$three_met[df_task02_filtered$lit_num == 1]),
    sum(df_task02_filtered$four_met[df_task02_filtered$lit_num == 1]),
    sum(df_task02_filtered$one_met[df_task02_filtered$physical == 1]),
    sum(df_task02_filtered$two_met[df_task02_filtered$physical == 1]),
    sum(df_task02_filtered$three_met[df_task02_filtered$physical == 1]),
    sum(df_task02_filtered$four_met[df_task02_filtered$physical == 1]),
    sum(df_task02_filtered$one_met[df_task02_filtered$learn == 1]),
    sum(df_task02_filtered$two_met[df_task02_filtered$learn == 1]),
    sum(df_task02_filtered$three_met[df_task02_filtered$learn == 1]),
    sum(df_task02_filtered$four_met[df_task02_filtered$learn == 1]),
    sum(df_task02_filtered$one_met[df_task02_filtered$socio_em == 1]),
    sum(df_task02_filtered$two_met[df_task02_filtered$socio_em == 1]),
    sum(df_task02_filtered$three_met[df_task02_filtered$socio_em == 1]),
    sum(df_task02_filtered$four_met[df_task02_filtered$socio_em == 1])
  )
)

# adding group for each connection between domains and target met
links$group <- as.factor(c("type_1","type_2","type_3","type_4", "type_5", "type_6",
                           "type_7", "type_8", "type_9","type_10","type_11","type_12",
                           "type_13", "type_14","type_15", "type_16"))

# creating group for nodes to ensure they are assigned the same color
nodes$group <- as.factor(c("my_unique_group"))


# Assigning color to connections and nodes
my_color <- 'd3.scaleOrdinal() .domain(["type_1","type_2","type_3","type_4", "type_5", "type_6",
                           "type_7", "type_8", "type_9","type_10","type_11","type_12",
                           "type_13", "type_14","type_15", "type_16", "my_unique_group"]) .range(["#f46d43",
                           "#f46d43", "#f46d43","#f46d43","#fdae61","#fdae61","#fdae61",
                           "#fdae61","#fee08b","#fee08b","#fee08b","#fee08b","#e6f598",
                           "#e6f598","#e6f598","#e6f598", "#d9f0d3"])'

# creating the sankey diagram
task02_fig02 <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", 
                              Value = "value", NodeID = "name", 
                              colourScale=my_color, LinkGroup="group", NodeGroup="group")