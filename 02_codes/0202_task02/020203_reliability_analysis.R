#-----------------------------------------------------------------------------------------------------------
# Project: P3 Assessment 
# 
# Purpose: This script is to run the summary reliability analysis of task 02
# 
# Date: 16 August 2024
#-----------------------------------------------------------------------------------------------------------

## Task HTML 2E

## load the data 
df_task02_master <- read_csv(file.path(outputData, "task02_master_data.csv"))

# use filtered data
df_task02_filtered <- df_task02_master %>% 
  drop_na(develop) %>%
  drop_na(age_months)

# Specify the items and other parameters
items_2e <- c('ec6_new', 'ec7_new', 'ec8_new', 'ec9_new', 'ec10_new', 
              'ec11_new', 'ec12_new', 'ec13_new', 'ec14_new', 'ec15_new')
domain_2e <- "HTML Readme based index"
cohort_label_2e <- "3 to 4 year olds"

# Prepare data for alpha calculation
alpha_items_2e <- df_task02_filtered[items_2e]

# Calculate Cronbach's Alpha using the psych package
alpha_obj_2e <- psych::alpha(alpha_items_2e)

# Prepare the result as a data frame
alpha_results_2e <- data.frame(
  Domain = domain_2e,
  Cohort = cohort_label_2e,
  Raw_Alpha = round(alpha_obj_2e$total$raw_alpha, 2),
  Std_Alpha = round(alpha_obj_2e$total$std.alpha, 2),
  Average_R = round(alpha_obj_2e$total$average_r, 2),
  Observations = nrow(alpha_items_2e)
)

# Reliability analysis: table 2e
summary_task02_table5 <- kable(alpha_results_2e, caption = "Table: Reliability analysis and number of observations",
                               col.names = c("Domain", "Cohort", "Cronbach's Alpha", "Standardized Alpha", "Average Item Correlation", "Observations"),
                               align = 'c', format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1, bold = TRUE, width = "15em") %>%
  column_spec(3:6, width = "10em", background = "lightgray") %>%
  row_spec(0, bold = TRUE, color = "white", background = "darkblue") %>%
  add_header_above(c(" " = 1, "Reliability Analysis" = 5), background = "darkgray")

#### figure 3 : reliability overall and by age cohort

## load the data 
df_task02_master <- read_csv(file.path(outputData, "task02_master_data.csv"))

# use filtered data
df_task02_filtered <- df_task02_master %>% 
  drop_na(develop) %>%
  drop_na(age_months)  %>%
  mutate(cohort = factor(cut(age_months, 
                             breaks = c(35, 41, 47, 53, 60), 
                             labels = c("36-41", "42-47", "48-53", "54-60"),
                             include.lowest = TRUE),
                         levels = c("36-41", "42-47", "48-53", "54-60")))

# creating subsets of data by each cohort
cohorts <- c("36-41", "42-47", "48-53", "54-60")
cohort_dfs <- lapply(cohorts, function(c) df_task02_filtered %>% filter(cohort == c))

# specifying items per domain as explained by the MICS SFR
domains <- list( lit_num = c("ec6_new", "ec7_new", "ec8_new"),
                 physical = c("ec9_new", "ec10_new"),
                 learn = c("ec11_new", "ec12_new"),
                 social_emotional = c("ec13_new", "ec14_new", "ec15_new"),
                 develop = c("lit_num", "physical", "learn", "socio_em") )

# function to calculate alpha and extract the values
calculate_alpha <- function(df, items, domain, cohort_label) {
  alpha_items <- df[, items]
  alpha_obj <- psych::alpha(alpha_items)
  data.frame(
    Domain = domain,
    Cohort = cohort_label,
    Raw_Alpha = alpha_obj$total$raw_alpha,
    Std_Alpha = alpha_obj$total$std.alpha,
    Average_R = alpha_obj$total$average_r )}


# store results
results_list <- list()

# Loop
for (domain in names(domains)) {
  #all
  items <- domains[[domain]]
  results_list[[paste0(domain, "_all")]] <- calculate_alpha(df_task02_filtered,
                                                            items, domain,
                                                            "36 - 60")
  # cohorts
  for (i in seq_along(cohorts)) {
    cohort_label <- cohorts[i]
    cohort_df <- cohort_dfs[[i]]
    results_list[[paste0(domain, "_", cohort_label)]] <- calculate_alpha(cohort_df,
                                                                         items, domain,
                                                                         cohort_label)
  }
}

final_results_df <- do.call(rbind, results_list)


# Define proper labels for the domains
domain_labels <- c(
  "lit_num" = "Literacy & Numeracy",
  "physical" = "Physical",
  "learn" = "Learning",
  "social_emotional" = "Social & Emotional",
  "develop" = "ECDI"
)

# Define the x position for '36 - 60' cohort
x_position_36_60 <- which(unique(final_results_df$Cohort) == "36 - 60") - 0.5
x_position_36_60_end <- x_position_36_60 + 1

# fig03
task02_fig3 <- ggplot(final_results_df, aes(x = Cohort, y = Raw_Alpha, color = Domain)) +
  geom_point(size = 2.5) +
  facet_wrap(~ Domain, ncol = 1, labeller = labeller(Domain = domain_labels), strip.position = "left") +
  labs(title = "Cronbach's Alpha by Domain and Age Cohort",
       x = "Age (in months)",
       y = "Cronbach's Alpha (Raw_Alpha)",
       caption = "Data source: Authors' calculation using Zimbabwe MICS 2019") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12),
        strip.text = element_text(size = 7),  
        strip.placement = "outside",
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  geom_hline(yintercept = 0.70, linetype = "dashed", color = "red", size = 1) +
  geom_rect(aes(xmin = x_position_36_60, xmax = x_position_36_60_end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.1, inherit.aes = FALSE) +
  coord_fixed(ratio = 0.5)
# cor_matrix <- cor(df_task02_filtered[, domains$physical], use = "complete.obs")
# print(cor_matrix)



