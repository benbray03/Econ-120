rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(patchwork)

PERMID <- "7154990"
PERMID <- as.numeric(gsub("\\D", "", PERMID))
set.seed(PERMID)

data_description <- read_csv("data_description.csv")

#Part 1
cpi_data <- read_csv("CPI_U_minneapolis_fed.csv") %>%
  select(1:2)

education_data <- read_csv('education_data.csv') %>%
  rename('year' = 'YEAR') %>%
  rename('school_id' = 'UNITID') %>%
  rename('school_name' = 'INSTNM') %>%
  rename('state_id' = 'STABBR') %>%
  rename('predominant_degree' = 'PREDDEG') %>%
  rename('institution_type' = 'CONTROL') %>%
  rename('median_debt_low_income' = 'LO_INC_DEBT_MDN') %>%
  rename('median_debt_med_income' = 'MD_INC_DEBT_MDN') %>%
  rename('median_debt_high_income' = 'HI_INC_DEBT_MDN') %>%
  rename('default_rate' = 'CDR3') %>%
  rename('avg_family_income' = 'FAMINC') %>%
  mutate(
    school_name = tolower(school_name),
    across(c('median_debt_low_income', 'median_debt_med_income', 'median_debt_high_income','default_rate', 'avg_family_income'), as.numeric)
  )

education_data_filled <- education_data %>%
  group_by(state_id, year) %>%
  mutate(
    median_debt_low_income = if_else(
      is.na(median_debt_low_income),
      mean(median_debt_low_income, na.rm = TRUE),
      median_debt_low_income
    ),
    median_debt_med_income = if_else(
      is.na(median_debt_med_income),
      mean(median_debt_med_income, na.rm = TRUE),
      median_debt_med_income
    ),
    median_debt_high_income = if_else(
      is.na(median_debt_high_income),
      mean(median_debt_high_income, na.rm = TRUE),
      median_debt_high_income
    ),
    default_rate = if_else(
      is.na(default_rate),
      mean(default_rate, na.rm = TRUE),
      default_rate
    ),
    avg_family_income = if_else(
      is.na(avg_family_income),
      mean(avg_family_income, na.rm = TRUE),
      avg_family_income
    )
  )


missing_data_low <- education_data_filled %>% filter(is.nan(median_debt_low_income))
missing_data_med <- education_data_filled %>% filter(is.nan(median_debt_med_income))
missing_data_high <- education_data_filled %>% filter(is.nan(median_debt_high_income))


train_data_low <- education_data_filled %>% filter(!is.nan(median_debt_low_income))
train_data_med <- education_data_filled %>% filter(!is.nan(median_debt_med_income))
train_data_high <- education_data_filled %>% filter(!is.nan(median_debt_high_income))


lm_model_low <- lm(median_debt_low_income ~ median_debt_med_income + median_debt_high_income, data = train_data_low)
lm_model_med <- lm(median_debt_med_income ~ median_debt_low_income + median_debt_high_income, data = train_data_med)
lm_model_high <- lm(median_debt_high_income ~ median_debt_low_income + median_debt_med_income, data = train_data_high)


predicted_values_low <- predict(lm_model_low, newdata = missing_data_low)
predicted_values_med <- predict(lm_model_med, newdata = missing_data_med)
predicted_values_high <- predict(lm_model_high, newdata = missing_data_high)


education_data_filled$median_debt_low_income[is.nan(education_data_filled$median_debt_low_income)] <- predicted_values_low
education_data_filled$median_debt_med_income[is.nan(education_data_filled$median_debt_med_income)] <- predicted_values_med
education_data_filled$median_debt_high_income[is.nan(education_data_filled$median_debt_high_income)] <- predicted_values_high


missing_data_default <- education_data_filled %>% filter(is.nan(default_rate))
missing_data_income <- education_data_filled %>% filter(is.nan(avg_family_income))

for (year in unique(education_data_filled$year)) {
  year_data <- education_data_filled %>% filter(year == year)
  mean_default_rate <- mean(year_data$default_rate, na.rm = TRUE)
  education_data_filled$default_rate[is.nan(education_data_filled$default_rate) & education_data_filled$year == year] <- mean_default_rate
}

for (year in unique(education_data_filled$year)) {
  year_data <- education_data_filled %>% filter(year == year)
  median_avg_family_income <- median(year_data$avg_family_income, na.rm = TRUE)
  education_data_filled$avg_family_income[is.nan(education_data_filled$avg_family_income) & education_data_filled$year == year] <- median_avg_family_income
}

for (year in unique(education_data_filled$year)) {
  year_data <- education_data_filled %>% filter(year == year)
  mean_low_income <- mean(year_data$median_debt_low_income, na.rm = TRUE)
  education_data_filled[education_data_filled$year == year & is.na(education_data_filled$median_debt_low_income), "median_debt_low_income"] <- mean_low_income
}

for (year in unique(education_data_filled$year)) {
  year_data <- education_data_filled %>% filter(year == year)
  mean_med_income <- mean(year_data$median_debt_med_income, na.rm = TRUE)
  education_data_filled[education_data_filled$year == year & is.na(education_data_filled$median_debt_med_income), "median_debt_med_income"] <- mean_med_income
}

for (year in unique(education_data_filled$year)) {
  year_data <- education_data_filled %>% filter(year == year)
  mean_high_income <- mean(year_data$median_debt_high_income, na.rm = TRUE)
  education_data_filled[education_data_filled$year == year & is.na(education_data_filled$median_debt_high_income), "median_debt_high_income"] <- mean_high_income
}



education_data_clean <- education_data_filled %>%
  mutate(
    institution_type = ifelse(institution_type == 1, 'public', 'private')
  )

education_data_BA1 <- education_data_clean %>%
  filter(predominant_degree == 3)

education_data_BA <- education_data_BA1 %>%
  left_join(cpi_data, by = "year") %>%
  mutate(
    real_debt_low_income = median_debt_low_income * (251.1 / CPI),
    real_debt_med_income = median_debt_med_income * (251.1 / CPI),
    real_debt_high_income = median_debt_high_income * (251.1 / CPI),
    real_family_income = avg_family_income * (251.1 / CPI)
  ) %>%
  select(-c('median_debt_low_income', 'median_debt_med_income', 'median_debt_high_income', 'avg_family_income', 'CPI'))

#Part 2
cost_data1 <- read_csv("cost_data.csv") %>%
  select(c('UNITID', 'INSTNM', 'YEAR', 'NPT41_PUB', 'NPT43_PUB', 'NPT45_PUB', 'NPT41_PRIV', 'NPT43_PRIV', 'NPT45_PRIV'))

cost_data2 <- cost_data1 %>%
  rename('year' = 'YEAR') %>%
  rename('school_id' = 'UNITID') %>%
  rename('school_name' = 'INSTNM') %>%
  rename('mean_cost_low_income_public' = 'NPT41_PUB') %>%
  rename('mean_cost_med_income_public' = 'NPT43_PUB') %>%
  rename('mean_cost_high_income_public' = 'NPT45_PUB') %>%
  rename('mean_cost_low_income_private' = 'NPT41_PRIV') %>%
  rename('mean_cost_med_income_private' = 'NPT43_PRIV') %>%
  rename('mean_cost_high_income_private' = 'NPT45_PRIV') %>%
  mutate(
    school_name = tolower(school_name),
    across(c('mean_cost_low_income_public', 'mean_cost_med_income_public', 'mean_cost_high_income_public', 'mean_cost_low_income_private', 'mean_cost_med_income_private', 'mean_cost_high_income_private'), as.numeric)
  )

for (year in unique(cost_data2$year)) {
  year_data <- cost_data2 %>% filter(year == year)
  mean_cost_low_income_public <- mean(year_data$mean_cost_low_income_public, na.rm = TRUE)
  cost_data2$mean_cost_low_income_public[is.na(cost_data2$mean_cost_low_income_public) & cost_data2$year == year] <- mean_cost_low_income_public
}

# Filling missing values for mean_cost_med_income_public
for (year in unique(cost_data2$year)) {
  year_data <- cost_data2 %>% filter(year == year)
  mean_cost_med_income_public <- mean(year_data$mean_cost_med_income_public, na.rm = TRUE)
  cost_data2$mean_cost_med_income_public[is.na(cost_data2$mean_cost_med_income_public) & cost_data2$year == year] <- mean_cost_med_income_public
}

# Filling missing values for mean_cost_high_income_public
for (year in unique(cost_data2$year)) {
  year_data <- cost_data2 %>% filter(year == year)
  mean_cost_high_income_public <- mean(year_data$mean_cost_high_income_public, na.rm = TRUE)
  cost_data2$mean_cost_high_income_public[is.na(cost_data2$mean_cost_high_income_public) & cost_data2$year == year] <- mean_cost_high_income_public
}

# Filling missing values for mean_cost_low_income_private
for (year in unique(cost_data2$year)) {
  year_data <- cost_data2 %>% filter(year == year)
  mean_cost_low_income_private <- mean(year_data$mean_cost_low_income_private, na.rm = TRUE)
  cost_data2$mean_cost_low_income_private[is.na(cost_data2$mean_cost_low_income_private) & cost_data2$year == year] <- mean_cost_low_income_private
}

# Filling missing values for mean_cost_med_income_private
for (year in unique(cost_data2$year)) {
  year_data <- cost_data2 %>% filter(year == year)
  mean_cost_med_income_private <- mean(year_data$mean_cost_med_income_private, na.rm = TRUE)
  cost_data2$mean_cost_med_income_private[is.na(cost_data2$mean_cost_med_income_private) & cost_data2$year == year] <- mean_cost_med_income_private
}

# Filling missing values for mean_cost_high_income_private
for (year in unique(cost_data2$year)) {
  year_data <- cost_data2 %>% filter(year == year)
  mean_cost_high_income_private <- mean(year_data$mean_cost_high_income_private, na.rm = TRUE)
  cost_data2$mean_cost_high_income_private[is.na(cost_data2$mean_cost_high_income_private) & cost_data2$year == year] <- mean_cost_high_income_private
}

cost_data3 <- cost_data2 %>%
  mutate(
    mean_cost_low_income = ifelse(is.na(mean_cost_low_income_public), mean_cost_low_income_private, mean_cost_low_income_public),
    mean_cost_med_income = ifelse(is.na(mean_cost_med_income_public), mean_cost_med_income_private, mean_cost_med_income_public),
    mean_cost_high_income = ifelse(is.na(mean_cost_high_income_public), mean_cost_high_income_private, mean_cost_high_income_public)
  )%>%
  select(-c('mean_cost_low_income_public', 'mean_cost_low_income_private', 'mean_cost_med_income_public', 'mean_cost_med_income_private', 'mean_cost_high_income_public', 'mean_cost_high_income_private'))

cost_data4 <- cost_data3 %>%
  left_join(cpi_data, by = "year") %>%
  mutate(
    real_cost_low_income = mean_cost_low_income*(251.1/CPI),
    real_cost_med_income = mean_cost_med_income*(251.1/CPI),
    real_cost_high_income = mean_cost_high_income*(251.1/CPI),
  )

cost_data <- cost_data4 %>%
  select(-c('mean_cost_low_income', 'mean_cost_med_income', 'mean_cost_high_income', 'CPI'))


#part 3
education_data_BA_cost <- education_data_BA %>%
  left_join(cost_data, by = c('year', 'school_id')) %>%
  select(-'school_name.y')

debt_cost_sumstat_year <- education_data_BA_cost %>%
  group_by(year, institution_type) %>%
  summarize(mean_debt_for_low_income = mean(real_debt_low_income, na.rm = T),
            mean_debt_for_median_income = mean(real_debt_med_income, na.rm = T),
            mean_debt_for_high_income = mean(real_debt_high_income, na.rm = T),
            mean_cost_for_low_income = mean(real_cost_low_income, na.rm = T),
            mean_cost_for_median_income = mean(real_cost_med_income, na.rm = T),
            mean_cost_for_high_income = mean(real_cost_high_income, na.rm = T)
  ) %>%
  ungroup()

debt_table <- debt_cost_sumstat_year %>%
  select(year, institution_type, starts_with('mean_debt')) %>%
  pivot_longer(cols = starts_with("mean_debt"),
               names_to = "income_category",
               values_to = "debt",
               names_prefix = "mean_debt_for_") %>%
  mutate(income_category = str_replace(income_category, '_', ' '))

cost_table <- debt_cost_sumstat_year %>%
  select(year, institution_type, starts_with('mean_cost')) %>%
  pivot_longer(cols = starts_with("mean_cost"),
               names_to = "income_category",
               values_to = "cost",
               names_prefix = "mean_cost_for_") %>%
  mutate(income_category = str_replace(income_category, '_', ' '))

debt_cost_data_by_year <- inner_join(debt_table, cost_table, by = c('year', 'institution_type', 'income_category'))
  
  
debt_sumstat_school_type <- education_data_BA_cost %>%
  group_by(institution_type) %>%
  summarize(
    mean_debt_for_low_income = mean(real_debt_low_income, na.rm = TRUE),
    mean_debt_for_median_income = mean(real_debt_med_income, na.rm = TRUE),
    mean_debt_for_high_income = mean(real_debt_high_income, na.rm = TRUE),
    mean_family_income = mean(real_family_income, na.rm = TRUE)
  )

debt_sumstat_year <- education_data_BA_cost %>%
  group_by(year) %>%
  summarize(
    mean_debt_for_low_income = mean(real_debt_low_income, na.rm = TRUE),
    mean_debt_for_median_income = mean(real_debt_med_income, na.rm = TRUE),
    mean_debt_for_high_income = mean(real_debt_high_income, na.rm = TRUE),
    mean_family_income = mean(real_family_income, na.rm = TRUE)
  )

cost_sumstat_school_type <- education_data_BA_cost %>%
  group_by(institution_type) %>%
  summarize(
    mean_cost_for_low_income = mean(real_cost_low_income, na.rm = TRUE),
    mean_cost_for_median_income = mean(real_cost_med_income, na.rm = TRUE),
    mean_cost_for_high_income = mean(real_cost_high_income, na.rm = TRUE)
  )

cost_sumstat_year <- education_data_BA_cost %>%
  group_by(year) %>%
  summarize(
    mean_cost_for_low_income = mean(real_cost_low_income, na.rm = TRUE),
    mean_cost_for_median_income = mean(real_cost_med_income, na.rm = TRUE),
    mean_cost_for_high_income = mean(real_cost_high_income, na.rm = TRUE)
  )

#Plot 1
debt_sumstat_year_long <- pivot_longer(debt_sumstat_year, 
                                       cols = starts_with("mean_debt_for"), 
                                       names_to = "income_category", 
                                       values_to = "debt")

debt_sumstat_year_filtered <- debt_sumstat_year_long %>%
  filter(income_category %in% c("mean_debt_for_low_income", "mean_debt_for_median_income", "mean_debt_for_high_income"))

custom_labels <- c("mean_debt_for_low_income" = "Low Income",
                   "mean_debt_for_median_income" = "Median Income",
                   "mean_debt_for_high_income" = "High Income")

ggplot(debt_sumstat_year_filtered, aes(x = as.integer(year), y = debt, group = income_category, color = income_category)) +
  geom_line() +
  labs(
       x = "Year",
       y = "Debt") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(name = "Income Category", 
                     values = c("mean_debt_for_low_income" = "blue", "mean_debt_for_median_income" = "green", "mean_debt_for_high_income" = "red"), 
                     labels = custom_labels) +
  theme_minimal() + 
  plot_annotation(
    title = "Debts Over Each Year by Income Category",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

#Plot 2
private_schools_data <- education_data_BA_cost %>%
  filter(institution_type == "private")

private_debt_sumstat_year <- private_schools_data %>%
  group_by(year) %>%
  summarize(
    mean_debt_for_low_income = mean(real_debt_low_income, na.rm = TRUE),
    mean_debt_for_median_income = mean(real_debt_med_income, na.rm = TRUE),
    mean_debt_for_high_income = mean(real_debt_high_income, na.rm = TRUE),
    mean_family_income = mean(real_family_income, na.rm = TRUE)
  )

priv_sumstat_year_long <- pivot_longer(private_debt_sumstat_year, 
                                       cols = starts_with("mean_debt_for"), 
                                       names_to = "income_category", 
                                       values_to = "debt")

private_debt_sumstat_year_filtered <- priv_sumstat_year_long %>%
  filter(income_category %in% c("mean_debt_for_low_income", "mean_debt_for_median_income", "mean_debt_for_high_income"))

plot_private_schools <- ggplot(private_debt_sumstat_year_filtered, aes(x = as.integer(year), y = debt, group = income_category, color = income_category)) +
  geom_line() +
  labs(title = "Private Schools",
       x = "Year",
       y = "Mean Debt") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(name = "Income Category", 
                     values = c("mean_debt_for_low_income" = "blue", "mean_debt_for_median_income" = "green", "mean_debt_for_high_income" = "red"), 
                     labels = custom_labels) +
  theme_minimal() +
  theme(legend.position = 'none')

public_schools_data <- education_data_BA_cost %>%
  filter(institution_type == "public")

public_debt_sumstat_year <- public_schools_data %>%
  group_by(year) %>%
  summarize(
    mean_debt_for_low_income = mean(real_debt_low_income, na.rm = TRUE),
    mean_debt_for_median_income = mean(real_debt_med_income, na.rm = TRUE),
    mean_debt_for_high_income = mean(real_debt_high_income, na.rm = TRUE),
    mean_family_income = mean(real_family_income, na.rm = TRUE)
  )

public_sumstat_year_long <- pivot_longer(public_debt_sumstat_year, 
                                       cols = starts_with("mean_debt_for"), 
                                       names_to = "income_category", 
                                       values_to = "debt")

public_debt_sumstat_year_filtered <- public_sumstat_year_long %>%
  filter(income_category %in% c("mean_debt_for_low_income", "mean_debt_for_median_income", "mean_debt_for_high_income"))

plot_public_schools <- ggplot(public_debt_sumstat_year_filtered, aes(x = as.integer(year), y = debt, group = income_category, color = income_category)) +
  geom_line() +
  labs(title = "Public Schools",
       x = "Year",
       y = "Mean Debt") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(name = "Income Category", 
                     values = c("mean_debt_for_low_income" = "blue", "mean_debt_for_median_income" = "green", "mean_debt_for_high_income" = "red"), 
                     labels = custom_labels) +
  theme_minimal()

plot_private_schools + plot_public_schools +
  plot_annotation(
    title = "Mean Debt in Private vs. Public Schools Over Time",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )


#Plot 3
private_schools_data_CA <- education_data_BA_cost %>%
  filter(institution_type == "private", state_id == 'CA')

privateCA_debt_sumstat_year <- private_schools_data_CA %>%
  group_by(year) %>%
  summarize(
    mean_debt_for_low_income = mean(real_debt_low_income, na.rm = TRUE),
    mean_debt_for_median_income = mean(real_debt_med_income, na.rm = TRUE),
    mean_debt_for_high_income = mean(real_debt_high_income, na.rm = TRUE),
    mean_family_income = mean(real_family_income, na.rm = TRUE)
  )

privCA_sumstat_year_long <- pivot_longer(privateCA_debt_sumstat_year, 
                                       cols = starts_with("mean_debt_for"), 
                                       names_to = "income_category", 
                                       values_to = "debt")

private_CA_debt_sumstat_year_filtered <- privCA_sumstat_year_long %>%
  filter(income_category %in% c("mean_debt_for_low_income", "mean_debt_for_median_income", "mean_debt_for_high_income"))

plot_private_CA_schools <- ggplot(private_CA_debt_sumstat_year_filtered, aes(x = as.integer(year), y = debt, group = income_category, color = income_category)) +
  geom_line() +
  labs(title = "Private Schools in California",
       x = "Year",
       y = "Mean Debt") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(name = "Income Category", 
                     values = c("mean_debt_for_low_income" = "blue", "mean_debt_for_median_income" = "green", "mean_debt_for_high_income" = "red"), 
                     labels = custom_labels) +
  theme_minimal() +
  theme(legend.position = 'none')

private_schools_data_notCA <- education_data_BA_cost %>%
  filter(institution_type == "private", state_id != 'CA')

privatenotCA_debt_sumstat_year <- private_schools_data_notCA %>%
  group_by(year) %>%
  summarize(
    mean_debt_for_low_income = mean(real_debt_low_income, na.rm = TRUE),
    mean_debt_for_median_income = mean(real_debt_med_income, na.rm = TRUE),
    mean_debt_for_high_income = mean(real_debt_high_income, na.rm = TRUE),
    mean_family_income = mean(real_family_income, na.rm = TRUE)
  )

privnotCA_sumstat_year_long <- pivot_longer(privatenotCA_debt_sumstat_year, 
                                         cols = starts_with("mean_debt_for"), 
                                         names_to = "income_category", 
                                         values_to = "debt")

private_not_CA_debt_sumstat_year_filtered <- privnotCA_sumstat_year_long %>%
  filter(income_category %in% c("mean_debt_for_low_income", "mean_debt_for_median_income", "mean_debt_for_high_income"))

plot_private_notCA_schools <- ggplot(private_not_CA_debt_sumstat_year_filtered, aes(x = as.integer(year), y = debt, group = income_category, color = income_category)) +
  geom_line() +
  labs(title = "Private Schools Outside of California",
       x = "Year",
       y = "Mean Debt") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(name = "Income Category", 
                     values = c("mean_debt_for_low_income" = "blue", "mean_debt_for_median_income" = "green", "mean_debt_for_high_income" = "red"), 
                     labels = custom_labels) +
  theme_minimal() +
  theme(legend.position = 'none')

public_schools_data_CA <- education_data_BA_cost %>%
  filter(institution_type == "public", state_id == 'CA')

publicCA_debt_sumstat_year <- public_schools_data_CA %>%
  group_by(year) %>%
  summarize(
    mean_debt_for_low_income = mean(real_debt_low_income, na.rm = TRUE),
    mean_debt_for_median_income = mean(real_debt_med_income, na.rm = TRUE),
    mean_debt_for_high_income = mean(real_debt_high_income, na.rm = TRUE),
    mean_family_income = mean(real_family_income, na.rm = TRUE)
  )

publicCA_sumstat_year_long <- pivot_longer(publicCA_debt_sumstat_year, 
                                         cols = starts_with("mean_debt_for"), 
                                         names_to = "income_category", 
                                         values_to = "debt")

public_CA_debt_sumstat_year_filtered <- publicCA_sumstat_year_long %>%
  filter(income_category %in% c("mean_debt_for_low_income", "mean_debt_for_median_income", "mean_debt_for_high_income"))


plot_public_CA_schools <- ggplot(public_CA_debt_sumstat_year_filtered, aes(x = as.integer(year), y = debt, group = income_category, color = income_category)) +
  geom_line() +
  labs(title = "Public Schools in California",
       x = "Year",
       y = "Mean Debt") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(name = "Income Category", 
                     values = c("mean_debt_for_low_income" = "blue", "mean_debt_for_median_income" = "green", "mean_debt_for_high_income" = "red"), 
                     labels = custom_labels) +
  theme_minimal() +
  theme(legend.position = 'none')

public_schools_data_notCA <- education_data_BA_cost %>%
  filter(institution_type == "public", state_id != 'CA')

publicnotCA_debt_sumstat_year <- public_schools_data_notCA %>%
  group_by(year) %>%
  summarize(
    mean_debt_for_low_income = mean(real_debt_low_income, na.rm = TRUE),
    mean_debt_for_median_income = mean(real_debt_med_income, na.rm = TRUE),
    mean_debt_for_high_income = mean(real_debt_high_income, na.rm = TRUE),
    mean_family_income = mean(real_family_income, na.rm = TRUE)
  )

publicnotCA_sumstat_year_long <- pivot_longer(publicnotCA_debt_sumstat_year, 
                                            cols = starts_with("mean_debt_for"), 
                                            names_to = "income_category", 
                                            values_to = "debt")

public_not_CA_debt_sumstat_year_filtered <- publicnotCA_sumstat_year_long %>%
  filter(income_category %in% c("mean_debt_for_low_income", "mean_debt_for_median_income", "mean_debt_for_high_income"))

plot_public_notCA_schools <- ggplot(public_not_CA_debt_sumstat_year_filtered, aes(x = as.integer(year), y = debt, group = income_category, color = income_category)) +
  geom_line() +
  labs(title = "Public Schools Outside of California",
       x = "Year",
       y = "Mean Debt") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(name = "Income Category", 
                     values = c("mean_debt_for_low_income" = "blue", "mean_debt_for_median_income" = "green", "mean_debt_for_high_income" = "red"), 
                     labels = custom_labels) +
  theme_minimal()

plot_private_notCA_schools + plot_private_CA_schools + plot_public_notCA_schools + plot_public_CA_schools +
  plot_annotation(
    title = "Mean Debt in Private vs. Public Schools Over Time in California and All other States",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )


#family income over time
family_income_data <- education_data_BA_cost %>%
  filter(state_id == 'CA')

publicCA_debt_sumstat_year <- family_income_data %>%
  group_by(year) %>%
  summarize(
    mean_family_income = mean(real_family_income, na.rm = TRUE))

family_income_sumstat_year_long <- pivot_longer(publicCA_debt_sumstat_year, 
                                              cols = starts_with("mean_family"), 
                                              names_to = "income_category", 
                                              values_to = "debt")

family_income_sumstat_year_filtered <- family_income_sumstat_year_long %>%
  filter(income_category %in% "mean_family_income")

ggplot(family_income_sumstat_year_filtered, aes(x = as.integer(year), y = debt)) +
  geom_line() +
  labs(x = "Year", y = "Mean Family Income") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(as.integer(family_income_sumstat_year_filtered$year)), max(as.integer(family_income_sumstat_year_filtered$year)), by = 1)) +
  plot_annotation(
    title = "Mean Family Income Over Time",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )


#default rate

default_rate_data <- education_data_BA_cost %>%
  filter(state_id == 'CA')

default_rate_sumstat_year <- default_rate_data %>%
  group_by(year) %>%
  summarize(
    mean_default_rate = mean(default_rate, na.rm = TRUE))

plot_default_rate <- ggplot(default_rate_sumstat_year, aes(x = as.integer(year), y = mean_default_rate)) + 
  geom_line() +
  labs(
    x = "Year",
    y = "Default Rate") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal() +
  plot_annotation(
    title = "Mean Default Rate Over Time",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

# Out of Pocket Costs
mean_costs_by_year <- cost_data %>%
  group_by(year) %>%
  summarize(
    mean_cost_low_income = mean(real_cost_low_income, na.rm = TRUE),
    mean_cost_med_income = mean(real_cost_med_income, na.rm = TRUE),
    mean_cost_high_income = mean(real_cost_high_income, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(mean_cost_low_income, mean_cost_med_income, mean_cost_high_income),
    names_to = "Income Category",
    values_to = "Mean Cost"
  )

plot_mean_costs <- ggplot(mean_costs_by_year, aes(x = as.integer(year), y = `Mean Cost`, color = `Income Category`, group = `Income Category`)) +
  geom_line() +
  labs(
    title = "Mean Costs Over Time for Different Income Categories",
    x = "Year",
    y = "Mean Cost"
  ) +
  scale_color_manual(
    name = "Income Category",
    values = c(
      mean_cost_low_income = "blue",
      mean_cost_med_income = "green",
      mean_cost_high_income = "red"
    ),
    labels = c("High Income", "Low Income", "Medium Income")
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal()

plot_default_rate + plot_mean_costs + plot_layout(ncol = 1)

costs_2020 <- mean_costs_by_year %>%
  filter(year == 2020)

# Create a bar plot for 2020 data
ggplot(costs_2020, aes(x = `Income Category`, y = `Mean Cost`, fill = `Income Category`)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Mean Costs in 2020 for Different Income Categories",
    x = NULL,  # Remove x-axis label
    y = "Mean Cost"
  ) +
  scale_fill_manual(
    name = "Income Category",
    values = c(
      mean_cost_low_income = "lightblue",
      mean_cost_med_income = "darkgreen",
      mean_cost_high_income = "pink"
    ),
    labels = c("High Income", "Low Income", "Medium Income")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  ) +
  scale_x_discrete(labels = c("mean_cost_low_income" = "Low Income", 
                              "mean_cost_med_income" = "Medium Income", 
                              "mean_cost_high_income" = "High Income")) +
  guides(fill = FALSE)

#Part 2
graduate_income <- read_csv("graduates_income_2018.csv")%>%
  rename('year' = 'YEAR') %>%
  rename('school_id' = 'UNITID') %>%
  rename('school_name' = 'INSTNM') %>%
  rename('med_earning_low_6' = 'MD_EARN_WNE_INC1_P6') %>%
  rename('med_earning_low_8' = 'MD_EARN_WNE_INC1_P8') %>%
  rename('med_earning_low_10' = 'MD_EARN_WNE_INC1_P10') %>%
  rename('med_earning_med_6' = 'MD_EARN_WNE_INC2_P6') %>%
  rename('med_earning_med_8' = 'MD_EARN_WNE_INC2_P8') %>%
  rename('med_earning_med_10' = 'MD_EARN_WNE_INC2_P10') %>%
  rename('med_earning_high_6' = 'MD_EARN_WNE_INC3_P6') %>%
  rename('med_earning_high_8' = 'MD_EARN_WNE_INC3_P8') %>%
  rename('med_earning_high_10' = 'MD_EARN_WNE_INC3_P10')

graduate_income <- graduate_income %>%
  mutate(across(starts_with("med_earning_"), ~ ifelse(. == "NULL", NA, as.numeric(.))))

# Loop through each column with the pattern 'med_earning_*'
for (col in grep("^med_earning_", names(graduate_income), value = TRUE)) {
  for (year in unique(graduate_income$year)) {
    year_data <- graduate_income %>% filter(year == year)
    mean_earning <- mean(year_data[[col]], na.rm = TRUE)
    graduate_income[[col]][graduate_income[[col]] %in% c(NA, NaN)] <- mean_earning
  }
}

reshaped_data <- graduate_income %>%
  select(year, starts_with("med_earning_")) %>%
  pivot_longer(cols = -year, names_to = "Income_Tier", values_to = "Median_Earnings") %>%
  mutate(Years_After_College = case_when(
    grepl("6", Income_Tier) ~ '6',
    grepl("8", Income_Tier) ~ '8',
    grepl("10", Income_Tier) ~ '10',
    TRUE ~ NA_character_
  ))

filtered_reshaped_data <- reshaped_data %>%
  filter(!is.na(Years_After_College)) %>%
  group_by(Years_After_College, Income_Tier) %>%
  summarize(mean_median_earnings = mean(Median_Earnings, na.rm = TRUE)) %>%
  mutate(Income_Category = case_when(
    grepl("low", Income_Tier) ~ "Low",
    grepl("high", Income_Tier) ~ "High",
    grepl("med", Income_Tier) ~ "Medium",
    TRUE ~ "Other"
  )) %>%
  filter(Income_Category %in% income_categories)

# Ensure the plot code is using the right column names
ggplot(filtered_reshaped_data, aes(x = as.numeric(Years_After_College), y = mean_median_earnings, color = Income_Category)) +
  geom_line() +
  labs(
    x = "Years After College",
    y = "Mean Median Earnings",
    color = "Income Category"
  ) +
  scale_x_continuous(breaks = c(6, 8, 10)) +
  theme_minimal() +
  ggtitle("Mean Median Earnings Over Time by Income Category")




ggplot(reshaped_data, aes(x = Years_After_College, y = mean_median_earnings, color = Income_Tier, group = Income_Tier)) +
  geom_line() +
  labs(
    title = "Mean Median Earnings Over Years After College",
    x = "Years After College",
    y = "Mean Median Earnings"
  ) +
  scale_x_continuous(breaks = c(6, 7, 8, 9, 10)) +  # Set specific x-axis breaks
  scale_color_manual(values = c('blue', 'green', 'red')) +  # Change colors as needed
  theme_minimal()
