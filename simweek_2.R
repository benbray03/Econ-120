rm(list=ls()) 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(janitor)

PERMID <- "7154990" 
PERMID <- as.numeric(gsub("\\D", "", PERMID)) #Don't touch
set.seed(PERMID) 


ibt_data <- read_csv("ibt_testdata.csv")%>%
  slice(-(1:2))%>%
  clean_names()

ibt_data <- ibt_data%>%
  select(duration_in_seconds, finished, q65_page_submit, q66_page_submit, race_ethnicity, english)

ibt_data_finished <- ibt_data%>%
  filter(finished == TRUE)

race_count <- ibt_data_finished%>%
  group_by(race_ethnicity)%>%
  summarise(n = n())

ibt_data_clean <- ibt_data_finished%>%
  mutate(
    race = ifelse(grepl("asi", race_ethnicity, ignore.case = TRUE), "asian", race_ethnicity),
    race = ifelse(grepl("chi", race, ignore.case = TRUE), "asian", race),
    race = ifelse(grepl("korean", race, ignore.case = TRUE), "asian", race),
    race = ifelse(grepl("white", race, ignore.case = TRUE), "white", race),
    race = ifelse(!grepl("white|asian", race, ignore.case = TRUE), "other", race),
    duration_in_seconds = as.numeric(duration_in_seconds),
    q65_page_submit = as.numeric(q65_page_submit),
    q66_page_submit = as.numeric(q66_page_submit)
  )

#2a
avg_q65_time <- mean(ibt_data_clean$q65_page_submit, na.rm = TRUE)

q65_stat_by_native_language <- ibt_data_clean %>%
  mutate(q65_indicator = ifelse(q65_page_submit >= avg_q65_time, "above_average", "below_average"))

native_q65_stat <- q65_stat_by_native_language %>%
  tabyl(english, q65_indicator)%>%
  adorn_totals(c("col", "row"))%>%
  adorn_percentages("row")%>%
  adorn_pct_formatting(digits = 1)

#2b
avg_q66_time <- mean(ibt_data_clean$q66_page_submit, na.rm = TRUE)

q66_stat_by_native_language <- ibt_data_clean %>%
  mutate(q66_indicator = ifelse(q66_page_submit >= avg_q66_time, "above_average", "below_average"))

native_q66_stat <- q66_stat_by_native_language %>%
  tabyl(english, q66_indicator)%>%
  adorn_totals(c("col", "row"))%>%
  adorn_percentages("row")%>%
  adorn_pct_formatting(digits = 1)

#boxplot

response_times_data <- ibt_data_clean %>%
  select(english, q65_page_submit, q66_page_submit)

response_times_data <- response_times_data %>%
  pivot_longer(cols = c(q65_page_submit, q66_page_submit), names_to = "question", values_to = "response_time")

# Create boxplots side by side for Questions 65 and 66
custom_labels <- c("q65_page_submit" = "Insects and Pleasantries", "q66_page_submit" = "Flowers and Pleasantries")

ggplot(response_times_data, aes(x = english, y = response_time, fill = english)) +
  geom_boxplot() +
  facet_wrap(~ question, scales = "free_x", nrow = 1, , labeller = as_labeller(custom_labels)) +
  labs(title = "Response Time by Language Proficiency",
       x = "Language Proficiency", y = "Response Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels if needed
        strip.background = element_blank(),  # Remove background of facet labels
        strip.placement = "outside",  # Place facet labels outside the plot
        axis.line = element_line(color = "black", size = 1),
        plot.title = element_text(hjust = 0.5))

#bar

combined_data <- merge(native_q65_stat, native_q66_stat, all = TRUE) %>%
  pivot_longer(cols = 2:3,
               names_to = "tag",
               values_to = "averages")

custom_labels <- c("above_average" = "Above Average", "below_average" = "Below Average")


ggplot(combined_data, aes(y = averages, x = english, fill = test)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap("tag", labeller = as_labeller(custom_labels)) +
  labs(title = "Implicit Bias Test Speed Performance by English Proficiency",
       x = "English Proficiency Status",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5))
