rm(list=ls()) 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(janitor)

PERMID <- "7154990" 
PERMID <- as.numeric(gsub("\\D", "", PERMID))
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

race_count_clean <- ibt_data_clean%>%
  group_by(race)%>%
  summarise(n = n())

#6
avg_q65_time <- mean(ibt_data_clean$q65_page_submit, na.rm = TRUE)

q65_stat_by_race <- ibt_data_clean %>%
  mutate(q65_indicator = ifelse(q65_page_submit >= avg_q65_time, "above_average", "below_average")) %>%
  group_by(race, q65_indicator) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = q65_indicator, values_from = count) %>%
  mutate(Total = above_average + below_average) %>%
  ungroup() %>%
  mutate(
    above_average_Share = sprintf("%.1f%%", (above_average / Total) * 100),
    below_average_Share = sprintf("%.1f%%", (below_average / Total) * 100),
    Total_Share = sprintf("%.1f%%", ((below_average / Total * 100) + (above_average / Total * 100)))
  ) %>%
  select(race, above_average_Share, below_average_Share, Total_Share)

race_q65_stat <- q65_stat_by_race %>%
  rename( 
    above_average = above_average_Share,
    below_average = below_average_Share,
    Total = Total_Share
  )%>%
  mutate(test = 'insects and pleasant words aligned')

#7
avg_q66_time <- mean(ibt_data_clean$q66_page_submit, na.rm = TRUE)

q66_stat_by_race <- ibt_data_clean %>%
  mutate(q66_indicator = ifelse(q66_page_submit >= avg_q66_time, "above_average", "below_average")) %>%
  group_by(race, q66_indicator) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = q66_indicator, values_from = count) %>%
  mutate(Total = above_average + below_average) %>%
  ungroup() %>%
  mutate(
    above_average_Share = sprintf("%.1f%%", (above_average / Total) * 100),
    below_average_Share = sprintf("%.1f%%", (below_average / Total) * 100),
    Total_Share = sprintf("%.1f%%", ((below_average / Total * 100) + (above_average / Total * 100)))
  ) %>%
  select(race, above_average_Share, below_average_Share, Total_Share)

race_q66_stat <- q66_stat_by_race %>%
  rename(
    above_average = above_average_Share,
    below_average = below_average_Share,
    Total = Total_Share
  )%>%
  mutate(test = "flowers and pleasant words aligned")

combined_data <- merge(race_q65_stat, race_q66_stat, all = TRUE) %>%
  pivot_longer(cols = 2:3,
               names_to = "tag",
               values_to = "averages")
custom_labels <- function(variable) {
  ifelse(tag == "above_average", "Above Average", "Below Average")
}
custom_labels <- c("above_average" = "Above Average", "below_average" = "Below Average")

ggplot(combined_data, aes(y = averages, x = race, fill = test)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap("tag", labeller = as_labeller(custom_labels)) +
  labs(title = "Implicit Bias Test Speed Performance by Race",
       x = "Race",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5))

  