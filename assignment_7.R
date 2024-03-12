rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(janitor)

PERMID <- "7154990"
PERMID <- as.numeric(gsub("\\D", "", PERMID))
set.seed(PERMID)

festival <- read_csv("assign_2.csv")
festival <- clean_names(festival)

festival_unique <- festival%>%
  select(x, days_attend, hours_attend, gender, age, extend, spend_donations)%>%
  distinct(x, .keep_all = TRUE)

festival_clean_1 <- festival_unique %>%
  separate(spend_donations,
           into = c("spend_donations_min", "spend_donations_max"),
           sep = "-",
           remove = FALSE) %>%
  mutate(spend_donations_min = parse_number(spend_donations_min),
         spend_donations_max = parse_number(spend_donations_max))

festival_clean_2 <- festival_clean_1%>%
  mutate(spend_donations_min_perhour = ifelse(hours_attend > 0, spend_donations_min / hours_attend, NA),
         spend_donations_max_perhour = ifelse(hours_attend > 0 & !is.na(hours_attend), spend_donations_max / hours_attend, NA))

festival_clean <- festival_clean_2 %>%
  mutate(extend = coalesce(extend, "Unclear"))

festival_sum_stat_age <- festival_clean %>%
  group_by(age)%>%
  summarize(avg_spend_donations_min_perhour = mean(spend_donations_min_perhour, na.rm = TRUE),
            avg_spend_donations_max_perhour = mean(spend_donations_max_perhour, na.rm = TRUE))

festival_sum_stat_extend <- festival_clean %>%
  group_by(extend)%>%
  summarize(avg_spend_donations_min_perhour = mean(spend_donations_min_perhour, na.rm = TRUE),
            avg_spend_donations_max_perhour = mean(spend_donations_max_perhour, na.rm = TRUE))

age_extend <- festival_clean%>%
  mutate(extend_values = ifelse(extend == 'Yes', 'Yes',
                                ifelse(extend == 'No', 'No', 'Unclear')))%>%
  group_by(age)%>%
  janitor::tabyl(age, extend_values)%>%
  adorn_percentages()%>%
  adorn_pct_formatting()

age_extend <- festival_clean%>%
  group_by(age)%>%
  janitor::tabyl(age, extend)%>%
  adorn_percentages()%>%
  adorn_pct_formatting()%>%
  adorn_title(placement = 'combined', 'age category', 'extend')

extend_age <- festival_clean%>%
  group_by(extend)%>%
  janitor::tabyl(extend, age)%>%
  adorn_percentages()%>%
  adorn_pct_formatting()%>%
  adorn_title(placement = 'combined', 'extend', 'age')

