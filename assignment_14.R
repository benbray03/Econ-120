rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)

PERMID <- "7154990" 
PERMID <- as.numeric(gsub("\\D", "", PERMID))
set.seed(PERMID)

#1
dailySteps <- read_csv("dailySteps.csv")

#2
sleepDay <- read_csv("sleepDay.csv")

#3
sleepDayDate <- sleepDay %>%
  mutate(SleepDay = mdy_hms(SleepDay) %>% date())%>%
  rename("ActivityDay" = "SleepDay")

dailyStepsDate <- dailySteps %>%
  mutate(ActivityDay = mdy(ActivityDay))

merged_data <- inner_join(dailyStepsDate, sleepDayDate, by = c("Id", "ActivityDay"))

nobs1 <- merged_data %>%
  filter(!is.na(TotalSleepRecords) & !is.na(TotalMinutesAsleep) & !is.na(TotalTimeInBed)) %>%
  nrow()


#4
unique_ids <- merged_data %>%
  distinct(Id)

nMatchedPeople <- nrow(unique_ids)

#5
correlation_data <- merged_data %>%
  mutate(SleepPercentage = TotalMinutesAsleep / TotalTimeInBed) 

theCorrelationCoefficient <- cor(correlation_data$SleepPercentage, correlation_data$StepTotal, use = "complete.obs")

