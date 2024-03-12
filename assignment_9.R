rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse) 
PERMID <- "7154990" 
PERMID <- as.numeric(gsub("\\D", "", PERMID)) 
set.seed(PERMID)

grades <- read_csv('my_local_HS_grades.csv')

my_local_HS_grades <- grades %>% 
  mutate(grade_cat = case_when(
    str_detect(grade_given, "^A") ~ "A",
    str_detect(grade_given, "^B") ~ "B",
    str_detect(grade_given, "^C") ~ "C",
    str_detect(grade_given, "^D") ~ "D",
    str_detect(grade_given, "^F") ~ "F",
  ))

overall_grade_share <- my_local_HS_grades %>%
  group_by(dept, grade_cat) %>%
  summarise(total_people = sum(sum_of_student_count)) %>%
  group_by(dept) %>%
  mutate(share = total_people / sum(total_people) * 100) %>%
  ungroup() %>%
  select(dept, grade_cat, total_people, share)

png("overall_grade_share_plot.png")
ggplot(overall_grade_share, aes(x = dept, y = share, fill = grade_cat)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Share of Students in Each Grade Category by Department", fill = "Grade", x = "Department", y = "Share") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()
dev.off()

compsci_grade_over_time <- my_local_HS_grades %>%
  filter(dept == "CMPSC")%>%
  mutate(year = as.numeric(sub("(F|S|W|M)(\\d{2})", "20\\2", quarter)),
         quarter = sub("(F|S|W|M)(\\d{2})", "\\1", quarter))%>%
  group_by(year, grade_cat)%>%
  summarise(total_people = sum(sum_of_student_count))%>%
  mutate(share = total_people / sum(total_people) * 100) %>%
  select(year, grade_cat, total_people, share)

png("compsci_grade_over_time_plot.png")
ggplot(compsci_grade_over_time, aes(x = year, y = share, color = grade_cat)) +
  geom_line() +
  geom_point() +
  labs(title = "Computer Science Grade Distribution from 2009-2022", color = "Grade", x = "Year", y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(fill=NA))
dev.off()

all_courses_grade_over_time <- my_local_HS_grades %>%
  mutate(year = as.numeric(sub("(F|S|W|M)(\\d{2})", "20\\2", quarter)),
         quarter = sub("(F|S|W|M)(\\d{2})", "\\1", quarter))%>%
  group_by(year, dept, grade_cat)%>%
  summarise(total_people = sum(sum_of_student_count))%>%
  group_by(dept, year)%>%
  mutate(share = total_people / sum(total_people) * 100) %>%
  select(year, dept, grade_cat, total_people, share) %>%
  arrange(dept)

png("all_courses_grade_over_time_plot.png")
ggplot(all_courses_grade_over_time, aes(x=year, y=share, color = grade_cat)) +
  geom_line() +
  facet_wrap(~dept) + 
  geom_point(size = 1) + 
  labs(title = "Grade Distribution by Department from 2009-2022", color = "Grade", x="Year", y="Percentage") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(fill=NA))
dev.off()

compsci_total_enrollment <- compsci_grade_over_time%>%
  group_by(year) %>%
  summarise(total_people = sum(total_people)) %>%
  select(year, total_people)

png("extragraph.png")
ggplot(compsci_total_enrollment, aes(x = year, y = total_people)) +
  geom_line() +
  geom_point() +
  labs(title = "Computer Science Enrollment from 2009-2022", color = "Grade", x = "Year", y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(fill=NA))
dev.off()
