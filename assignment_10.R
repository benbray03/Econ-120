rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

PERMID <- "7154990" 
PERMID <- as.numeric(gsub("\\D", "", PERMID))
set.seed(PERMID)

library(tidyverse)

grade_data_easy_class <- tibble("hours" = sample(seq(1 ,5.5 , 0.5) , 10) ,
                                "class_level" = rep(1,10) ,
                                "error" = rnorm(10,2,5) ) %>%
  mutate(grade = 65 + 2*hours + error)

grade_data_hard_class <- tibble("hours" = sample(seq(4 ,8.5 , 0.5) , 10) ,
                                "class_level" = rep(0,10) ,
                                "error" = rnorm(10,2,5)) %>%
  mutate(grade = 50 + hours + error)

grade_data <- bind_rows(grade_data_easy_class , grade_data_hard_class) %>%
  select(-c(error)) %>%
  mutate(class = seq(1,20,1))


grade_data %>% 
  ggplot(aes(x = hours , y = grade , color = "all classes")) +
  geom_point() +
  xlab("Hours")+
  ylab("Grade")+
  ggtitle("Grade versus Hours Spent Studying")+
  scale_color_manual(values = c("all classes" = "black")) +
  geom_smooth(method = lm , se = 0 )

grade_data %>% ggplot(aes(x = hours , y = grade , color = "all classes")) +
  geom_point(data = grade_data %>% filter(class_level == 1) ,
             aes(x = hours, y = grade, color = "easier class")) +
  geom_point(data = grade_data %>% filter(class_level == 0) ,
             aes(x = hours, y = grade, color = "harder class")) +
  geom_smooth(data = grade_data %>% filter(class_level == 1) ,
              aes(x = hours, y = grade, color = "easier class"), method = lm) +
  geom_smooth(data = grade_data %>% filter(class_level == 0) ,
              aes(x = hours, y = grade, color = "harder class"), method = lm) +
  scale_color_manual(values = c("easier class" = "red" ,
                                "harder class" = "blue" ,
                                "all classes" = "black")) +
  xlab("Hours")+
  ylab("Grade")+
  ggtitle("Grade versus Hours Spent Studying Split by Class Levels")+
  geom_smooth(method = lm, se = 0)
