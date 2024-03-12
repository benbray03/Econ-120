rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse) 

PERMID <- "7154990" 
PERMID <- as.numeric(gsub("\\D", "", PERMID))
set.seed(PERMID) 

grades <- function(seed){
  set.seed(seed)
  
  grades_1_fall <- tibble("name" = c("isabel", "marcos" , "john" , "imani" , "kenji") ,
                          "quarter" = rep("fall" , 5),
                          "grade_first_year" = sample(60:100 , 5))
  
  grades_1_winter <- tibble("name" = c("isabel", "marcos" , "john" , "imani" , "kenji") ,
                            "quarter" = rep("winter" , 5),
                            "grade_first_year" = sample(60:100 , 5))
  
  grades_1_spring <- tibble("name" = c("isabel", "marcos" , "john" , "imani" , "kenji") ,
                            "quarter" = rep("spring" , 5),
                            "grade_first_year" = sample(60:100 , 5))
  
  freshman_grade <- bind_rows(grades_1_fall,grades_1_winter , grades_1_spring)
  
  grades_2_fall <- tibble(
    "student_name" = c("isabel", "marcos" , "john" , "yuriko" , "mickey") ,
    "student_quarter" = rep("fall" , 5),
    "grade_second_year" = sample(60:100 , 5))
  
  grades_2_winter <- tibble(
    "student_name" = c("isabel", "marcos" , "john" , "yuriko" , "mickey") ,
    "student_quarter" = rep("winter" , 5),
    "grade_second_year" = sample(60:100 , 5))
  
  grades_2_spring <- tibble(
    "student_name" = c("isabel", "marcos" , "john" , "yuriko" , "mickey") ,
    "student_quarter" = rep("spring" , 5),
    "grade_second_year" = sample(60:100 , 5))
  
  sophomore_grade <- bind_rows(grades_2_fall,grades_2_winter , grades_2_spring)
  
  return(list(freshman_grade,sophomore_grade))
}

freshman_grade <- grades(PERMID)[[1]]

sophomore_grade <- grades(PERMID)[[2]]

grade_fall_1 <- left_join(freshman_grade, sophomore_grade, by = c("name" = "student_name", "quarter" = "student_quarter"))%>%
  filter(quarter == "fall")

grade_fall_2 <- right_join(freshman_grade, sophomore_grade, by = c("name" = "student_name", "quarter" = "student_quarter")) %>%
  filter(quarter == "fall")

grade_fall_3 <- inner_join(freshman_grade, sophomore_grade, by = c("name" = "student_name", "quarter" = "student_quarter"))%>%
  filter(quarter == "fall")

grade_fall_4 <- full_join(freshman_grade, sophomore_grade, by = c("name" = "student_name", "quarter" = "student_quarter")) %>%
  filter(quarter == "fall")

grade_all_quarter_1 <- left_join(freshman_grade, sophomore_grade, by = c("name" = "student_name", "quarter" = "student_quarter"))

grade_all_quarter_2 <- right_join(freshman_grade, sophomore_grade, by = c("name" = "student_name", "quarter" = "student_quarter"))

grade_all_quarter_3 <- inner_join(freshman_grade, sophomore_grade, by = c("name" = "student_name", "quarter" = "student_quarter"))

grade_all_quarter_4 <- full_join(freshman_grade, sophomore_grade, by = c("name" = "student_name", "quarter" = "student_quarter"))
