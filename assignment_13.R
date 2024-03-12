rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)

PERMID <- "7154990" 
PERMID <- as.numeric(gsub("\\D", "", PERMID)) 
set.seed(PERMID) 

player_salary <- read_csv('player_salary.csv')
player_salary <- player_salary%>%
  select(c(-1))

cpi_fed <- read_csv('CPI_U_minneapolis_fed.csv')
cpi_fed <- cpi_fed%>%
  select(year, CPI)

player_salary_cpi1 <- player_salary %>%
  left_join(cpi_fed, by = c("season" = "year") , relationship = "many-to-one")

player_salary_cpi2 <- player_salary_cpi1%>%
  mutate(real_2015_salary = salary*(237.0/CPI))

player_salary_cpi <- player_salary_cpi2 %>%
  mutate(real_2015_salary_mill = real_2015_salary/1000000)

salary_by_year_stat <- player_salary_cpi %>%
  group_by(season) %>%
  summarise(min_salary = min(real_2015_salary_mill),
            avg_salary = mean(real_2015_salary_mill),
            max_salary = max(real_2015_salary_mill))

nba_games_perf_minutes1 <- read_csv('nba_games_perf_minutes.csv') %>%
  select(-1)%>%
  mutate(player_name = tolower(player_name))

nba_games_perf_minutes_dups <- nba_games_perf_minutes1%>%
  group_by(player_name, game_id) %>%
  summarize(n = n())%>%
  ungroup() %>%
  filter(n>1)

nba_games_perf_minutes <- distinct(nba_games_perf_minutes1, .keep_all = TRUE)

nba_games_perf_points1 <- read_csv('nba_games_perf_points.csv') %>%
  select(2:4)%>%
  mutate(player_name = tolower(player_name))

nba_games_perf_points_dups <- nba_games_perf_points1 %>%
  group_by(player_name, game_id)%>%
  summarize(n = n())%>%
  ungroup()%>%
  filter(n>1)

nba_games_perf_points <- distinct(nba_games_perf_points1, .keep_all = TRUE)

nba_games_perf <- inner_join(nba_games_perf_minutes, nba_games_perf_points, by = c("player_name", "game_id"))

nba_games_perf <- nba_games_perf %>%
  mutate(minutes = as.numeric(substr(min, 1, 2))) %>%
  relocate(minutes, .after = min)
  
nba_games_perf_avg <- nba_games_perf %>%
  summarise(avg_min = mean(minutes, na.rm = TRUE),
            avg_points = mean(pts, na.rm = TRUE))

nba_games_season <- read_csv('nba_games_season.csv') %>%
  select(2:3)

nba_games_perf_season <- nba_games_perf %>%
  left_join(nba_games_season, by = 'game_id', relationship = 'many-to-one')

nba_games_perf_season_avg <- nba_games_perf_season %>%
  group_by(season)%>%
  summarise(avg_min = mean(minutes, na.rm = T),
            avg_pts = mean(pts, na.rm = T))%>%
  slice(1:6)

player_salary_perf <- salary_by_year_stat %>%
  inner_join(nba_games_perf_season_avg, by = c('season'))

