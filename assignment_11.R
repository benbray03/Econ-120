#assignment_11

rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)

PERMID <- "7154990" 
PERMID <- as.numeric(gsub("\\D", "", PERMID))
set.seed(PERMID)

gtrend_data <- read_csv("inf_google_trend_NOT_clean.csv")
gtrend_data <- gtrend_data%>%
  select(-c(1,2))

gtrend_data_clean1 <- gtrend_data %>%
  tidyr::extract(c(country.date), into = c("geo", "date"),
                 regex = ("(.+)\\:(.+)")) %>%
  mutate(geo = case_when(
    geo == "CA" ~ "canada",
    geo == "US" ~ "united states",
    geo == "ZA" ~ "south africa",
    geo == "MX" ~ "mexico")) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

gtrend_data_clean2 <- gtrend_data_clean1 %>%
  tidyr::extract(c(google.search.rate),
                 into = c("inflation", "interest_rate", "market", "job", "trade"),
                 regex = ("(.+)\\-(.+)\\-(.+)\\-(.+)\\-(.+)")) %>%
  mutate(inflation = as.numeric(inflation),
         interest_rate = as.numeric(interest_rate),
         market = as.numeric(market),
         job = as.numeric(job),
         trade = as.numeric(trade))

gtrend_data_clean3 <- gtrend_data_clean2%>%
  filter(as.Date(`date`) > as.Date("2004-12-01"))

gtrend_data_clean <- gtrend_data_clean3 %>%
  mutate(across(all_of(c("inflation", "interest_rate", "market", "job", "trade")), 
                ~ ifelse(is.na(.), ifelse(!is.na(lag(.)), lag(.), NA), .)))

US_inflation_by_year_before <- gtrend_data_clean %>%
  filter(geo == "united states") %>%
  mutate(year = year(date)) %>% group_by(year)

US_inflation_by_year <- US_inflation_by_year_before%>%            
  summarize(avg_yearly_inflation_US = mean(value, na.rm = TRUE)) %>%
  rename(year_us = year)

US_correlation <- gtrend_data_clean %>%
  filter(geo == "united states") %>%
  select(value, inflation, interest_rate, market, job, trade) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()%>%
  select(value)

CA_correlation <- gtrend_data_clean %>%
  filter(geo == "canada") %>%
  select(value, inflation, interest_rate, market, job, trade) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()%>%
  select(value)

MX_correlation <- gtrend_data_clean %>%
  filter(geo == "mexico") %>%
  select(value, inflation, interest_rate, market, job, trade) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()%>%
  select(value)

SA_correlation <- gtrend_data_clean %>%
  filter(geo == "mexico") %>%
  select(value, inflation, interest_rate, market, job, trade) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()%>%
  select(value)

all_country_correlation <- list(CA_correlation = CA_correlation, 
                                US_correlation = US_correlation, 
                                MX_correlation = MX_correlation, 
                                SA_correlation = SA_correlation)
write.csv(all_country_correlation, file = "all_country_correlation.csv", row.names = TRUE)

figure_1 <- gtrend_data_clean%>%
  filter(geo == 'united states')%>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = inflation/10, col = 'search for the word inflation')) +
  geom_line(aes(y = value , color = "inflation rate")) +
  scale_color_manual(values = c("black", "red")) +
  guides(color=guide_legend("")) +
  theme_classic() +
  ylab("Rate") +
  ggtitle("Inflation Rate versus Google Searches for 'Inflation'") + 
  geom_vline(aes(xintercept = lubridate::as_date("2020-03-01")), linetype = "dashed") +
  ggplot2::annotate("text", x = lubridate::as_date("2020-03-01") , y = 10 ,
                    label = "COVID-19", size = 3 , hjust = -0.1) +
  xlab("Date")

df_newdata <- data.frame(
  Country = c("United States", "Canada", "Mexico", "South Africa"),
  Inflation = c(0.765143544453716, 0.732516761340703, 0.307393534945711, 0.307393534945711),
  Interest = c(0.571079919694043,	0.465606989348436,	0.0182973040907708,	0.0182973040907708),
  Market = c(0.242658596321715,	0.137117148929135,	-0.0489505667074451,	-0.0489505667074451),
  Job = c(-0.0569809894394664,	-0.0105138738927183,	-0.0375160658256804,	-0.0375160658256804),
  Trade = c(-0.0306587126208924,	0.0818843068811081,	-0.0864939035539538,	-0.0864939035539538)
  
)
df_newlong<- df_newdata%>%
  gather(variable, value, -Country)

ggplot(df_newlong, aes(x = Country, y = value, fill = variable)) +
  geom_col(position = 'dodge', width = 0.7) +
  labs(
    title = "Correlation for Adjusted Value of Inflation by Country and Google Search Word Rates",
    x = "Country",
    y = "Rate",
    fill = "Search Word"
  ) +
  theme_minimal() +  # Set a minimal theme for the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

figure_2 <- gtrend_data_clean %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = inflation/10 , col = "search for the word inflation")) +
  geom_line(aes(y = value, color = "inflation rate")) +
  scale_color_manual(values = c("black", "red")) +
  guides(color=guide_legend("")) +
  ylab("Rate") +
  ggtitle("Inflation Rate versus Google Searches for 'Inflation' in US, Canada, Mexico and South Africa")+
  geom_vline(aes(xintercept = lubridate::as_date("2020-03-01")), linetype = "dashed") +
  ggplot2::annotate("text", x = lubridate::as_date("2020-03-01") , y = 10 ,
                    label = "COVID-19", size = 2, hjust = -0.1) +
  xlab("Date") +
  facet_wrap(vars(geo), nrow = 2, ncol = 2)

all_correlation <- gtrend_data_clean %>%
  select(value, inflation, interest_rate, market, job, trade) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()%>%
  select(value)
