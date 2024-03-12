rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse) 
PERMID <- "7154990" 
PERMID <- as.numeric(gsub("\\D", "", PERMID)) 
set.seed(PERMID)

unbiased_die_25 <- sample(1:6, size = 25, replace = T, prob = rep(1/6, 6))

unbiased_die_tibble_25 <- unbiased_die_25 %>% as_tibble() %>%
  group_by(value) %>%
  summarise(n = n(), share = n/25)

biased_die_25 <- sample(1:6, size = 25, replace = T, prob = c(3/6,1/6,rep(1/12,4)))

biased_die_tibble_25 <- biased_die_25 %>% as_tibble() %>%
  group_by(value) %>%
  summarise(n = n(), share = n/25)


png("biased_vs_unbiased_plot_25.png")
biased_vs_unbiased_plot_25 <- barplot(
  rbind(unbiased_die_tibble_25$share, biased_die_tibble_25$share),
  beside = TRUE,
  col = c("blue", "red"),
  xlab = "Number",
  ylab = "Proportion",
  names.arg = c('one', 'two', 'three', 'four', 'five', 'six'),
  main = "Fair vs. Unfair Die (25 Rolls)",
  legend.text = c("Fair", "Unfair"),
  args.legend = list(x = "top", fill = c("blue", "red"))
)
dev.off()

unbiased_die_10 <- sample(1:6, size = 10000, replace = T, prob = rep(1/6, 6))

unbiased_die_tibble_10 <- unbiased_die_10 %>% as_tibble() %>%
  group_by(value) %>%
  summarise(n = n(), share = n/10000)

biased_die_10 <- sample(1:6, size = 10000, replace = T, prob = c(3/6,1/6,rep(1/12,4)))

biased_die_tibble_10 <- biased_die_10 %>% as_tibble() %>%
  group_by(value) %>%
  summarise(n = n(), share = n/10000)

png("biased_vs_unbiased_plot_10.png")
biased_vs_unbiased_plot_10 <- barplot(
  rbind(unbiased_die_tibble_10$share, biased_die_tibble_10$share),
  beside = TRUE,
  col = c("blue", "red"),
  names.arg = c('one', 'two', 'three', 'four', 'five', 'six'),
  main = "Fair vs. Unfair Die (10,000 rolls)",
  xlab = "Number",
  ylab = "Proportion",
  legend.text = c("Fair", "Unfair"),
  args.legend = list(x = "topright", fill = c("blue", "red"))
)
dev.off()
