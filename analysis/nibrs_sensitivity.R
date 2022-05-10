
suppressMessages(library(vroom))
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(glue))
suppressMessages(library(xtable))
source(here('analysis', 'utils.R'))


# load files ----

files_nibrs <- list.files(here("data", "nibrs"))
files_nibrs <- files_nibrs[grepl('weights', files_nibrs) & !grepl('multiple', files_nibrs)]
df <- files_nibrs %>%
  purrr::map(~ vroom(here('data', 'nibrs', .x), col_types = cols()) %>%
               select(-fips)) %>%
  bind_rows() %>%
  select(-matches('nb|nnet|lasso|rf|logistic'))

# first letter to uppercase
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


p <- df %>%
  group_by(crime_recode) %>%
  sample_n(1000, replace = FALSE) %>%
  mutate(crime_recode = firstup(crime_recode)) %>%
  ggplot(aes(mod_log, mod_sl)) + 
  theme_bw() + 
  xlab(expression('Estimated likelihood of police notification\nby logistic regression with survey weights')) + 
  ylab(expression(atop('Estimated likelihood of police notification by the SuperLearner', 'accounting for survey weights'))) + 
  geom_abline(slope = 1, linetype = 'dashed', col = 'red') + 
  geom_point(alpha = 0.3) + 
  facet_wrap(~ factor(crime_recode, levels = c('Sex offense', 'Robbery', 'Aggravated assault', 'Simple assault')), ncol = 2)
ggsave(filename = here('figures', 'predictions_logistic_vs_superlearner.pdf'), plot = p, width = 8, height = 6)


# see distribution of predictions by crime type
df %>% group_by(crime_recode) %>% 
  summarise(quants = quantile(mod_sl, probs = c(1e-10, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))) %>%
  mutate(nrow = 1:n()) %>%
  pivot_wider(names_from = crime_recode, values_from = quants)

# stats overall and by crime type
df %>% summarise(pi_star = n()/sum(1/mod_sl), q_star = sum(y)/sum(1/mod_sl))
df %>% group_by(crime_recode) %>% summarise(pi_star = n()/sum(1/mod_sl), q_star = sum(y)/sum(1/mod_sl))

# stats by racial group overall and by crime type

stats_raceoff_tb <- df %>% group_by(crime_recode, race_of_offender) %>% 
  summarise(pi_star = n()/sum(1/mod_sl), 
            q_star = sum(y)/sum(1/mod_sl)) %>%
  pivot_longer(cols = c(pi_star, q_star), names_to = 'stat', values_to = 'value') %>%
  mutate(value = case_when(
    value < 1 ~ glue('{round(value*100)}\\%'),
    TRUE ~ glue('{round(value)}')
  )) %>%
  pivot_wider(names_from = crime_recode, values_from = value) %>%
  group_by(race_of_offender) %>% mutate(n_row = 1:n()) %>%
  arrange(n_row, race_of_offender) %>% select(-n_row) %>%
  mutate(race_of_offender = glue('{race_of_offender} offenders')) %>%
  select(race_of_offender, `sex offense`, robbery, `aggravated assault`, `simple assault`) 
stats_raceoff_tb$race_of_offender <- glue('\\textbullet\\; {stats_raceoff_tb$race_of_offender}')

desc_tb <- c('\\% police notification, $\\pi^*$', 
             '\\% arrests (w/$\\pi$), $q^*$') %>%
  cbind(., matrix(NA, nrow = 2, ncol = 4))
colnames(desc_tb) <- colnames(stats_raceoff_tb)

desc_tb[1,] %>%
  bind_rows(stats_raceoff_tb[c(1,2),]) %>%
  bind_rows(desc_tb[2,]) %>%
  bind_rows(stats_raceoff_tb[c(3,4),]) %>%
  xtable() %>% print(., include.rownames=FALSE, sanitize.text.function = identity)



