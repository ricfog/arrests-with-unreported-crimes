
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(survey))
suppressMessages(library(tidymodels))
suppressMessages(library(glue))
suppressMessages(library(cli))
suppressMessages(library(xtable))
suppressMessages(library(data.table))
suppressMessages(library(dtplyr))

source(here("analysis", "utils_regression.R"))

# ----

# load nibrs data
files_nibrs <- list.files(here("data", "nibrs"))
files_nibrs <- files_nibrs[grepl("imputed", files_nibrs) & grepl("weights", files_nibrs) & grepl("multiple", files_nibrs)]
files_names <- str_sub(files_nibrs, start = 14) %>%
  str_remove_all(., "data_imputed|_multiple_weights.csv|_1")
df_list <- files_nibrs %>%
  purrr::map(~ read_csv(here("data", "nibrs", .x), col_types = cols()) %>%
    select(-matches("lasso|rf|nb|sl|nnet")) %>%
    filter(year >= 2006 & year <= 2015)) %>%
  setNames(files_names)
df_list <- df_list %>% map(~ .x %>%
  mutate(wgt = 1 / mod_log) %>%
  select(-mod_log))

df_list <- df_list %>% map(~ .x %>%
  filter(race_of_offender == "black" | race_of_offender == "white") %>%
  filter(race_of_victim == "black" | race_of_victim == "white"))

# representation in police data
df_list %>% map(~ .x %>%
  group_by(race_of_offender, multiple_offender) %>%
  summarise(n = sum(1 / mod_logistic)) %>%
  mutate(prop = n / sum(n)))
df_list %>% map(~ .x %>%
  group_by(race_of_offender) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)))
df_list %>% map(~ .x %>%
  group_by(race_of_offender) %>%
  summarise(n = sum(1 / mod_logistic)) %>%
  mutate(prop = n / sum(n)))
# compute arrest rates by racial group of the offender
df_list %>% map(~ .x %>%
  group_by(race_of_offender, multiple_offender) %>%
  summarise(arrest_rate = sum(y) / sum(1 / mod_logistic), arrest_rate_police = mean(y)))

df_list %>% map(~ .x %>%
  group_by(multiple_offender, region) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)))
df_list %>% map(~ .x %>%
  group_by(multiple_offender, is_private_location) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)))

df_list %>% map(~ .x %>%
  distinct(incident_number, multiple_offender) %>%
  group_by(multiple_offender) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)))
df_list %>% map(~ .x %>%
  group_by(multiple_offender) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)))

