
set.seed(431214324)

suppressMessages(library(tidymodels))
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(glue))
suppressMessages(library(keras))
suppressMessages(library(cli))
suppressMessages(library(keras))
suppressMessages(library(dtplyr))

use_condaenv("py3.9", required = TRUE)

source(here('analysis', 'utils_regression.R'))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  multiple <- FALSE
} else {
  multiple <- ifelse(is.null(args[1]), FALSE, TRUE)
}
if(multiple){
  data_code <- '0010'
} else{
  data_code <- '0110'
}


cli_li(glue("Data code for NCVS: ", data_code))


# ----
 
# load nibrs data
files_nibrs <- list.files(here("data", "nibrs"))
files_nibrs <- files_nibrs[#grepl(data_code, files_nibrs) & 
  grepl("imputed", files_nibrs) & 
    !grepl('weights', files_nibrs)]
if(multiple){
  files_nibrs <- files_nibrs[grepl('multiple', files_nibrs)]
} else{
  files_nibrs <- files_nibrs[!grepl('multiple', files_nibrs)]
}
nibrs <- files_nibrs %>%
  purrr::map_dfr(~ read_csv(here('data', 'nibrs', .x), col_types = cols(fips = col_character()))) %>%
  filter(age_of_victim >= 12)

# load regression models
wl <- read_rds(here('data', 'regmodels', glue('ncvs_1_{data_code}_weightedlog.rds')))
wl_recipe <- read_rds(here('data', 'regmodels', glue('ncvs_1_{data_code}_weightedlog_recipe.rds')))
sl <- read_rds(here('data', 'regmodels', glue('ncvs_1_{data_code}_superlearner.rds')))
sl_nnet <- load_model_hdf5(here('data', 'regmodels', glue('ncvs_1_{data_code}_superlearner_keras.hdf5')))


# ----

if(multiple){
  nibrs4reg <- prepare_nibrs_for_reg_multiple(nibrs)
} else{
  nibrs4reg <- prepare_nibrs_for_reg(nibrs)
}


# get predictions for logistic regression
log_preds <- predict(wl, bake(wl_recipe, nibrs4reg), type = 'response')

# get predictions for superlearner
not_nnet <- c('mod_lasso', 'mod_rf', 'mod_logistic', 'mod_nb')
sl_preds <- sl$models[not_nnet] %>%
  map_dfr(~ predict(.x, nibrs4reg %>%
                  mutate(stratum = '1', psu = '1', incident_weight_adjusted_for_series_crimes = 1),
                type = 'prob') %>% 
        select(.pred_yes) %>%
          mutate(nrow = 1:n()), .id = 'model')
          
X_nnet <- bake(extract_recipe(sl$models$mod_nnet), nibrs4reg %>%
                  mutate(stratum = 1, psu = 1, incident_weight_adjusted_for_series_crimes = 1))
sl_preds_nnet <- predict(sl_nnet, as.matrix(X_nnet), type = 'prob')[,2] %>% 
    tibble(.pred_yes = .) %>%
    mutate(nrow = 1:n())

sl_preds_wide <- sl_preds %>%
  bind_rows(sl_preds_nnet %>% mutate(model = 'mod_nnet')) %>%
  pivot_wider(names_from = model, values_from = .pred_yes) %>%
  select(-nrow)

sl_preds_comb <- as.matrix(sl_preds_wide %>% select(all_of(names(sl$weights)))) %*% t(sl$weights)


# predictions
preds <- tibble(mod_log = log_preds) %>%
 bind_cols(sl_preds_wide) %>%
 bind_cols(mod_sl = sl_preds_comb[,1])

if(multiple){
  preds <- preds %>%
    bind_cols(nibrs4reg %>% select(year, originating_agency_identifier, incident_number))
    nibrsw <- nibrs %>% inner_join(preds, by = c('originating_agency_identifier', 'incident_number', 'year'))
} else{
  nibrsw <- nibrs %>% bind_cols(preds)
}

# save weighted NIBRS
v <- files_nibrs %>%
  purrr::map(~ nibrsw %>% filter(crime_recode == str_sub(.x, start = 14, end=ifelse(multiple, -16, -7))) %>%
               write_csv(here('data', 'nibrs',
                                paste0(str_sub(.x, end=-5), '_weights.csv'))))
