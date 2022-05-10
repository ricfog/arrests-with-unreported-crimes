
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(survey))
suppressMessages(library(tidymodels))
suppressMessages(library(glue))
suppressMessages(library(cli))
suppressMessages(library(xtable))
suppressMessages(library(data.table))
suppressMessages(library(dtplyr))
source(here('analysis', 'utils_regression.R'))


# -----

# load nibrs data
files_nibrs <- list.files(here("data", "nibrs"))
files_nibrs <- files_nibrs[grepl("imputed", files_nibrs) & grepl("weights", files_nibrs) & grepl("multiple", files_nibrs)]
files_names <- str_sub(files_nibrs, start = 14) %>%
  str_remove_all(., "data_imputed|_multiple_weights.csv|_1")
df_list <- files_nibrs %>%
  purrr::map(~ read_csv(here("data", "nibrs", .x), col_types = cols()) %>%
    select(-matches("lasso|rf|nb|sl|nnet|logistic")) %>%
    filter(year >= 2006 & year <= 2015)) %>%
  setNames(files_names)
df_list <- df_list %>% map(~ .x %>%
  mutate(wgt = 1 / mod_log) %>%
  select(-mod_log))


df_list <- df_list %>% map(~ .x %>% filter(race_of_offender == 'black' | race_of_offender == 'white') %>%
  filter(race_of_victim == 'black' | race_of_victim == 'white'))
# uncomment for only multiple offenders
df_list <- df_list %>% map(~ .x %>% filter(multiple_offender == 'yes'))
# uncomment co-offending
# df_list <- df_list %>% map(~ .x %>% filter(multiple_offender == 'yes') %>%
#                              group_by(year, state_abbv, incident_number) %>%
#                              mutate(n = n(),
#                                        race_white = sum(race_of_offender == 'white')) %>%
#                              filter(n == 2 & race_white == 1) %>% select(-n, -race_white) %>%
#                              ungroup)


# load logistic regression models fitted on NCVS
data_code <- "0010"
logreg_ncvs <- read_rds(here('data', 'regmodels', glue('ncvs_1_{data_code}_weightedlog.rds')))
logreg_ncvs_recipe <- read_rds(here('data', 'regmodels', glue('ncvs_1_{data_code}_weightedlog_recipe.rds')))


# GEEs
# run_wgt_gee(df_original, df_processed,
#   offense_name,
#   wgt = TRUE,
#   verbose = TRUE,
#   max_iter = 10
# )


coefsw_est <- df_list %>%
  purrr::imap(~ run_wgt_gee(
    df_original = .x,
    df_processed = prepare_df_for_reg_multiple(
      df = .x %>% select(-multiple_offender),
      offense_name = .y,
      wgt = TRUE
    ),
    offense_name = .y,
    wgt = TRUE,
    max_iter = 10
  ))

coefsw_est %>% map(~ .x$coefs %>% filter(grepl('race', term)))

coefsw_var <- coefsw_est %>%
  map( ~ get_var_gee(est = .x,
                     logreg_ncvs = logreg_ncvs,
                     logreg_ncvs_recipe = logreg_ncvs_recipe,
                     fake_run = FALSE))

# 
# bind estimates & compute p values and statistics
coefsw <- map2(.x = coefsw_est, 
               .y = coefsw_var, 
               .f = ~ pluck(.x, 'coefs') %>% 
                 bind_cols(
                   tibble(std.error = sqrt(diag(.y)))
                 ) %>%
                 mutate(
                   statistic = estimate / std.error,
                   p.value = 2 * (1 - sapply(
                     abs(statistic),
                     stats::pnorm
                   ))
                 )
)

coefsw_tab <- get_table_coef(coefsw)

cli_h2('Regression results')

print(xtable(reorder_and_filter_rows(coefsw_tab), 
              align = c('l', 'l', rep('c', ncol(coefsw_tab) -1))),
      include.rownames = FALSE,
      booktabs = TRUE,
      sanitize.text.function=function(x){x})
