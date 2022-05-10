
set.seed(32432)

# load packages ----
suppressMessages(library(tidymodels))
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(Matrix))
suppressMessages(library(cli))
suppressMessages(library(glue))
suppressMessages(library(xtable))
suppressMessages(library(furrr))
suppressMessages(library(survey))
source(here('analysis', 'utils_regression.R'))


n_cores <- 50L
plan(multicore, workers = as.integer(n_cores))

# load data ----

# load nibrs data
files_nibrs <- list.files(here("data", "nibrs"))
files_nibrs <- files_nibrs[grepl('imputed', files_nibrs) & grepl('weights', files_nibrs) & !grepl('multiple', files_nibrs)]
files_names <- str_sub(files_nibrs, start = 14) %>% 
  str_remove_all(., '_1.csv|data_imputed_|_1_weights.csv')
df_list <- files_nibrs %>%
  purrr::map(~ read_csv(here('data', 'nibrs', .x), col_types = cols()) %>%
   select(-matches('lasso|rf|nb|sl|nnet|logistic')) %>%
   select(#-fips, 
          -originating_agency_identifier) %>%
   filter(year >= 2006 & year <= 2015)
  ) %>%
  setNames(files_names)
df_list <- df_list %>% map(~ .x %>%
  mutate(wgt = 1 / mod_log) %>%
  select(-mod_log))


# load logistic regression models fitted on NCVS
data_code <- "0110"
logreg_ncvs <- read_rds(here('data', 'regmodels', 
     paste0('ncvs_1_', data_code,'_weightedlog.rds')))
logreg_ncvs_recipe <- read_rds(here('data', 'regmodels', glue('ncvs_1_{data_code}_weightedlog_recipe.rds')))


# estimate alpha ----

cli_h1("Estimate alpha")

design_matrix_list <- df_list %>% 
  purrr::imap( ~ prepare_df_for_reg(.x, .y, wgt = FALSE, interaction = FALSE))
coefs <- design_matrix_list %>% 
  purrr::map(~ glm(y ~ 0+.-wgt, data = . , family = binomial()) %>% tidy())

coefs_tab <- get_table_coef(coefs) %>%
  reorder_and_filter_rows()


# estimate q ----

cli_h1("Estimate q")

# estimate coefficients
coefsw_est <- df_list %>%
  purrr::imap(~ run_wgt_reg(df_original = .x, 
                            df_processed = prepare_df_for_reg(df = .x, 
                                                              offense_name = .y, 
                                                              wgt = TRUE,
                                                              interaction = FALSE), 
                                                              offense_name = .y))

# estimate variance
coefsw_var <- coefsw_est %>%
  map( ~ get_var_lrw(est = .x,
                      logreg_ncvs = logreg_ncvs,
                      logreg_ncvs_recipe = logreg_ncvs_recipe))

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

# compare estimates ----

cli_h2('Regression results')

print(xtable(reorder_and_filter_rows(coefs_tab), 
             align = c('l', 'l', rep('c', ncol(coefs_tab) -1))),
      include.rownames = FALSE,
      booktabs = TRUE,
      sanitize.text.function=function(x){x})

print(xtable(reorder_and_filter_rows(coefsw_tab), 
              align = c('l', 'l', rep('c', ncol(coefsw_tab) -1))),
      include.rownames = FALSE,
      booktabs = TRUE,
      sanitize.text.function=function(x){x})






