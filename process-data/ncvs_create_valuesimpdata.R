
set.seed(21321321)


suppressMessages(library(tidymodels))
suppressMessages(library(here))
suppressMessages(library(mice))
suppressMessages(library(tidyverse))
suppressMessages(library(cli))


# ----
# 

args <- commandArgs(trailingOnly = TRUE)

if(length(args)==0){
  data_code <- '0110'
} else{
  data_code <- args[1]
  sink(file = here('log-files', paste0('ncvs_imputation_', data_code, '.log')))
}
filename <- here('data', 'ncvs', paste0('data_', data_code, '.csv'))
data_code <- str_split(data_code, '')[[1]]
if(!all(data_code=='1' | data_code == '0')) stop('Code not valid')

df <- read_csv(filename, col_types = cols()) %>%
  mutate_if(is.character, ~ as.factor(.x)) %>%
  mutate(year = as.factor(year))
glimpse(df)

# add imputation procedure
cat(paste0(paste0(rep('----', 5), collapse = ''), '\n Missing values:\n'))
round(colSums(is.na(df))/nrow(df),2) %>% sort() %>% data.frame()

m_df <- 1
vars_not_to_impute <- c('psu', 
  'stratum', 
  'reported_to_police',
  'ncvs_id_for_households', 
  'incident_weight_adjusted_for_series_crimes')
df_not_to_impute <- df %>%
  select(all_of(vars_not_to_impute))
df <- df %>% select(-all_of(vars_not_to_impute))
# from https://www.rdocumentation.org/packages/mice/versions/3.6.0/topics/mice
# pmm, predictive mean matching (numeric data)
# logreg, logistic regression imputation (binary data, factor with 2 levels)
# polyreg, polytomous regression imputation for unordered categorical data (factor > 2 levels)
# polr, proportional odds model for (ordered, > 2 levels)
cli_text('Imputation')
list_X_imputed <- mice(df,
                       m = m_df,
                       print = TRUE,
                       maxit = 20)
list_X_imputed$loggedEvents

list_df_imputed <- 1:m_df %>%
  map(~ complete(list_X_imputed, .) %>%
        bind_cols(df_not_to_impute))

if(sum(is.na(list_df_imputed[[1]]))>0) warning('There are NAs in the data!')

colSums(is.na(list_df_imputed[[1]]))/nrow(list_df_imputed[[1]]) %>%
  data.frame()


#----


w <- list_df_imputed %>%
  setNames(1:m_df) %>%
  purrr::imap(
    ~ write_csv(.x, here(
      "data", "ncvs",
      paste0(
        "data_imputed_", .y, "_", paste0(data_code, collapse = ''), ".csv"
      )
    ))
  )


warnings()