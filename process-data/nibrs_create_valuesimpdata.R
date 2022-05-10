
set.seed(13213432)

suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(mice))
suppressMessages(library(cli))


args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  offense_input <- "aggravated assault"
  multiple <- FALSE
} else {
  offense_input <- args[1]
  print(args[2])
  multiple <- ifelse(is.na(args[2]), FALSE, TRUE)
}
cli_text("\n Offense type selected: ", offense_input, "\n")
cli_text("\n Multiple offenders: ", ifelse(multiple, 'yes', 'no'), "\n")

# ----


write_imputed_df <- function(df, offense_sel){

  df <- df %>%
    # drop covariates that are not of interest
    select(-matches('cleared_exceptionally|city_name|total_off|total_empl')) %>%
    mutate_if(is.character, ~ as.factor(.x)) %>%
    select(-matches('crime_type|population_group'))  %>%
    filter(age_of_victim >= 12)

  y <- df$y
  # variables with only one value
  var_onlyoneval <- colnames(df)[apply(df, 2, function(x) length(unique(x)))==1]
  var_onlyoneval <- c(var_onlyoneval, 'fips', 'state_abbv', 'year', 'incident_number', 'originating_agency_identifier')
  X_onlyoneval <- df %>% select(-y) %>% select(matches(var_onlyoneval), region)
  X <- df %>% select(-y) %>%
    select(-matches(var_onlyoneval), -region) %>% droplevels()
  
  
  # show missing values
  print('* Check missing values:')
  nas <- colSums(is.na(X))/nrow(X)
  print(sort(round(nas[which(nas>0)],3)) %>% rev())
  
  
  # imputation procedure
  # alternative package: https://github.com/FarrellDay/miceRanger
  # see also: # https://stats.stackexchange.com/questions/99334/fast-missing-data-imputation-in-r-for-big-data-that-is-more-sophisticated-than-s
  m_df <- 1
  X_imputed <- mice(X,
                     m = m_df,
                     defaultmethod=c('logreg', 'polr', 'pmm', 'polyreg', 'polr'),
                     print = TRUE,
                    maxit = 10 
                    )
  X_imputed$loggedEvents
  
  # experiment with miceRanger
  # cl <- makeCluster(30)
  # registerDoParallel(cl)
  # miceObjPar <- miceRanger(
  #   X
  #   , m=m_df
  #   , parallel = TRUE
  #   , verbose = TRUE
  #   , maxiter = 15
  # )
  
  list_X_imputed <- list(complete(X_imputed))
  
  ## transformed features
  colnames(list_X_imputed[[1]])
  # check absence of NAs
  print('* Check missing values:')
  print(colSums(is.na(X_imputed %>% complete(.,1))))
  
  # drop constants
  list_X_imputed <- list_X_imputed %>% 
    purrr::map(~ Filter(function(x)(length(unique(x))>1), .x))
  
  list_X_imputed <- list_X_imputed %>% purrr::map( ~ .x %>% 
                                                     add_column(y = y) %>% 
                                                     bind_cols(X_onlyoneval)
                                                     )
  
  df_1 <- list_X_imputed[[1]] %>% select(-any_of(c('fips')))
  if(sum(is.na(df_1))>0) warning('There are NAs in the data!')
  
  list_X_imputed %>%
    setNames(1:m_df) %>%
    purrr::imap(~ write_csv(.x, here('data', 'nibrs', paste0('data_imputed_', offense_sel, '_', .y, ifelse(multiple, '_multiple', ''), '.csv'))))
}


# ----

files <- list.files(path=here('data', 'nibrs'), pattern="*.csv")
files <- files[grepl('data_', files) & !grepl('imputed', files)]

files <- files[grepl(offense_input, files)]
if(multiple){
  files <- files[grepl('multiple', files)]
} else{
  files <- files[!grepl('multiple', files)]
}

# load files
list_df <- files %>%
  purrr::map(~read_csv(here('data', 'nibrs', .x),col_types = cols()))
names_df <- str_replace(word(files, 2, sep = "_"), '.csv', '')


# filter data of interest
map2(list_df,
     names_df,
     ~ write_imputed_df(.x, .y))
