set.seed(24432)

# initial parameters ----
options(warn=-1)
args <- commandArgs(trailingOnly = TRUE)

suppressMessages(library(tidymodels))
suppressMessages(library(doParallel))
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(keras))
suppressMessages(library(discrim))


#keras::install_keras(method  = "conda", conda_python_version = "3.9", envname = "py3.9", tensorflow = "2.5.0")
use_condaenv("py3.9", required = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if(length(args)==0){
  data_code <- '0110'
  n_cores <- 50L
  fake_run <- TRUE#0
} else{
  data_code <- args[1]
  n_cores <- as.integer(args[2])
  fake_run <- ifelse(is.na(args[3]), TRUE, FALSE)
}
cat('\n Data code: ', data_code, '\n')
cat('\n Number of cores: ', n_cores, '\n')
cat('\n Fake run: ', fake_run, '\n')

# register cores
suppressMessages(cl <- makePSOCKcluster(n_cores))
suppressMessages(registerDoParallel(cl))

# split the data into training and test sets ----

df <- read_csv(here('data', 'ncvs', 
                 paste0('data_imputed_1_', data_code, '.csv')),
            col_types = cols()) %>%
  mutate(reported_to_police = ifelse(reported_to_police == 1, 'yes', 'no')) %>%
  dplyr::select(-ncvs_id_for_households)


df_explore_rows <- df %>%
  mutate(n_row = 1:n()) %>%
  group_by(year < 2010, crime_recode, reported_to_police) %>%
  sample_frac(1/5, replace = FALSE) %>%
  pull(n_row)
# dataset to use for the final model
df_final <- df[setdiff(1:nrow(df), df_explore_rows),]
# dataset for exploration
df_explore <- df[df_explore_rows,]

# get variable about sampling
sampling_info_vars <- c('psu', 'stratum', 'incident_weight_adjusted_for_series_crimes')
sampling_info_df <- df %>% dplyr::select(all_of(sampling_info_vars))
sampling_info_df_final <- sampling_info_df %>% dplyr::slice(-df_explore_rows)
sampling_info_df_explore <- sampling_info_df %>% dplyr::slice(df_explore_rows)

# metrics <- metric_set(roc_auc, accuracy)
# cv_folds <- vfold_cv(df_explore,
#                      v = 10,
#                      repeats = 1,
#                      strata = crime_recode)



get_best_performing_model <- 
  function(mod_to_fit,
           mod_recipe, 
           grid, 
           cv_folds, 
           metrics,
           formula,
           parallel_method='everything'){
    
             
    wkfl_mod <- 
      workflow() %>%
      add_recipe(mod_recipe) %>%
      add_model(mod_to_fit, 
                formula = formula)

    metrics <- yardstick::metric_set(roc_auc, accuracy)
    
    mod_fit <- tune_grid(
      wkfl_mod,
      resamples = cv_folds,
      grid = grid,
      metrics = metrics,
      control = control_grid(verbose = FALSE, 
                             save_pred = TRUE,
                             # https://tune.tidymodels.org/reference/control_grid.html
                             parallel_over = parallel_method))
    
    full_data <- cv_folds$splits[[1]]$data %>% 
      dplyr::select(incident_weight_adjusted_for_series_crimes, crime_recode)
    mod_fit_preds <- mod_fit %>%
      pull(.predictions) %>%
      map_dfr( ~.x %>% dplyr::select(.pred_yes, .config, reported_to_police, .row)) 

  mod_fit_preds <- mod_fit_preds %>%
      bind_cols(full_data[mod_fit_preds$.row,])
  
  mod_fit_preds_by_crime <- mod_fit_preds %>%
    group_by(.config, crime_recode) %>%
    summarise(
      auc = glmnet:::auc(y = reported_to_police, 
                         prob = .pred_yes, 
                         w = incident_weight_adjusted_for_series_crimes)
    )
  
  # get best model
  avg_auc_by_model <- mod_fit_preds_by_crime %>% ungroup %>%
    group_by(.config) %>%
    summarise(auc = mean(auc)) %>%
    bind_cols(grid)
  #print(avg_auc_by_model)
    
  # get the parameters linked to this model
  mod_best <- tibble(
    .config = (avg_auc_by_model %>% arrange(desc(auc)))[1,] %>% pull(.config)
  ) %>%
    mutate(row = parse_number(str_sub(.config, -4, -1))) %>%
    inner_join(grid %>% mutate(row = 1:n()), by = 'row') %>%
    dplyr::select(-row)
    
    # extracts best model
    mod_best_wkfl <- wkfl_mod %>% 
      finalize_workflow(mod_best[1,])
    
    list(
      mod_fit = mod_fit,
      mod_best_wkfl = mod_best_wkfl
    )
  }



cv_folds <- vfold_cv(df_explore,
                     v = ifelse(fake_run, 2, 10),
                     repeats = 1,
                     strata = 'reported_to_police')

# resample the observations in the training set
for(i in 1:length(cv_folds$splits)){
  index_train <- cv_folds[[1]][[i]]$in_id
  weights <- df_explore %>% dplyr::slice(index_train) %>% pull(incident_weight_adjusted_for_series_crimes) # check this
  cv_folds[['splits']][[i]]$in_id <- c(index_train, # to avoid that some observations won't get sampled 
                                      sample(index_train, 
                                      5e4, 
                                      prob = weights, replace = TRUE))
  }



# single layer nnet ----

print("mlp")


df_recipe_nnet <- recipe(reported_to_police ~ 0+., data = df)  %>%
  step_rm(year) %>%
  step_rm(all_of(c('psu', 'stratum', 'incident_weight_adjusted_for_series_crimes'))) %>%
  step_mutate(age_of_victim = pmin(age_of_victim, 60)) %>%
  step_dummy(crime_recode, one_hot = TRUE) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  #step_interact(~ crime_recode_sex.offense:matches('^(?!crime|reported).+', perl = TRUE)) %>%
  step_nzv(all_predictors(), freq_cut = 999)

nnet <- get_best_performing_model(
  mlp(
    mode = "classification",
    hidden_units = tune(),#64,
    penalty = 1e-3,
    dropout = NULL,
    epochs = ifelse(fake_run, 1, 30),
    activation = 'softmax'
  ) %>%
  set_engine('keras', verbose = TRUE) %>%#, verbose = 0) %>%
  translate(),
  df_recipe_nnet,
  grid_random(
    parameters(hidden_units(range = c(32, 128))),
    #epochs(range = c(20, 80))),
    size = ifelse(fake_run, 2, 5)
    ),
  cv_folds = cv_folds,
  metrics = metrics,
  formula = as.formula('reported_to_police ~ .')
)


# random forest ----

print('random forest')


df_recipe <- recipe(reported_to_police ~ 0+., data = df) %>%
  step_rm(all_of(c('psu', 'stratum', 'incident_weight_adjusted_for_series_crimes')))


rf <- get_best_performing_model(
  
  rand_forest(mtry = tune(), trees = tune(), min_n = 10) %>% 
    set_engine("ranger", importance = "impurity") %>%#, num.threads = n_cores) %>% 
    set_mode("classification") %>%
    translate(),
  df_recipe,
  grid_regular(
    mtry(range = c(3,10)),        
    trees(range = c(50, 1000)),
    levels = c(mtry = ifelse(fake_run, 1, 3),trees = ifelse(fake_run, 1, 15))),
  cv_folds = cv_folds,
  formula = as.formula('reported_to_police ~ .')
)


# naive bayes -----

naivebayes <- get_best_performing_model(
  naive_Bayes(
  smoothness = tune(),
  Laplace = tune()
  ) %>% 
  set_engine('naivebayes') %>%
  set_mode('classification'),
  df_recipe,
  grid_regular(parameters(smoothness(),  Laplace()), size = 30),
  cv_folds = cv_folds,
  metrics = metrics,
  formula = as.formula('reported_to_police ~ .')
)
   

# logistic regression ----

# define recipe
df_recipe_linear <- recipe(reported_to_police ~ 0+., data = df)  %>%
  step_rm(year) %>%
  step_rm(all_of(c('psu', 'stratum', 'incident_weight_adjusted_for_series_crimes')), type_sex_offense) %>%
  step_mutate(age_of_victim = pmin(age_of_victim, 60)) %>%
  step_mutate(age_of_victim2 = age_of_victim^2,
              age_of_victim3 = age_of_victim^3) %>%
  step_dummy(crime_recode, one_hot = TRUE) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_interact(~ crime_recode_sex.offense:matches('^(?!crime|reported).+', perl = TRUE)) %>%
  step_nzv(all_predictors(), freq_cut = 999)


print('logistic')

logistic <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

logistic <- 
  workflow() %>%
  add_recipe(df_recipe_linear) %>%
  add_model(logistic, formula = as.formula('reported_to_police ~ .'))




# lasso ----

print('lasso')

lasso <- get_best_performing_model(
  
  logistic_reg(penalty = tune(), mixture = 1) %>% # lasso
    set_engine("glmnet") %>%
    set_mode("classification") %>%
    translate(),
  df_recipe_linear,
  grid_regular(parameters(penalty()), levels = ifelse(fake_run, 2, 100)),
  cv_folds = cv_folds,
  metrics = metrics,
  formula = as.formula('reported_to_police ~ .'),
  parallel_method = 'everything'
)



# train meta learner ----

get_predictions <- function(mod_to_fit, df_train, df_test){

  mod_fit <- mod_to_fit %>%
    fit(data = df_train)
  
  predict(mod_fit, df_test, type = 'prob') %>%
    dplyr::select(.pred_yes) %>%
    bind_cols(df_test)
  
}

get_cv_predictions <- function(mod_to_fit, cv_folds, metrics){
  cat('\n* Evaluating model')
  cv_folds$splits %>%
    map(
      ~ get_predictions(mod_to_fit, analysis(.x), assessment(.x))
    ) %>%
    bind_rows()
  
}

list_models <- list(
  lasso$mod_best_wkfl,
  rf$mod_best_wkfl,
  nnet$mod_best_wkfl,
  logistic,
  naivebayes$mod_best_wkfl
)
names(list_models) <- c('mod_lasso', 'mod_rf', 'mod_nnet', 'mod_logistic', 'mod_nb')

preds <- list_models %>%
  map(~ get_cv_predictions(.x,cv_folds))
preds_tb <- preds %>%
  map_dfr(~ .x, .id = 'model') %>%
  group_by(model) %>% mutate(nrow = 1:n()) %>% ungroup %>% # some observations are identical
  pivot_wider(names_from = model, values_from = .pred_yes) %>%
  mutate(reported_to_police = ifelse(reported_to_police == 'yes', 1, 0))

# look at models performance
preds_tb %>%
  pivot_longer(cols = matches('mod_'), names_to = 'model', values_to = 'pred') %>%
  group_by(model, crime_recode) %>%
  summarise(
    avg_auc = glmnet:::auc(
      y = reported_to_police,
      w = incident_weight_adjusted_for_series_crimes,
      prob = pred
    )
  ) %>%
  pivot_wider(names_from = model, values_from = avg_auc)

preds_eval_info <- preds_tb %>%
  dplyr::select(reported_to_police, crime_recode, incident_weight_adjusted_for_series_crimes)

# generate grid of weights
grid_max <- ifelse(fake_run, 10, 1e4)
grid_weights <- 1:grid_max %>%
  map(~ runif(length(list_models), 0, 1)) %>%
  map(~ .x/sum(.x)) %>%
  map(~ as_tibble(t(.x)) %>%
        setNames(names(list_models))) %>%
        bind_rows(.id = 'n_weights') %>%
        nest(params  = all_of(names(list_models)))

preds_matrix <- dplyr::select(preds_tb, all_of(names(grid_weights$params[[1]])))
auc_by_grid_weights <- grid_weights %>%
  mutate(preds = map(params, ~ as.matrix(c(.x) * preds_matrix) %>% rowSums())) %>%
  mutate(eval = map(preds, ~ tibble(preds = .x) %>% bind_cols(
    w = preds_tb %>% pull(incident_weight_adjusted_for_series_crimes),
    y = preds_tb %>% pull(reported_to_police),
    crime_recode = preds_tb %>% pull(crime_recode)
  ) %>%#
  group_by(crime_recode) %>%
    summarise(
      auc = glmnet:::auc(
        y = y,
        w = w,
        prob = preds
      )
    ) %>%
    ungroup %>%
    summarise(avg_auc = mean(auc))
  )) %>%
  unnest(eval)



best_weights <- auc_by_grid_weights %>%
filter(avg_auc == max(avg_auc)) %>% 
inner_join(grid_weights) %>%
unnest(params) %>%
dplyr::select(-n_weights, -preds, -avg_auc)

 
# save ----

# reweight data
weights <- df_final %>% pull(incident_weight_adjusted_for_series_crimes) # check this
df_final_resampled <- df_final[sample(1:nrow(df_final), 
                                      ifelse(fake_run, 1e3, 5e4), 
                                      prob = weights, replace = TRUE),]

# fit models
list_models_fit <- list_models %>%
  map(~ fit(.x, df_final_resampled))

# save models
list(models = list_models_fit, 
     weights = best_weights) %>%
  write_rds(., here('data', 'regmodels', 
                          paste0('ncvs_1_', data_code,'_superlearner.rds')))
  

# save keras separately because it doesn't get saved in the rds
save_model_hdf5(list_models_fit$mod_nnet$fit$fit$fit, here('data', 'regmodels', paste0('ncvs_1_', data_code,'_superlearner_keras.hdf5')))
