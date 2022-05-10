
set.seed(2444)

suppressMessages(library(tidymodels))
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(survey))
suppressMessages(library(glue))
suppressMessages(library(glmnet))
suppressMessages(library(gridExtra))
suppressMessages(library(xtable))
source(here('analysis', 'utils_regression.R'))

# initial parameters ----

args <- commandArgs(trailingOnly = TRUE)
if(length(args)==0){
  data_code <- '0110'#'0010'
} else{
  data_code <- args[1]
}
cat('\n Data code: ', data_code, '\n')

# split the data into training and test sets ----
df <- read_csv(here('data', 'ncvs', 
                 paste0('data_imputed_1_', data_code, '.csv')),
            col_types = cols()) %>%
  select(-ncvs_id_for_households)

# create year group variable
# See here https://bjs.ojp.gov/sites/g/files/xyckuh236/files/media/document/ncvs_variance_user_guide_11.06.14.pdf
df <- df %>%
  mutate(year_group = case_when(
    year <= 2005 ~ '1997–2005',
    year <= 2015 ~ '2006-2015',
    year > 2015 ~ '2016+'
  )) %>%
  mutate(psu = ifelse(year == 2016, 3, psu),
          stratum = ifelse(year == 2016, 1000, stratum))

# use survey design
# to understand how to deal with self-representing PSUs ("certainty")
# https://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
# options(survey.lonely.psu="certainty")
# pseudo PSUs solve the issue
levels_reported_to_police <- c('no', 'yes')

df_explore_rows <- df %>%
  mutate(n_row = 1:n()) %>%
  group_by(year < 2010, crime_recode, reported_to_police) %>%
  sample_frac(1/5, replace = FALSE) %>% 
  pull(n_row)
# shuffle rows
df_explore_rows <- sample(df_explore_rows, length(df_explore_rows), replace = FALSE)
# dataset to use for the final model
df_final <- df[setdiff(1:nrow(df), df_explore_rows),]
# dataset for exploration
df_explore <- df[df_explore_rows,]

# get variable about sampling
sampling_info_vars <- c('psu', 'stratum', 'incident_weight_adjusted_for_series_crimes', 'year_group')
sampling_info_df <- df %>% select(all_of(sampling_info_vars))
sampling_info_df_final <- sampling_info_df %>% dplyr::slice(-df_explore_rows)
sampling_info_df_explore <- sampling_info_df %>% dplyr::slice(df_explore_rows)


# assess models ----

## define models ----

# baseline
df_mod1 <- recipe(reported_to_police ~ 0+., data = df)  %>%
  step_rm(all_of(sampling_info_vars), type_sex_offense) %>%
  step_mutate(age_of_victim = pmin(age_of_victim, 60)) %>%
  step_dummy(crime_recode, one_hot = FALSE) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_interact(~ race_of_offender_white:race_of_victim_white) %>%
  step_nzv(all_predictors(), freq_cut = 999)

# baseline with age polynomial terms
df_mod2 <- recipe(reported_to_police ~ 0+., data = df)  %>%
  step_rm(all_of(sampling_info_vars), type_sex_offense) %>%
  step_mutate(age_of_victim = pmin(age_of_victim, 60)) %>%
  step_mutate(age_of_victim2 = age_of_victim^2,
              age_of_victim3 = age_of_victim^3) %>%
  step_dummy(crime_recode, one_hot = TRUE) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_interact(~ race_of_offender_white:race_of_victim_white) %>%
  step_nzv(all_predictors(), freq_cut = 999) %>%
  step_rm(crime_recode_simple.assault)

# year excluded
df_mod3 <- recipe(reported_to_police ~ 0+., data = df)  %>%
  step_rm(year) %>%
  step_rm(all_of(sampling_info_vars), type_sex_offense) %>%
  step_mutate(age_of_victim = pmin(age_of_victim, 60)) %>%
  step_dummy(crime_recode, one_hot = FALSE) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_interact(~ race_of_offender_white:race_of_victim_white) %>%
  step_nzv(all_predictors(), freq_cut = 999)

# interactions for sex offenses
df_mod4 <- recipe(reported_to_police ~ 0+., data = df)  %>%
  step_rm(year) %>%
  step_rm(all_of(sampling_info_vars), type_sex_offense) %>%
  step_mutate(age_of_victim = pmin(age_of_victim, 60)) %>%
  step_dummy(crime_recode, one_hot = TRUE) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_interact(~ race_of_offender_white:race_of_victim_white) %>%
  step_interact(~ crime_recode_sex.offense:matches('^(?!crime|reported).+', perl = TRUE)) %>%
  step_nzv(all_predictors(), freq_cut = 999)

# interactions for robbery and sex offenses
df_mod5 <- recipe(reported_to_police ~ 0+., data = df)  %>%
  step_rm(year) %>%
  step_rm(all_of(sampling_info_vars), type_sex_offense) %>%
  step_factor2string(crime_recode) %>%
  step_mutate(crime_recode = case_when(
    crime_recode == 'simple assault' | crime_recode == 'aggravated assault' ~ 'assault',
    TRUE ~ crime_recode
  )) %>%
  step_string2factor(crime_recode) %>%
  step_mutate(age_of_victim = pmin(age_of_victim, 60)) %>%
  step_dummy(crime_recode, one_hot = TRUE) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_interact(~ crime_recode_sex.offense:matches('^(?!crime|reported).+', perl = TRUE)) %>%
  step_interact(~ crime_recode_robbery:matches('^(?!crime|reported).+', perl = TRUE)) %>%
  step_interact(~ crime_recode_assault:matches('^(?!crime|reported).+', perl = TRUE)) %>%
  step_rm(!matches('^(crime|reported).+', perl = TRUE)) %>%
  step_nzv(all_predictors(), freq_cut = 999)

# interactions for all crimes
df_mod6 <- recipe(reported_to_police ~ 0+., data = df)  %>%
  step_rm(year) %>%
  step_rm(all_of(sampling_info_vars), type_sex_offense) %>%
  step_mutate(age_of_victim = pmin(age_of_victim, 60)) %>%
  step_dummy(crime_recode, one_hot = TRUE) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_interact(~ race_of_offender_white:race_of_victim_white) %>%
  step_interact(~ crime_recode_robbery:matches('^(?!crime|reported).+', perl = TRUE)) %>%
  step_interact(~ crime_recode_sex.offense:matches('^(?!crime|reported).+', perl = TRUE)) %>%
  step_interact(~ crime_recode_simple.assault:matches('^(?!crime|reported).+', perl = TRUE)) %>%
  step_interact(~ crime_recode_aggravated.assault:matches('^(?!crime|reported).+', perl = TRUE)) %>%
  step_rm(!matches('^(crime|reported).+', perl = TRUE)) %>%
  step_nzv(all_predictors(), freq_cut = 999)

# interaction for type of sex offense
df_mod7 <- recipe(reported_to_police ~ 0+., data = df)  %>%
  step_rm(all_of(sampling_info_vars)) %>%
  step_mutate(age_of_victim = pmin(age_of_victim, 60)) %>%
  step_dummy(crime_recode, one_hot = TRUE) %>%
  step_dummy(type_sex_offense, one_hot = TRUE) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_interact(~ race_of_offender_white:race_of_victim_white) %>%
  step_interact(~ type_sex_offense_rape:matches('^(?!crime|reported).+', perl = TRUE)) %>%
  step_interact(~ type_sex_offense_other:matches('^(?!crime|reported).+', perl = TRUE)) %>%
  step_interact(~ type_sex_offense_sexual.assault:matches('^(?!crime|reported).+', perl = TRUE)) %>%
  step_nzv(all_predictors(), freq_cut = 999)


# function for model fitting
get_mod_fit <- function(df, rows_train, sampling_info, crime_recode){
  #browser()
  # df <- df %>%
  #   mutate(reported_to_police = ifelse(reported_to_police == 'yes', 1, 0))
  # this could be placed outside of the loop
  design_mod <- survey::svydesign(id=~psu,
                                  strat=~stratum+year_group, 
                                  weight=~incident_weight_adjusted_for_series_crimes, 
                                  data = df %>%
                                    bind_cols(sampling_info) %>%
                                    slice(rows_train) %>%
                                    mutate(stratum = 1), # fake strata just for model fitting
                                  # here we are not interested in the variance of the estimates
                                  nest = TRUE)
  formula_mod <- glue('reported_to_police ~',
                      '{paste0(setdiff(colnames(df), "reported_to_police"), collapse = "+")}')
  mod_fit <- svyglm(as.formula(formula_mod), 
         design = design_mod,
         family = quasibinomial())
  X <- as.matrix(df %>% slice(-rows_train) %>% select(any_of(names(coef(mod_fit)))))
  if(!all(names(coef(mod_fit))[-1]==colnames(X))) print('error')
  X <- X %>% cbind(1,.)
  linear_pred <- X %*% coef(mod_fit)
  preds <- 1/(1 + exp(-linear_pred))

  tibble(y = df %>% slice(-rows_train) %>% pull(reported_to_police), 
         weight = sampling_info %>% slice(-rows_train) %>% pull(incident_weight_adjusted_for_series_crimes),
         preds = preds,
         mod_fit = list(mod_fit),
         rows = setdiff(1:nrow(df), rows_train)
  )
}


train_folds <- df_explore %>%
  vfold_cv(., v = 20, repeats = 1, strata = crime_recode) %>%
  pull(splits) %>%
  map(~ .x$in_id)



cross_list_df <- cross2(
  list(
  mod1 = bake(prep(df_mod1), df_explore),
  mod2 = bake(prep(df_mod2), df_explore),
  mod3 = bake(prep(df_mod3), df_explore),
  mod4 = bake(prep(df_mod4), df_explore),
  mod5 = bake(prep(df_mod5), df_explore),
  mod6 = bake(prep(df_mod6), df_explore),
  mod7 = bake(prep(df_mod7), df_explore)
  ),
  list(
    as.list(train_folds)
  ))

mod_fit <- cross_list_df %>%
  map( ~ map_dfr(.x[[2]], ~ get_mod_fit(df = .y[[1]], 
                                        rows_train = .x, 
                                        sampling_info = sampling_info_df_explore,
                                        crime_recode = df_explore %>% pull(crime_recode)), .y=.x))


## evaluate models ----

# auc
mod_fit %>%
  map_dfr(~ .x %>% summarise(
    auc = glmnet:::auc(y = y, 
                     prob = preds, 
                     w = weight)),
          .id = 'model') %>%
  pivot_wider(names_from = model, values_from = auc)

# auc by crime type
mod_fit %>%
  map_dfr(~ .x %>% inner_join(df_explore %>% 
                                mutate(rows = 1:n()) %>%
                                select(rows, crime_recode),
                              by = 'rows') %>%
            group_by(crime_recode) %>%
        summarise(auc = glmnet:::auc(y = y, 
                     prob = preds, 
                     w = weight)),
        .id = 'model') %>%
  pivot_wider(names_from = model, values_from = auc)

# accuracy
mod_fit %>%
  map_dfr(~ .x %>% inner_join(df_explore %>% 
                                mutate(rows = 1:n()) %>%
                                select(rows, crime_recode),
                              by = 'rows') %>% group_by(crime_recode) %>%
            summarise(acc = sum(ifelse(y==ifelse(preds>0.5,1,0), 1, 0)*weight)/sum(weight)),
          .id = 'model') %>%
  pivot_wider(names_from = model, values_from = acc)


# evaluate calibration
p <- mod_fit %>%
  map_dfr(~ bind_rows(.x),
          .id = 'model') %>%
  mutate(preds = pmin(floor(preds*10)/10, 0.9)) %>%
  group_by(model, preds) %>%
  summarise(y = mean(y)) %>%
  ggplot(aes(preds, y, fill = model)) + 
  geom_col(position = 'dodge2') + 
  theme_bw()
p
# calibration by crime type
p <- mod_fit %>%
  map_dfr(~ bind_rows(.x %>% inner_join(df_explore %>% 
                                          mutate(rows = 1:n()) %>%
                                          select(rows, crime_recode),
                                        by = 'rows')),
          .id = 'model') %>%
  mutate(preds = pmin(floor(preds*10)/10, 0.9)) %>%
  group_by(model, crime_recode, preds) %>%
  summarise(y = mean(y)) %>%
  ggplot(aes(preds, y, fill = model)) + 
  geom_col(position = 'dodge2') + 
  theme_bw() + 
  facet_wrap(~ crime_recode)
p


# evaluate ----

## setup

# create design matrix for final model
# & train model on the rest of the sample
df_recipe <- recipe(reported_to_police ~ 0+., data = df)  %>%
  step_rm(year) %>%
  step_rm(all_of(sampling_info_vars), type_sex_offense) %>%
  step_mutate(age_of_victim = pmin(age_of_victim, 60)) %>%
  step_dummy(injury, one_hot = TRUE) %>% step_rm(injury_no) %>%
  step_dummy(weapon, one_hot = TRUE) %>% step_rm(weapon_no) %>%
  step_dummy(relationship_to_offender, one_hot = TRUE) %>% step_rm(relationship_to_offender_stranger) %>%
  step_dummy(msa, one_hot = TRUE) %>% step_rm(msa_not.msa) %>%
  step_dummy(crime_recode, one_hot = FALSE) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_nzv(all_predictors(), freq_cut = 999)


df_final_4reg <- bake(prep(df_recipe), df_final)
# colnames(df_final_4reg)

# save prepped recipe
write_rds(prep(df_recipe), here('data', 'regmodels', 
     paste0('ncvs_1_', data_code,'_weightedlog_recipe.rds')))


# # note that the function should take in a daset that contains *all* the information
# # about the survey design (i.e., all the original strata and PSUs). Here we do not 
# # really do that because that would force us to track the individual observations
# # since the beginning.
# # To incorporate the information about the sampling design we can do the following
# see https://pubmed.ncbi.nlm.nih.gov/8659480/
fake_df <- data.frame(
  year_group = rep(c("1997–2005", "2006-2015", "2016+"), each = 160 * 2),
  stratum = rep(rep(1:160, each = 2), 3),
  psu = rep(c(1, 2), 160 * 3),
  incident_weight_adjusted_for_series_crimes = 1
)
dfs <- survey::svydesign(id=~psu, 
                         strata=~year_stratum, 
                         weight=~incident_weight_adjusted_for_series_crimes, 
                         data = df_final_4reg %>%
                           bind_cols(sampling_info_df_final) %>%
                           mutate(real = 1) %>%
                           mutate(psu = as.numeric(psu),
                           stratum = as.numeric(stratum)) %>%
                           bind_rows(
                             df_final_4reg[1,] %>%
                               dplyr::slice(rep(1, each = 160 * 2 * 3)) %>%
                               bind_cols(
                                 fake_df
                             ) %>%
                                 mutate(real = 0)
                           ) %>%
                           mutate(year_stratum = glue('{stratum}_{year_group}')) %>%
                           select(-stratum, -year_group)#,
                         ,nest = TRUE
                         ) %>%
  # subset 
  survey:::subset.survey.design(., real>0)

# write_rds(dfs, here('data', 'regmodels', 
#      paste0('ncvs_1_', data_code,'_design.rds')))

options(survey.lonely.psu="certainty")
X_formula <- paste0(setdiff(colnames(df_final_4reg), c('reported_to_police')), collapse = '+')
formula_reg <- glue('reported_to_police ~  {X_formula}')
mod_fit <- svyglm(as.formula(formula_reg), design = dfs, family = quasibinomial())
write_rds(mod_fit, here('data', 'regmodels', 
     paste0('ncvs_1_', data_code,'_weightedlog.rds')))


# get AUC on final dataset (overfitting)
X <- as.matrix(df_final_4reg %>% select(any_of(names(coef(mod_fit)))))
X <- X %>% cbind(1,.)
linear_pred <- X %*% coef(mod_fit)
tibble(
  preds = 1/(1 + exp(-linear_pred)),
  crime_recode = df_final %>% pull(crime_recode),
  reported_to_police = df_final %>% pull(reported_to_police),
  weight = df_final %>% pull(incident_weight_adjusted_for_series_crimes)
) %>%
group_by(crime_recode) %>%
summarise(auc = glmnet:::auc(
  y = reported_to_police,
  prob = preds,
  w = weight
)) 


coefs_tab <- get_table_coef(list(sex = tidy(mod_fit))) %>%
  reorder_and_filter_rows()

print(xtable(reorder_and_filter_rows(coefs_tab), 
             align = c('l', 'l', rep('c', ncol(coefs_tab) -1))),
      include.rownames = FALSE,
      booktabs = TRUE,
      sanitize.text.function=function(x){x})


 # evaluate the model -----

df_folds <- df_final %>%
  vfold_cv(., v = 20, repeats = 1, strata = crime_recode) %>%
  pull(splits) %>%
  map(~ .x$in_id)

cross_list_df <- cross2(
  list(
  mod = juice(prep(df_recipe))
  ),
  list(
    as.list(df_folds)
  ))

mod_fit <- cross_list_df %>%
  map( ~ map_dfr(.x[[2]], ~ get_mod_fit(df = .y[[1]], 
                                        rows_train = .x, 
                                        sampling_info = sampling_info_df,
                                        crime_recode = df_explore %>% pull(crime_recode)), .y=.x))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73")
crimes_levels <- c('sex offense', 'robbery', 'aggravated assault', 'simple assault')
get_boot_ci_auc <- function(x, preds, wgt, n_iter = 1e4){
  #browser()
  if(!wgt[1]){wgt <- rep(1, length(x))}
  rows_resample <- 1:n_iter %>%
    map(~ sample(1:length(x), length(x), n_iter))
  x_out <- rows_resample %>%
    map(~ glmnet:::auc(y = x[.x], 
                       prob = preds[.x], 
                       w = wgt[.x])) %>% unlist()
  tibble(mean_x = mean(x_out), 
    lb = quantile(x_out, 0.025), 
    ub = quantile(x_out, 0.975))
}
ci_auc <- mod_fit %>%
  map_dfr(~ .x %>% inner_join(df_explore %>% 
                                mutate(rows = 1:n()) %>%
                                select(rows, crime_recode),
                              by = 'rows') %>%
            group_by(crime_recode) %>%
            summarise(auc = glmnet:::auc(y = y, 
                     prob = preds[,1], 
                     w = weight), 
                  ci = list(get_boot_ci_auc(x = y, preds = preds[,1], wgt = weight))),
        .id = 'model') %>%
  unnest(ci)
p_auc <- ci_auc %>%
  ggplot(aes(x = factor(crime_recode, levels = crimes_levels), 
             y = auc, 
             fill = factor(crime_recode, levels = crimes_levels))) + 
  geom_col() + 
  scale_fill_manual('Crime type', values=cbPalette[1:4]) + 
  theme_bw() + 
  ylim(0,1) + 
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.1) +
  xlab('') + 
  ylab('Area under the curve (AUC)') + 
  scale_x_discrete(labels = c(
    'sex offense' = 'sex offense',
    'robbery' = 'robbery',
    'aggravated assault' = 'aggravated\n assault',
    'simple assault' = 'simple\n assault'
  ))
# p_auc %>%
#   ggsave(filename = here('figures', 'logistic_regression_auc_ncvs.pdf'),  
#          plot = .,
#          width = 7, height = 3)


# calibration
get_boot_ci_mean <- function(x, wgt=FALSE, n_iter = 1e4){
  if(!wgt[1]){wgt <- rep(1, length(x))}
  x_out <- 1:n_iter %>%
    map(~ mean(x[sample(1:length(x), length(x), n_iter, prob = wgt)])) %>%
    unlist()
  tibble(#mean_x = mean(x_out), 
           lb = quantile(x_out, 0.025), 
           ub = quantile(x_out, 0.975))
}
ci_cal <- mod_fit %>%
  map_dfr(~ .x %>% inner_join(df_explore %>% 
                                mutate(rows = 1:n()) %>%
                                select(rows, crime_recode),
                              by = 'rows') %>%
            mutate(pred_round = floor(pmin(preds[,1], 0.99) * 10)/10 + 0.05) %>%
            group_by(crime_recode, pred_round) %>%
            summarise(mean_y = sum(y * weight)/sum(weight),
                      ci = list(get_boot_ci_mean(y, wgt = weight))
                      ),
          .id = 'model') %>%
  unnest(ci)
p_cal <- ci_cal %>%
  filter(pred_round >= 0.1) %>%
  ggplot(aes(pred_round, mean_y, col = factor(crime_recode, levels = crimes_levels))) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lb, ymax = ub,
                  fill = factor(crime_recode, levels = crimes_levels)), 
              colour = NA,
              alpha = 0.2) + 
  theme_bw() + 
  scale_fill_manual('Crime type', values=cbPalette[1:4], guide = 'none') + 
  scale_color_manual('Crime type', values=cbPalette[1:4], guide = 'none') + 
  xlab('Estimated likelihood of police notification') + 
  ylab('Share of victimizations reported\n to law enforcement')
# p %>%
#   ggsave(filename = here('figures', 'logistic_regression_cal_ncvs.pdf'), 
#          height = 3, width = 8,
#          plot = .)
  
grid.arrange(p_cal, p_auc, widths=c(0.5, 0.5), nrow = 1) %>%
    ggsave(filename = here('figures', 'logistic_regression_calandauc_ncvs.pdf'),
           height = 3, width = 10,
           plot = .)



