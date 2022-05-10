
options(future.globals.maxSize= 9999999999)

set.seed(32432)

# load packages ----
suppressMessages(library(tidymodels))
suppressMessages(library(vroom))
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(Matrix))
suppressMessages(library(cli))
suppressMessages(library(glue))
suppressMessages(library(xtable))
suppressMessages(library(furrr))

source(here('analysis', 'utils_regression.R'))

args <- commandArgs(trailingOnly = TRUE)
if(length(args)==0){
  #offense_sel <- 'aggravated assault'
  offense_sel <- 'simple assault'
  n_cores <- 50L
} else{
  offense_sel <- args[1]
  n_cores <- as.integer(args[2])
}
cat('\n Number of cores: ', n_cores, '\n')
cat('\n Offense selected: ', offense_sel, '\n')


plan(multicore, workers = as.integer(n_cores))

# load data ----

# load nibrs data
files_nibrs <- list.files(here("data", "nibrs"))
files_nibrs <- files_nibrs[grepl('imputed', files_nibrs) & !grepl('weights', files_nibrs)]
files_names <- str_sub(files_nibrs, start = 14) %>% 
  str_remove(., '_1.csv') %>% str_remove(., 'data_imputed_')
  names(files_nibrs) <- files_names
df_list <- files_nibrs[offense_sel] %>%
  purrr::map(~ vroom(here('data', 'nibrs', .x), col_types = cols()) %>%
   select(-matches('lasso|rf|mod_log|nb|sl|nnet|log')) %>%
   select(-fips) %>%
   filter(year >= 2006 & year <= 2015)
  ) %>%
  setNames(offense_sel)


# load logistic regression models fitted on NCVS
data_code <- "0110"
logreg_ncvs <- read_rds(here('data', 'regmodels', 
     paste0('ncvs_1_', data_code,'_weightedlog.rds')))
logreg_ncvs_recipe <- read_rds(here('data', 'regmodels', glue('ncvs_1_{data_code}_weightedlog_recipe.rds')))
logreg_ncvs_design <- read_rds(here('data', 'regmodels', glue('ncvs_1_{data_code}_design.rds')))

# not needed, but used if I don't want to compute the superlearner estimates in the nibrs_get file
df_list <- df_list %>%
  map(~ .x %>%
    bind_cols(
      tibble(pred_log = as.vector(survey:::predict.svyglm(logreg_ncvs, bake(logreg_ncvs_recipe, prepare_nibrs_for_reg(.x)), 
                                                          type = 'response', se = FALSE)))
    ))



# diagnostics ----

cli_h1("Model diagnostics")


get_grid_values <- function(x, method = "even", n_grid = 5, quant_grid = 0.1) {
  if (method == "even") {
    lb <- quantile(x, probs = 0.025)
    ub <- quantile(x, probs = 0.975)
    
    out <- seq(lb, ub, length = n_grid)
  } else if (method == "quantile") {
    out <- quantile(x, probs = seq(0.05, 0.95, by = quant_grid))
  }
  return(out)
}

get_est_under_rwgt <- function(varname, df, offense_name, B = 100) {

  cli_h2(glue('Variable: {varname}'))

  B_list <- as.list(1:B)
  names(B_list) <- 1:B

  df_processed <- prepare_df_for_reg(df = df, 
                                     offense_name = offense_name, 
                                     wgt = TRUE,
                                     interaction = FALSE)
  
  if (varname %in% c(
    'log_population_served',
    'age_of_victim',
    'age_of_offender',
    'officer_per_1000capita'
    
  )) {
    grid_values <- df_processed %>%
      select({{varname}}) %>%
      summarise_all(function(x) get_grid_values(x, method = "even"))
  } else {
    grid_values <- tibble(values = c(0, 1))
    colnames(grid_values) <- varname
  }
  #browser()
  
  centers <- pull(grid_values, !!varname) %>% unique()
  values <- pull(df_processed, !!varname)
  
  # assign observation to the closest bin center
  bins_assigned <- tibble(
    n_obs = 1:nrow(df_processed),
    bin = as.list(values) %>%
      purrr::map(~ which.min(abs(.x - centers))) %>%
      unlist()
  )
  
  cli_text('Bins assigned: {length(unique(bins_assigned$bin))}')

  # reweighting
  coefs <- tibble()
  for (bin_chosen in 1:length(centers)) {
    cli_text(glue('\n Bin chosen: {bin_chosen}, dimension: {nrow(df[bins_assigned$bin == bin_chosen, ])}'))
    if (length(unique(df[bins_assigned$bin == bin_chosen, ]$y)) > 1) {

      #df_rwgt <- df[bins_assigned$bin == bin_chosen,]
      df_processed_rwgt <- df_processed[bins_assigned$bin == bin_chosen,]
      
      coefs <- coefs %>%
        bind_rows(tibble(
          center = centers[bin_chosen],
          boot_out = 1:B %>% map(~ sample(x = 1:nrow(df_processed_rwgt), 
                                  size = pmin(nrow(df_processed_rwgt), 1e4), 
                                  replace = TRUE)) %>% 
                            furrr::future_map(~ run_wgt_reg(df_original = FALSE, # not needed
                                                                df_processed = df_processed_rwgt[.x,], 
                                                                offense_name = offense_name, 
                                                                wgt = TRUE, 
                                                                verbose = FALSE)$coefs,
          .progress = TRUE,
          .options = furrr_options(seed = 3223414))
        ))
    }
  }
  
  coefs
}

# variables for the reweighting
terms_to_rwgt <- c(
  "race_of_victim_white",
  "injury_serious.injury",
  "weapon_firearm",
  "is_multiple_offense_yes",
  "msa_city.of.msa",
  "log_population_served"
)

if(offense_sel == 'simple assault') terms_to_rwgt <- setdiff(terms_to_rwgt, c("injury_serious.injury", "weapon_firearm"))
# if(offense_sel == 'sex offense') terms_to_rwgt <- setdiff(terms_to_rwgt, c('offender_male', 'victim_male'))
names(terms_to_rwgt) <- unlist(terms_to_rwgt)

est_under_rwgt <- terms_to_rwgt %>% 
      map(~ get_est_under_rwgt(.x, df_list[[offense_sel]], offense_sel, B=100)) %>%
      bind_rows(.id = 'term_rwgt')


# store focal slope
w <- est_under_rwgt %>% unnest(boot_out) %>% 
  vroom_write(here('analysis', "regression-results", 
                   glue("reg_{offense_sel}_reweighting_logreg.csv")))



# plot focal slope ----

get_focal_slope <- function(est_rwgt, term_chosen, offense_chosen){
  mean_estimates <- est_rwgt %>% filter(term == term_chosen & offense == offense_chosen) %>% 
    rowwise() %>% mutate(term_rwgt = rename_vars(term_rwgt)) %>% ungroup %>% 
    group_by(offense, term_rwgt, center) %>% summarise(mean_boot = mean(estimate))
  est_rwgt %>%
    filter(term == term_chosen & offense == offense_chosen) %>%
    rowwise() %>% mutate(term_rwgt = rename_vars(term_rwgt)) %>% ungroup %>%
    ggplot(aes(center, estimate)) +
    geom_point(alpha = 0.8, col='grey') + theme_bw() +
    geom_point(data=mean_estimates, aes(center, mean_boot), shape = 15) +
    facet_wrap(~ factor(term_rwgt, levels = order_terms), ncol = 6, scale = 'free_x', labeller = label_wrap_gen(width=20)) + 
    ylim(-0.5,0.5) +
    #ylim(-1,1) +
    geom_hline(data=h_line, aes(yintercept = 0), linetype = 'dashed', col = 'blue', alpha = 0.3) + 
    ylab(paste("Coef. of offender's\n race (white=1)")) + xlab("Value of group's center") + 
    ggtitle(paste0("Focal slope diagnostics for ", tolower(offense_chosen))) + 
    theme(plot.title = element_text(size=15), axis.title.y = element_text(size=12), axis.title.x = element_text(size=12))
}

est_under_rwgt <- vroom(here('analysis', "regression-results", 
                       glue("reg_{offense_sel}_reweighting_logreg.csv")))
# order_terms <- intersect(terms_to_rwgt, 
#   est_under_rwgt$term_rwgt) %>% sapply(., rename_vars)
order_terms <- terms_to_rwgt %>% sapply(., rename_vars)
# horizontal line at 0
h_line <- tibble(term_rwgt = order_terms, estimate = 0)
term_chosen <- 'race_of_offender_white'
# get focal slope and save
p <- est_under_rwgt %>%
  mutate(offense = offense_sel) %>%
  get_focal_slope(., term_chosen, offense_sel)
ggsave(height = 2, width = 10, plot = p, filename = here('figures', paste0('Focalslope_', offense_sel, '.pdf')))





