
# tools for data processing ----

# age -> character
nibrs_age2char <- function(df){
  if('age_of_offender' %in% colnames(df)){
  df <- df %>% mutate(age_of_offender = case_when(
    age_of_offender < 12 ~ "<12",
    age_of_offender >= 12 & age_of_offender < 15 ~ "12-14",
    age_of_offender >= 15 & age_of_offender < 18 ~ "15-17",
    age_of_offender >= 18 & age_of_offender < 21 ~ "18-20",
    age_of_offender >= 21 & age_of_offender < 30 ~ "21-29",
    age_of_offender >= 30 ~ "30+"
  ))
  }

  df
}

# process nibrs for regression with NCVS data
prepare_nibrs_for_reg <- function(df){
  df <- df %>% mutate(age_of_offender = case_when(
    age_of_offender < 12 ~ "<12",
    age_of_offender >= 12 & age_of_offender < 15 ~ "12-14",
    age_of_offender >= 15 & age_of_offender < 18 ~ "15-17",
    age_of_offender >= 18 & age_of_offender < 21 ~ "18-20",
    age_of_offender >= 21 & age_of_offender < 30 ~ "21-29",
    age_of_offender >= 30 ~ "30+"
  ))

  df
}

# process nibrs for regression with NCVS-like data
prepare_nibrs_for_reg_multiple <- function(df){
  # browser()
  df <- lazy_dt(df)
  df <- df %>%
    group_by(year, originating_agency_identifier, incident_number) %>%
    mutate(
      multiple_offenders_age_of_youngest = min(age_of_offender),
      multiple_offenders_age_of_oldest = max(age_of_offender),
      race_of_offender = ifelse(
        length(unique(race_of_offender)) > 1, 'mix white_black',
        race_of_offender[1]
      ),
      sex_of_offender = ifelse(
        length(unique(sex_of_offender)) > 1, 'mix',
        sex_of_offender[1]
      ),
      n_within_crime = 1:n()
    ) %>% ungroup %>%
    filter(n_within_crime == 1) %>% select(-n_within_crime, -age_of_offender) %>%
    mutate(
      multiple_offenders_age_of_youngest = case_when(
          multiple_offenders_age_of_youngest < 12 ~ "<12",
          multiple_offenders_age_of_youngest >= 12 & multiple_offenders_age_of_youngest < 15 ~ "12-14",
          multiple_offenders_age_of_youngest >= 15 & multiple_offenders_age_of_youngest < 18 ~ "15-17",
          multiple_offenders_age_of_youngest >= 18 & multiple_offenders_age_of_youngest < 21 ~ "18-20",
          multiple_offenders_age_of_youngest >= 21 & multiple_offenders_age_of_youngest < 30 ~ "21-29",
          multiple_offenders_age_of_youngest >= 30 ~ "30+"
    ),
    multiple_offenders_age_of_oldest = case_when(
          multiple_offenders_age_of_oldest < 12 ~ "<12",
          multiple_offenders_age_of_oldest >= 12 & multiple_offenders_age_of_youngest < 15 ~ "12-14",
          multiple_offenders_age_of_oldest >= 15 & multiple_offenders_age_of_youngest < 18 ~ "15-17",
          multiple_offenders_age_of_oldest >= 18 & multiple_offenders_age_of_youngest < 21 ~ "18-20",
          multiple_offenders_age_of_oldest >= 21 & multiple_offenders_age_of_youngest < 30 ~ "21-29",
          multiple_offenders_age_of_oldest >= 30 ~ "30+"
    ))

  return(as_tibble(df))
}






# process nibrs data for regression
prepare_df_for_reg <- function(df, 
                               offense_name,
                               wgt = FALSE,
                               interaction = FALSE){
                                 
  #cli_li(glue('{offense_name}: Preparing dataset for regression'))


  if(offense_name == 'simple assault') df <- df %>% 
      mutate(injury = if_else(injury == 'serious injury', 
                              'minor injury', injury))
  
  # preprocess raw
  df <- df %>% 
    select(-any_of(c('region', 'fips', 'crime_recode'))) %>%
    select(-matches('lasso|rf|mod_log|nb|sl|nnet|log')) %>%
    mutate(year = as.factor(year))
  
  # remove factors with only one level
  df <- Filter(function(x)(length(unique(x))>1), df)
  
  # interaction for race
  if(interaction) df <- df %>%
    mutate(inter_race = case_when(
      race_of_victim == 'white' & race_of_offender == 'black' ~ 'white_vic_black_off',
      race_of_victim == 'black' & race_of_offender == 'black' ~ 'black_vic_black_off',
      race_of_victim == 'white' & race_of_offender == 'white' ~ 'white_vic_white_off',
      race_of_victim == 'black' & race_of_offender == 'white' ~ 'black_vic_white_off',
    )) %>%
    select(-race_of_offender, -race_of_victim)
  
  # preprocess with recipes
  df_recipe <- recipe(y ~ ., data = df) %>%
    step_intercept(value = 1) %>%
    
    step_dummy(weapon, one_hot = TRUE) %>% 
    step_rm(any_of(c('weapon_no'))) %>%
    
    step_dummy(msa, one_hot = TRUE) %>%
    step_rm(any_of(c('msa_not.msa'))) %>%
    
    step_dummy(injury, one_hot = TRUE) %>%
    step_rm(any_of(c('injury_no'))) %>%
    
    step_dummy(relationship_to_offender, one_hot = TRUE) %>%
    step_rm(any_of(c('relationship_to_offender_stranger'))) %>%

    step_mutate(log_population_served = log(population_served)) %>%
    step_rm(population_served) %>%
    
    step_dummy(all_nominal(), -all_outcomes())
  
  df <- juice(prep(df_recipe))
  
  # add/remove wgt
  if(!wgt){
    df <- df %>% mutate(wgt =  1)
  }
  

  # return processed data
  return(df)
}




prepare_df_for_reg_multiple <- function(df, 
                                        offense_name,
                                        wgt = FALSE){
  
  # cli_text(offense_name)

  # add/remove wgt
  if(!wgt){
    df <- df %>% mutate(wgt =  1)
  }
  if(offense_name == 'simple assault') df <- df %>% 
      mutate(injury = if_else(injury == 'serious injury', 
                              'minor injury', injury))
  
  # preprocess raw
  df <- df %>% 
    select(-any_of(c('region', 'fips', 'crime_recode', 'drinking_drugs', 'region', 'type_sex_offense',
                     'incident_number', 'originating_agency_identifier'))) %>%
    select(-matches('lasso|rf|mod_log|nb|sl|nnet|log')) %>%
    mutate(year = as.factor(year))
  
  if(offense_name == 'aggravated assault' | offense_name == 'simple assault'){
    df <- df %>% select(-any_of(c('is_offense_attempted', 'multiple_offender')))
  }
  
  
  # preprocess with recipes
  df_recipe <- recipe(y ~ ., data = df) %>%
    step_intercept(value = 1) %>%
    
    step_dummy(weapon, one_hot = TRUE) %>% 
    step_rm(any_of(c('weapon_no'))) %>%
    
    # step_dummy(multiple_offender, one_hot = TRUE) %>% 
    # step_rm(any_of(c('multiple_offender_no'))) %>%
    
    step_dummy(msa, one_hot = TRUE) %>%
    step_rm(any_of(c('msa_not.msa'))) %>%
    
    step_dummy(injury, one_hot = TRUE) %>%
    step_rm(any_of(c('injury_no'))) %>%
    
    step_dummy(is_multiple_offense, one_hot = TRUE) %>%
    step_rm(any_of(c('is_multiple_offense_no'))) %>%
    
    step_dummy(relationship_to_offender, one_hot = TRUE) %>%
    step_rm(any_of(c('relationship_to_offender_stranger'))) %>%
    
    step_mutate(log_population_served = log(population_served)) %>%
    step_rm(population_served) %>%
    
    step_dummy(all_nominal(), -all_outcomes())
  

  # return processed data
  return(juice(prep(df_recipe)))
  
}






# tools for regression ----

## logistic regression -----

# IWLS
fit_logreg_irls <- function(theta, X, Y, wgt = 1, eps = 1e-4, verbose = TRUE) {
  if(verbose) cli_li('Fitting regression')

  llk <- Inf
  llk_new <- 0
  while (abs(llk_new - llk) >= eps) {
    
    llk <- llk_new

    # cat('\n', as.vector(theta))
    mu <- sigmoid(theta, X)
    S_diag_wgt <- mu * (1 - mu) * wgt
    S_wgt <- Matrix::Diagonal(x = S_diag_wgt)
    z_wgt <- X %*% theta + Matrix::Diagonal(x = 1 / S_diag_wgt) %*% (Y - mu * wgt)
    theta <- Matrix::solve(t(X) %*% S_wgt %*% X, t(X)) %*% S_wgt %*% z_wgt
    
    llk_new <- comp_llk(theta = theta, X = X, Y = Y, wgt = wgt)
    
    if(verbose) cli_text("nlog-lkl: ", llk_new)
  }
  theta
}

# compute log likelihood with wgts
comp_llk <- function(theta, X, Y, wgt) {
  sum(Y * log(sigmoid(theta, X)) + (wgt - Y) * log(1 - sigmoid(theta, X)))
}

# compute logistic 
sigmoid <- function(theta, x) {
  as.vector(1 / (1 + exp(-x %*% theta)))
}

# fit with GD (deprecated)
# fit_logreg_gd <- function(theta, x, y, wgt = 1, eps = 1e-4) {
#   llk <- Inf
#   llk_new <- 0
#   while (abs(llk_new - llk) >= eps) {
#     llk <- llk_new
#     grad <- t(sigmoid(theta, x) / wgt - y) %*% x
#     theta <- as.vector(theta - 0.000001 * grad)
#     llk_new <- comp_llk(theta = theta, X = X, Y = Y, wgt = wgt)
#   }
#   theta
# }

# run regression
run_wgt_reg <- function(df_original, df_processed, offense_name, wgt = TRUE, verbose = TRUE){

  if(verbose) cli_li(glue("Regression for {offense_name}"))
  # drop unbalanced data
  df_recipe <- recipe(y ~ .,
              data = df_processed) %>%
              step_nzv(all_predictors(), -one_of(c('intercept')), 
              freq_cut = 99/1)
  #df_processed2 <- juice(prep(df_recipe))
  X = as.matrix(df_processed %>% dplyr::select(-any_of(c('y', 'wgt', 'incident_number', 'originating_agency_identifier'))))
  # make X full rank
  # from https://stackoverflow.com/questions/19100600/extract-maximal-set-of-independent-columns-from-a-matrix
  X = X[, qr(X)$pivot[seq_len(qr(X)$rank)]]
  Y = df_processed$y %>% as.numeric() # not necessary
  if(wgt){wgt = df_processed %>% pull(wgt)} else{wgt <- 1}
  theta = rep(0, ncol(X))
  
  # run regression
  coef <- fit_logreg_irls(
    theta = theta, # initialize regression with randomly drawn parameters - 
    # currently setting everything to 0
    X = X,
    Y = Y, 
    wgt = wgt, 
    eps = 1e-5, # when to stop
    verbose = verbose
    )
  
  coef <- tibble(term = colnames(X), estimate = as.vector(coef))
  
  return(list('offense' = offense_name, 
              'df_original' = df_original, 
              'X' = X, 
              'Y'= Y, 
              'wgt' = wgt, 
              'coefs' = coef))
}



get_var_lrw <- function(
      est, # result from regression
      logreg_ncvs, 
      logreg_ncvs_recipe) {
  
  offense_name <- pluck(est, "offense")
  wgt <- pluck(est, "wgt")
  X_original <- pluck(est, "df_original")
  X <- pluck(est, "X")
  Y <- pluck(est, "Y")
  theta <- pull(pluck(est, "coefs"), estimate)
  
  # cli_li("Variance computation for {offense_name}")

  # other part 
  
  # get design matrix for NIBRS into NCVS format
  nn <- nrow(X)
  nv <- length(logreg_ncvs$fitted.values)
  gamma <- coef(logreg_ncvs)
  X_nf <- bake(logreg_ncvs_recipe, 
               nibrs_age2char(X_original) %>% 
                 mutate(crime_recode = offense_name)) %>%
    as.matrix(.)
  if(length(gamma) > ncol(X_nf)) X_nf <- cbind(1, X_nf)

  # check coefficients names match
  if(!all(colnames(X_nf)[-1] == names(beta)[-1])){stop('Coefficients names do not match')}
  # remember that wgt = 1/pi(Z)
  q_pred <- sigmoid(theta, X)
  J_theta <- t(X) %*%
    Matrix::Diagonal(
      n = nn,
      x = q_pred * (1 - q_pred) * wgt
    ) %*%
    X / nn

  exp_gamma <- exp(- X_nf %*% c(gamma))
  J_gamma <- matrix(NA, 
                   nrow = ncol(X), 
                   ncol = ncol(X_nf))
  for(j in 1:ncol(X)){
    for(k in 1:ncol(X_nf)){
     # cat('\n', j, '-', k)
      J_gamma[j,k] <- apply(q_pred * X[,j] * X_nf[,k] * exp_gamma, 2, mean)
    }
  }
  
  var_from_ncvs <- J_gamma %*% (logreg_ncvs$cov.unscaled * nv) %*% t(J_gamma)

  #h_est <- c(est$Y - 1/(1 + exp(- X %*% theta)) * wgt) * X
  #var_from_nibrs <- t(h_est) %*% h_est / nn
  var_from_nibrs <- t(X) %*% Matrix::Diagonal(x = (Y - q_pred)^2) %*% X / nn
  
  # compute final estimate
  out <- solve(J_theta, var_from_nibrs + nn / nv * var_from_ncvs) %*% solve(J_theta)

  return(out/nn)
}


## GEEs -----


fit_gee_theta <- function(theta, alpha, X, Y, cg = cg, wgt = NULL, eps = 1e-4, verbose = TRUE){
  
  if(is.null(wgt)) wgt <- rep(1, length(cg))
  if(verbose) cli_progress_message('Estimating theta')
  
  if(is.unsorted(cg)) stop('Crime groups are not sorted')
  
  llk <- Inf
  llk_new <- 0
  while (abs(llk_new - llk) >= eps) {
    
    llk <- llk_new

    preds <- as.vector(1 / (1 + exp(-X %*% theta)))
    frequencies <- as.vector(table(cg))

    D <- Matrix::Diagonal(x = preds * (1-preds))
    D_wgt <- Matrix::Diagonal(x = preds * (1-preds) * wgt)
    C_alpha <- geeM:::getBlockDiag(frequencies, xvec = alpha)$BDiag
    diag(C_alpha) <- 1
    W <- sqrt(D) %*% C_alpha %*% sqrt(D)
    ft <- t(X) %*% D %*% Matrix::solve(W, D_wgt %*% X) 
    st <- t(X) %*% D %*% Matrix::solve(W, (Y - preds* wgt))
    theta <- theta + as.vector(Matrix::solve(ft, st))
    
    llk_new <- comp_llk(theta = theta, X = X, Y = Y, wgt = 1)
    
    # if(verbose) cli_text("nlog-lkl: ", llk_new)
  }
  theta
  
  
}


fit_gee_alpha <- function(theta, X, Y, wgt, cg, verbose = TRUE){
  
  # data should already have been sorted in the parent function, but check
  if(is.unsorted(cg)) stop('Crime groups are not sorted')

  preds <- as.vector(1 / (1 + exp(-X %*% theta)))
  #browser()
  Y_mat_sqrtwgt <- Matrix::Diagonal(x = Y/sqrt(preds * (1-preds)) * 1/wgt)
  Y_mat_wgt <- Matrix::Diagonal(x = Y/sqrt(preds * (1-preds)) * 1/wgt)
  q_mat <- Matrix::Diagonal(x = preds/sqrt(preds * (1-preds)))
  q_mat_wgt <- Matrix::Diagonal(x = wgt * preds/sqrt(preds * (1-preds)))
  frequencies <- as.vector(table(cg))
  block_diag <- geeM:::getBlockDiag(frequencies, xvec = 1)$BDiag
  cor_mat <- Y_mat_sqrtwgt %*% block_diag %*% Y_mat_sqrtwgt - q_mat %*% block_diag %*% q_mat
  
  # alternatively 
  
  diag(cor_mat) <- 0
  n_elements <- sum(cor_mat != 0)/2
  if(n_elements == 0){alpha_est <- 0} else{
    alpha_est <- sum(triu(cor_mat))/n_elements
  }
  # pi_star <- length(unique(cg))/sum(wgt[!duplicated(cg)])
  
  return(alpha_est)
}


sort_data <- function(X, Y, wgt, cg, df_original=NULL){

  X <- X[order(cg),,drop = FALSE]
  if(!is.null(df_original)){
    df_original <- df_original[order(cg),,drop = FALSE]
  }
  Y <- Y[order(cg)]
  wgt <- wgt[order(cg)]
  cg <- sort(cg)

  return(list(df_original = df_original, X = X, Y = Y, wgt = wgt, cg = cg))
}


# make the way weights are handled consistent
run_wgt_gee <- function(df_original,
                        df_processed, 
                        offense_name, 
                        wgt = TRUE, 
                        verbose = TRUE,
                        max_iter = 10){
  # if(offense_name == 'simple  assault'){browser()} else{return()}
  if(verbose) cli_li(glue("Regression for {offense_name}"))

  X = data.matrix(df_processed %>% dplyr::select(-any_of(c('y', 'wgt', 'originating_agency_identifier', 'incident_number'))), rownames.force = NULL)
  Y = df_processed$y

  # create grouping
  cg <- as.vector(df_original %>% group_by(year, originating_agency_identifier, incident_number) %>% group_indices())
  if(wgt){wgt = as.vector(df_processed %>% pull(wgt))}
  
  # sort the data
  df_sorted <- sort_data(df_original = df_original, X = X, Y = Y, wgt = wgt, cg = cg)
  df_original <- df_sorted$df_original
  X <- df_sorted$X
  Y <- df_sorted$Y
  wgt <- df_sorted$wgt
  cg <- df_sorted$cg
  rm(df_sorted)
  
  # initialize the parameters
  theta <- rep(0, ncol(X))
  alpha <- 0


  for(iter in 1:max_iter){

    if(verbose) cli_text(glue('Iteration # {iter}'))
    
    theta <- fit_gee_theta(
      theta = theta,
      alpha = alpha,
      X = X,
      Y = Y, 
      cg = cg,
      wgt = wgt, 
      verbose = verbose
    )
    
    alpha <- fit_gee_alpha(
      theta = theta, 
      X = X,
      Y = Y,
      wgt = wgt,
      cg = cg, 
      verbose = verbose
    )
    
  }
  
  
  theta <- tibble(term = colnames(X), estimate = as.vector(theta))
  
  return(list('offense' = offense_name, 
              'df_original' = df_original,
              'X' = X, 
              'Y'= Y, 
              'cg' = cg,
              'wgt' = wgt, 
              'coefs' = theta,
              'alpha' = alpha))
  
}



# estimate the variance
get_var_gee <- function(
  est, 
  logreg_ncvs, 
  logreg_ncvs_recipe,
  fake_run = FALSE,
  verbose = TRUE) {
  # browser()
  if(verbose)   cli_li('Computing variance')

  # extract data
  offense_name <- pluck(est, "offense")
  wgt <- pluck(est, "wgt")
  df_original <- pluck(est, "df_original")
  X <- pluck(est, "X")
  Y <- pluck(est, "Y")
  cg <- pluck(est, "cg")
  theta <- pull(pluck(est, "coefs"), estimate)
  alpha <- pluck(est, "alpha")
  
  # sort data (even if it should already be sorted)
  df_sorted <- sort_data(
    df_original = df_original,
    X = X, 
    Y = Y,
    wgt = wgt,
    cg = cg
  )
  
  df_original <- df_sorted$df_original
  X <- df_sorted$X
  Y <- df_sorted$Y
  wgt <- df_sorted$wgt
  cg <- df_sorted$cg
  rm(df_sorted)
  
  if(verbose)   cli_progress_message('Transforming data into ncvs-like format')
  
  # get design matrix for NIBRS into NCVS format
  nn <- length(unique(cg)) # remember that this is the number of groups
  nv <- length(logreg_ncvs$fitted.values)
  gamma <- coef(logreg_ncvs)
  # here incidents will be collapsed
  if(fake_run){
    X_nf <- bake(logreg_ncvs_recipe, 
                 df_original %>% 
                   mutate(crime_recode = offense_name) %>%
                   group_by(year, incident_number, originating_agency_identifier) %>%
                   mutate(n = 1:n()) %>% filter(n == 1) %>% ungroup %>% dplyr::select(-year, -incident_number, -originating_agency_identifier)) %>%
      as.matrix(.)
  } else{
    nibrs4reg <- prepare_nibrs_for_reg_multiple(df_original %>% select(-wgt))
    X_nf <- bake(logreg_ncvs_recipe, 
                 nibrs4reg)
    X_nf <- as.matrix(X_nf)
  }
  if(length(gamma) > ncol(X_nf)) X_nf <- cbind(1, X_nf)
  
  if(verbose)   cli_progress_message('Operations')

  # compute Xi_gee
  
  q <- as.vector(1/(1 + exp(-X %*% theta)))
  D <- Matrix::Diagonal(x = q * (1-q))
  frequencies <- as.vector(table(cg))
  C_alpha <- geeM:::getBlockDiag(frequencies, xvec = alpha)$BDiag
  diag(C_alpha) <- 1
  W <- crossprod(sqrt(D), C_alpha) %>% crossprod(., sqrt(D))

  meat_out <- solve(W, D %*% X)
  meat_in <- Matrix::Diagonal(x = Y - q) %*% geeM:::getBlockDiag(frequencies, xvec = 1)$BDiag %*% Matrix::Diagonal(x = Y - q)
  xi <- crossprod(meat_out, meat_in) %*% meat_out


  # J_{gee, theta}
  D_wgt <- Matrix::Diagonal(x = q * (1-q) * wgt)
  J_theta <- t(X) %*% D %*% Matrix::solve(W, D_wgt %*% X) / nn
  

  exp_gamma <- as.vector(exp(- X_nf %*% c(gamma)))
  st <- Matrix::Diagonal(x = q * rep(exp_gamma, times = frequencies)) %*% X_nf[rep(1:nrow(X_nf), times = frequencies),]
  J_gamma <- (t(X) %*% D %*% solve(W, st))/nn


  var_from_ncvs <- J_gamma %*% (logreg_ncvs$cov.unscaled * nv) %*% t(J_gamma)
  var_from_nibrs <- xi / nn
  
  # compute final estimate
  out <- Matrix::solve(J_theta, var_from_nibrs + nn / nv * var_from_ncvs) %*% Matrix::solve(J_theta)
  
  return(out/nn)
}


## other -----

# print functions ----

get_stars <- function(x){ # to = tidy output
  stats::symnum(x, corr = FALSE, na = FALSE,
                legend = FALSE,
                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                symbols = c("***", "** ", "*  ", ".  ", "   "))
}

est2char <- function(x, digits=2, std.error = FALSE){

  x <- round(x,2)
  x <- as.character(x)
  x_after_dot <- str_split(x, '\\.')[[1]][2]
  if(!(str_split(x, '')[[1]][1]=='-') & !std.error){
    x <- paste0('\\phantom{-}', x)
  }
  
  if(!grepl('\\.', x)){
    x_after_dot <- paste0(rep("0", digits), collapse = '')
    x <- paste0(x, '.', x_after_dot)
  } else if(nchar(x_after_dot)<digits){
    x <- paste0(x, rep(0, digits - nchar(x_after_dot)))
  } else{
    x
  }
  return(x)
}

to2char <- function(to, digits){
  # browser()
  lapply(to$p.value, get_stars)
  to %>%
    # transform into characters
    rowwise() %>%
    mutate(estimate = est2char(estimate, digits, FALSE),
           std.error = est2char(std.error, digits, TRUE)) %>%
    mutate(sig = get_stars(p.value)) %>%
    mutate(estimate = glue('{estimate} ({std.error}){sig}')) %>%
    ungroup %>%
    select(term, estimate)
}


get_table_coef <- function(x){
  x %>%
    purrr::imap( ~ to2char(.x, 2) %>%
                   setNames(c('term', .y))) %>%
    purrr::reduce(., full_join, by = 'term') %>%
    rowwise() %>% mutate(term = rename_vars(term)) %>% ungroup %>%
    select(term, matches('sex'), matches('robbery'), matches('aggravated'), matches('simple'))
}

# rename coefficients
rename_vars <- function(vars){
  if(vars=='(Intercept)') vars <- 'Intercept'
  if(vars=='inter_race_black_vic_white_off') vars <- 'Black victim-White off.'
  if(vars=='inter_race_white_vic_black_off') vars <- 'White victim-Black off.'
  if(vars=='inter_race_white_vic_white_off') vars <- 'White victim-White off.'
  if(vars=='age_of_offender') vars <- 'Age of offender'
  if(vars=='age_of_victim') vars <- 'Age of victim'
  if(vars=='age_of_victim2') vars <- 'Age of victim (squared)'
  if(vars=='age_of_victim3') vars <- 'Age of victim (cubic)'
  if(vars=='crime_recode_robbery') vars <- 'Crime is robbery'
  if(vars=='crime_recode_sex.offense') vars <- 'Crime is sex offense'
  if(vars=='crime_recode_aggravated.assault') vars <- 'Crime is aggravated assault'
  if(vars=='age_of_offender_X12.14') vars <- 'Age of off. 12-14'
  if(vars=='age_of_offender_X15.17') vars <- 'Age of off. 15-17'
  if(vars=='age_of_offender_X18.20') vars <- 'Age of off. 18-20'
  if(vars=='age_of_offender_X21.29') vars <- 'Age of off. 21-29'
  if(vars=='age_of_offender_X30.') vars <- 'Age of off. 30+'
  if(vars=='multiple_offenders_age_of_youngest_X12.14') vars <- 'Age of youngest off. 12-14'
  if(vars=='multiple_offenders_age_of_youngest_X15.17') vars <- 'Age of youngest off. 15-17'
  if(vars=='multiple_offenders_age_of_youngest_X18.20') vars <- 'Age of youngest off. 18-20'
  if(vars=='multiple_offenders_age_of_youngest_X21.29') vars <- 'Age of youngest off. 21-29'
  if(vars=='multiple_offenders_age_of_youngest_X30.') vars <- 'Age of youngest off. 30+'
  if(vars=='multiple_offenders_age_of_oldest_X12.14') vars <- 'Age of oldest off. 12-14'
  if(vars=='multiple_offenders_age_of_oldest_X15.17') vars <- 'Age of oldest off. 15-17'
  if(vars=='multiple_offenders_age_of_oldest_X18.20') vars <- 'Age of oldest off. 18-20'
  if(vars=='multiple_offenders_age_of_oldest_X21.29') vars <- 'Age of oldest off. 21-29'
  if(vars=='multiple_offenders_age_of_oldest_X30.') vars <- 'Age of oldest off. 30+'
  if(grepl('white', vars) & grepl('victim', vars)) vars <- 'Victim is white'
  if(grepl('male', vars) & grepl('victim', vars)) vars <- 'Victim is male'
  if(grepl('white', vars) & grepl('offender', vars)) vars <- 'Off. is white'
  if(grepl('male', vars) & grepl('offender', vars)) vars <- 'Off. is male'
  if(grepl('drugs', vars)) vars <- 'Off. drinking/drugs'
  if(grepl('during', vars) & grepl('yes', vars)) vars <- 'During day'
  if(grepl('intercept', vars)) vars <- 'Intercept'
  if(grepl('relationship', vars) & grepl('intimate', vars)) vars <- 'Off. is intimate partner'
  if(grepl('relationship', vars) & grepl('stranger', vars)) vars <- 'Off. is stranger'
  if(grepl('relationship', vars) & grepl('family', vars)) vars <- 'Off. is family member'
  if(grepl('relationship', vars) & grepl('acquaintance', vars)) vars <- 'Off. is acquaintance'
  if(grepl('relationship', vars) & grepl('known', vars)) vars <- 'Off. is known'
  if(grepl('weapon', vars) & grepl('firearm', vars)) vars <- 'Firearm present'
  if(grepl('weapon', vars) & grepl('other', vars)) vars <- 'Other weapon present'
  if(grepl('injury', vars) & grepl('serious', vars)) vars <- 'Serious injury'
  if(grepl('injury', vars) & grepl('minor', vars)) vars <- 'Minor injury'
  if(grepl('private', vars) & grepl('location', vars)) vars <- 'Private location'
  if(grepl('multiple', vars) & grepl('offense', vars)) vars <- 'Multiple offenses'
  if(grepl('msa', vars) & grepl('not.city', vars)) vars <- 'MSA, not central city'
  if(grepl('msa', vars) & grepl('city.of.msa', vars)) vars <- 'MSA, central city'
  if(grepl('attempted', vars)) vars <- 'Offense only attempted'
  if(vars=='multiple_offender') vars <- 'multiple_offenders present'
  if(vars=='total_pop') vars <- 'Total population (ORI, log)'
  if(vars=='total_off') vars <- '# offenses (ORI, log)'
  if(vars=='population_leoka') vars <- 'Population served (ORI, log)'
  if(vars=='population_served') vars <- 'Population served (ORI)'
  if(vars=='log_population_served') vars <- 'Log population served (ORI)'
  if(vars=='officer_workload') vars <- "Officer's workload (ORI)"
  if(vars=='frac_black_off') vars <- '\\% black offenders (ORI)'
  if(vars=='offense_per_1000capita') vars <- '\\# Offenses per 1000 capita (ORI)'
  if(vars=='size_pop_log') vars <- 'Population size (ORI, log)'
  if(vars=='officer_per_1000capita') vars <- '\\# Officers per 1000 capita (ORI)'
  if(vars=='crime_recode_simple.assault') vars <- 'Crime is simple assault'
  if(vars=='crime_recode_aggravated.assault') vars <- 'Crime is aggravated assault'
  if(vars=='crime_recode_sex.offense') vars <- 'Crime is sex offense'
  if(vars=='crime_recode_robbery') vars <- 'Crime is robbery'
  vars
}

# reorder rows
reorder_and_filter_rows <- function(coefs){
  coefs %>% filter(grepl('Intercept', term)) %>% arrange(term) %>%
    bind_rows(coefs %>% filter(grepl('male|white|White|Age', term) & grepl('offender|Off.|off.', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('male|white|White|Age', term) & grepl('Victim|victim', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('acquaintance|stranger|partner|member|known', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('injury', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('Private|Residence|day', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('substance|alcohol', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('Firearm|weapon', term)) %>% arrange(term))  %>%
    bind_rows(coefs %>% filter(grepl('completed|Multiple|attempted', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('MSA', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('ORI', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('Core', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('simple assault|aggravated assault|robbery|sex offense', term)) %>% arrange(term))
}
