
# print functions ----

sable <- function(x, escape = T) {
  knitr::kable(x = x, format = "html", digits = 2, 
               align = "c", escape = TRUE) %>% 
  kableExtra::kable_styling(position = "center")
}

cat_data_code_used <- function(data_code){
  data_code <- str_split(data_code, '')[[1]]
  cat(paste0(
    paste0(rep('----', 5), collapse = ''),
    '\n The data analyzed include incidents/victimizations ',
    '\n * occurring in the period ', if_else(data_code[1]=='0',
                                     '2003-last year available',
                                     '2012-2016'), ' (included)',
    '\n * with ', if_else(data_code[2]=='0',
                          'multiple offenders/victims',
                          'only one victim and offender'),
    '\n * ', if_else(data_code[3]=='0',
                     'including ',
                     'excluding '),
    'verbal threats of assault',
    '\n * ', if_else(data_code[4]=='0',
                     'including hispanics',
                     'excluding hispanics')
  ))
}

format_perc <- function(x){
  if_else(x < 1, '$<$1', as.character(x))
}



# variance estimation ----

get_var_n <- function(x){

  # get corresponding xmat
  xmat <- inner_join(x = x %>% select(nrow), 
                     y = xmat, 
                     by = 'nrow')
  # get predicted likelihood
  weight <- xmat %>% pull(lkl)
  # transform xmat into data matrix
  xmat <- data.matrix(xmat %>% select(-nrow, -lkl, -y))
  
  # compute the estimate of n
  n_tot <- sum(1/weight)
  
  # estimate the first term
  first_term <- sum((1-weight)/(weight^2))
  # estimate the second term
  second_term_out <- as.vector(apply((1-weight)/weight * xmat, 2, sum))
  second_term <- t(second_term_out) %*% V_coef_ncvs %*% second_term_out
  # compute variance
  var_n <- first_term + second_term
  return(tibble(estimate = n_tot, var = as.double(var_n)))
}

get_var_pi <- function(x){
  # get data
  xmat <- inner_join(x = x %>% select(nrow), 
                     y = xmat, 
                     by = 'nrow')
  weight <- xmat %>% pull(lkl)
  xmat <- data.matrix(xmat %>% select(-nrow, -lkl, -y))
  
  # compute
  n_obs <- nrow(xmat) # number of observations in data
  n_tot <- sum(1/weight)
  pi <- n_obs/n_tot
  var_pi <- (mean(1-weight)/pi - 
               2 * (1-pi) + 
               pull(get_var_n(x), var)/n_tot) / n_tot
  return(tibble(estimate = pi, var = var_pi))
}


get_var_q <- function(x){
  # get data
  xmat <- inner_join(x = x %>% select(nrow), 
                     y = xmat, 
                     by = 'nrow')
  weight <- xmat %>% pull(lkl)
  y <- xmat %>% pull(y)
  xmat <- data.matrix(xmat %>% select(-nrow, -lkl, -y))
  
  # compute
  n_obs <- nrow(xmat) # number of observations in data
  n_tot <- sum(1/weight)
  pi <- n_obs/n_tot
  q <- sum(y)/n_tot
  var_q <- pi^2 / n_tot * (
    q * (1-q) / pi^2
    - 2 * (pi * sum(y/weight)/(q*n_obs) - 1)
    + pull(get_var_n(x), var)/n_tot
    )
  return(tibble(estimate = q, var = var_q))
}


