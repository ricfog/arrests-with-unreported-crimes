
# options(warn=-1)

suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(tidymodels))
suppressMessages(library(glue))
suppressMessages(library(xtable))
suppressMessages(library(survey))
source(here("analysis", "utils.R"))
source(here("analysis", "utils_regression.R"))

# -----

files_nibrs <- list.files(here("data", "nibrs"))
files_nibrs <- files_nibrs[grepl("weights", files_nibrs) & !grepl("multiple", files_nibrs)]
names(files_nibrs) <- str_split(files_nibrs, "_") %>%
  map(~ .x[3]) %>%
  unlist()
df_list <- files_nibrs %>%
  purrr::map(~ read_tsv(here("data", "nibrs", .x), col_types = cols()) %>%
    select(-matches("nb|logistic|nnet|lasso|rf")) %>%
    rename(lkl = mod_log) %>%
    filter(year >= 2006 & year <= 2015))

df_list <- df_list %>% map(~ .x %>%
  filter(race_of_offender == "black" | race_of_offender == "white") %>%
  filter(race_of_victim == "black" | race_of_victim == "white"))

# load logistic regression models fitted on NCVS
data_code <- "0110"
logreg_ncvs <- read_rds(here(
  "data", "regmodels",
  paste0("ncvs_1_", data_code, "_weightedlog.rds")
))
logreg_ncvs_recipe <- read_rds(here("data", "regmodels", glue("ncvs_1_{data_code}_weightedlog_recipe.rds")))

df_list <- df_list %>%
  map(~ .x %>%
    select(-lkl) %>%
    bind_cols(
      tibble(lkl = predict(logreg_ncvs, bake(logreg_ncvs_recipe, prepare_nibrs_for_reg(.x)), type = "response", se = FALSE))
    ))


# check presence of small weights
df_list %>%
  map_dfr(~ .x %>%
    summarise(quants = quantile(lkl, probs = c(1e-10, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))),
  .id = "crime_recode"
  ) %>%
  group_by(crime_recode) %>%
  mutate(nrow = 1:n()) %>%
  pivot_wider(names_from = crime_recode, values_from = quants)

df_list %>% map_dfr(~ .x %>% summarise(min_lkl = min(lkl), lower_than_5_100 = mean(lkl <= 0.1)),
  .id = "crime_recode"
)

dfb <- df_list %>% map_dfr(~ .x %>% mutate(fips = as.character(fips)), .id = "crime") # %>% arrange(lkl)

# trimming weights -- not needed
# df <- df %>% mutate(lkl = pmax(lkl, 0.05))

get_summary_stats <- function(df) {

  xmat <- bake(logreg_ncvs_recipe, prepare_nibrs_for_reg(df))
  y <- df$y
  var_names <- colnames(xmat)
  xmat <- cbind(1, xmat)
  
  # coefficients names match
  if (!all(colnames(xmat)[-1] == names(coef(logreg_ncvs))[-1])) stop("There's a problem with the coefficients names")
  preds_log_linear <- predict(logreg_ncvs, xmat, se = FALSE)
  lkl <- 1 / (1 + exp(-preds_log_linear))

  # covariance matrix of logistic regression
  nv <- length(logreg_ncvs$fitted.values)
  nn <- nrow(xmat)
  N <- sum(1 / lkl)
  alpha <- sum(y) / nn
  pi_star <- nn / N
  q_star <- sum(y) / N

  # if(variance_estimation){
  #
  se_alpha <- sqrt(alpha * (1 - alpha)) / sqrt(nn)

  # computation of variance for N
  logreg_cov <- logreg_ncvs$cov.unscaled * nv # survey:::svy.varcoef(logreg_ncvs, design = logreg_ncvs_design) * nv
  W <- colSums(exp(-preds_log_linear) * 1 / lkl * xmat) / nn
  var_N_est <- pi_star^2 * (mean((1 - lkl) / lkl^2) + t(W) %*% logreg_cov %*% W * nn / nv)
  se_N <- sqrt(var_N_est) / sqrt(nn) * N

  # computation of variance for pi_star
  var_pi_star <- pi_star^2 * (mean((pi_star - lkl) / lkl^2) + nn / nv * pi_star^2 * t(W) %*% logreg_cov %*% W)
  se_pi_star <- sqrt(var_pi_star) / sqrt(nn)

  # computation of variance for q_star
  var_q_star <- pi_star * q_star * (pi_star * mean((q_star - alpha * lkl) / lkl^2) + 1 - alpha + pi_star * nn / nv * t(W) %*% logreg_cov %*% W)
  se_q_star <- sqrt(var_q_star) / sqrt(nn)

  tribble(
    ~stat, ~value, ~se,
    "N", N, as.numeric(se_N),
    "pi", pi_star, as.numeric(se_pi_star),
    "n", nn, NA,
    "alpha", alpha, se_alpha,
    "q", q_star, as.numeric(se_q_star)
  )
}


# main analysis

## overall summary statistics ----

sstats <- df_list %>%
  map_dfr(~ .x %>% select(-fips)) %>%
  get_summary_stats()
sstats %>% xtable()


## summary stats by offense type ----

sstats_x_offense <- df_list %>% map(~ .x %>% get_summary_stats())
sstats_x_offense %>%
  bind_rows(.id = "model") %>%
  mutate(value = case_when(
    is.na(se) & value < 1 ~ glue("{round(value*100)}%"),
    is.na(se) ~ glue("{round(value)}"),
    value < 1 ~ glue("{round(value*100)}% ({round(se*100)})"),
    TRUE ~ glue("{round(value)} ({round(se)})")
  )) %>%
  select(-se) %>%
  pivot_wider(names_from = model, values_from = value) %>%
  select(`sex offense`, robbery, `aggravated assault`, `simple assault`) %>%
  xtable() %>%
  print(., include.rownames = FALSE)


## summary stats by racial group ----

df_list %>%
  map_dfr(~ .x %>% select(-fips)) %>%
  count(race_of_offender) %>%
  mutate(prop = n / sum(n))
# stats by racial group
sstats_blackoff <- df_list %>%
  map_dfr(~ .x %>%
    select(-fips) %>%
    filter(race_of_offender == "black")) %>%
  get_summary_stats(.)
sstats_whiteoff <- df_list %>%
  map_dfr(~ .x %>%
    select(-fips) %>%
    filter(race_of_offender == "white")) %>%
  get_summary_stats(.)
sstats_blackoff %>% xtable()
sstats_whiteoff %>% xtable()

# share of whites in all offenses
white_off_nibrs <- sum(dfb$race_of_offender == "white")
black_off_nibrs <- sum(dfb$race_of_offender == "black")
white_off_nibrs / (white_off_nibrs + black_off_nibrs)
white_off_nibrs / sstats_whiteoff$value[2] / (white_off_nibrs / sstats_whiteoff$value[2] + black_off_nibrs / sstats_blackoff$value[2])

## summary stats by racial group and crime type ----

sstats_raceoff <- df_list %>%
  map(~ .x %>%
    group_split(race_of_offender) %>%
    map(~ .x %>% get_summary_stats()) %>%
    setNames(unique(arrange(df_list[[1]], race_of_offender)$race_of_offender)))
# plot
stats_raceoff <- sstats_raceoff %>%
  map_dfr(~ .x %>% bind_rows(.id = "race_of_offender"), .id = "crime") %>%
  filter(stat == "alpha" | stat == "q")
p <- stats_raceoff %>%
  filter(crime != "sex offense") %>%
  ggplot(aes(stat, value, fill = race_of_offender)) +
  geom_col(position = position_dodge(0.9)) +
  facet_wrap(~ factor(crime, levels = c("robbery", "aggravated assault", "simple assault"))) +
  geom_errorbar(aes(ymin = value - 1.64 * se, ymax = value + 1.64 * se),
    position = position_dodge(width = 0.9), width = 0.1
  ) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.2, 0.4), labels = c("0%", "20%", "40%")) +
  scale_fill_manual("Race of offender", values = c("grey", "goldenrod")) +
  ylab("Share of crimes that result in arrests") +
  scale_x_discrete("", labels = c("offenses known\n to police", "all offenses"))

ggsave(
  height = 4, width = 8, plot = p,
  filename = here("figures", paste0("arrest_rates.pdf"))
)



# generate table
stats_raceoff_tb <- sstats_raceoff %>%
  map_dfr(~ .x %>%
    bind_rows(.id = "race_of_offender") %>%
    mutate(value = case_when(
      is.na(se) & value < 1 ~ glue("{round(value*100)}\\%"),
      is.na(se) ~ glue("{round(value)}"),
      value < 1 ~ glue('{round(value*100)}\\% ({ifelse(round(se*100)<1, "<1", round(se*100))}\\%)'),
      TRUE ~ glue("{round(value)} ({round(se)})")
    )) %>%
    select(-se), .id = "crime_recode") %>%
  pivot_wider(names_from = crime_recode, values_from = value) %>%
  group_by(race_of_offender) %>%
  mutate(n_row = 1:n()) %>%
  arrange(n_row, race_of_offender) %>%
  select(-n_row) %>%
  mutate(race_of_offender = glue("{race_of_offender} offenders")) %>%
  select(race_of_offender, `sex offense`, robbery, `aggravated assault`, `simple assault`)
stats_raceoff_tb$race_of_offender <- glue("\\textbullet\\; {stats_raceoff_tb$race_of_offender}")


desc_tb <- c(
  "\\# incidents (w/$\\pi$)", "\\% police notification, $\\pi^*$",
  "\\# reported incidents", "\\% arrests, $\\alpha^*$", "\\% arrests (w/$\\pi$), $q^*$"
) %>%
  cbind(., matrix(NA, nrow = 5, ncol = 4))
colnames(desc_tb) <- colnames(stats_raceoff_tb)

desc_tb[1, ] %>%
  bind_rows(stats_raceoff_tb[c(1, 2), ]) %>%
  bind_rows(desc_tb[2, ]) %>%
  bind_rows(stats_raceoff_tb[c(3, 4), ]) %>%
  bind_rows(desc_tb[3, ]) %>%
  bind_rows(stats_raceoff_tb[c(5, 6), ]) %>%
  bind_rows(desc_tb[4, ]) %>%
  bind_rows(stats_raceoff_tb[c(7, 8), ]) %>%
  bind_rows(desc_tb[5, ]) %>%
  bind_rows(stats_raceoff_tb[c(9, 10), ]) %>%
  xtable() %>%
  print(., include.rownames = FALSE, sanitize.text.function = identity)
