
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(haven))

# ----

clean_data <- function(df) {
  year <- df$V6
  df <- df %>%
    mutate(ORI9 = paste0(V3, "00")) %>%
    rename(year = V6) %>%
    rename(originating_agency_identifier = ORI9) %>%
    distinct(year, originating_agency_identifier, .keep_all = T)

  df <- df %>%
    mutate(
      population_leoka = ifelse(V9 == 0, NA, V9),
      total_empl = ifelse(V18 == 0, NA, V18),
      total_off = ifelse(V12 + V15 == 0, NA, V12 + V15),
      officer_rate = ifelse(V19 == 0, NA, V19)
    ) %>%
    select(originating_agency_identifier, year, 
           population_leoka, total_empl, total_off, officer_rate)

  return(df)
}


# ----

leoka_items <- c(
  4269, # 2003
  4462, # 2004
  4719, # 2005
  22402, # 2006
  25104, # 2007
  27646, # 2008
  30765, # 2009
  33525, # 2010
  34584, # 2011
  35020, # 2012
  36119, # 2013
  36395, # 2014
  36791, # 2015
  37062 # 2016
)


files <- list.files(path = here("downloads"), 
                    pattern = "\\.dta$", 
                    recursive = TRUE)
files <- files[parse_number(files) %in% leoka_items]
files <- files %>%
  map_chr(~ here("downloads", .x))

df <- files %>%
  map(~ clean_data(read_dta(.x))) %>%
  bind_rows()

df %>% write_csv(here("data", "leoka.csv"))
