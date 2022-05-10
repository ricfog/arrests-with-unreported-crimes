
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(haven))
suppressMessages(library(glue))

# ----

clean_data <- function(df) {
    
  df <- df %>%
    rename(originating_agency_identifier = ORI9) 
  
  if('FSTATE' %in% colnames(df)){
    df <- df %>%
      mutate(FIPS = glue('{FSTATE}{FCOUNTY}'))
  }

  return(df)
}


# -----

leaic_items <- c(35158, 4634)

files <- list.files(path = here("downloads"), 
                    pattern = "\\.dta$", 
                    recursive = TRUE)
files <- files[parse_number(files) %in% leaic_items]
files <- files %>%
  map_chr(~ here("downloads", .x))
names(files) <- c('2012', '2005')

df <- files %>%
  map_dfr(~ clean_data(read_dta(.x)), .id = 'year') %>%
  distinct(originating_agency_identifier, FIPS, year) %>%
  filter(originating_agency_identifier != '' & 
           originating_agency_identifier != '-1')

df %>% write_csv(here("data", "leaic.csv"))


