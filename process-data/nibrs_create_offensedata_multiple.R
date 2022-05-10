
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(glue))

# create offense data ----

vars_to_select_from_ncvs <- c(
  'crime_recode',
  'year',
  #'multiple_victim',
  'race_of_victim',
  #'race_eth_of_victim',
  'age_of_victim',
  'sex_of_victim',
  'multiple_offender',
  'race_of_offender',
  #'race_eth_of_offender',
  'age_of_offender',
  'sex_of_offender',
  'drinking_drugs_offender',
  # 'gang_member_offender',
  'multiple_offender',
  'relationship_to_offender',
  'is_during_day',
  'injury',
  'weapon',
  'is_offense_attempted',
  'msa',
  'region',
  'is_private_location',
  'crime_type',
  'type_sex_offense'
  )

other_vars_to_select <- c('y', 'crime_type',
                          'is_multiple_offense', 'state_abbv', 'fips',
                          'city_name', 'population_group', 'officer_per_1000capita',
                          'population_served', 'total_empl', 'total_off',
                          #'type_of_xvictim', 
                          'incident_number', 'originating_agency_identifier',
                          'cleared_exceptionally')

type_relationship <- glue('boyfriend|girlfriend|spouse|ex-spouse|relationship|sibling|stepsibling|parent|stepparent|child|',
'stepchild|grandchild|grandparent|in-law|family|friend|acquaintance|neighbor|babysittee|employee|employer|otherwise')

# design matrix with dummy features
get_design_matrix <- function(X) {

  X <- X %>%
    mutate(
      sex_of_offender = tolower(sex_of_offender),
      sex_of_victim = tolower(sex_of_victim),
      race_of_offender = tolower(race_of_offender),
      race_of_victim = tolower(race_of_victim),
      is_private_location = case_when(
        grepl("20|14|residence|hotel", tolower(location_type)) ~ "yes",
        TRUE ~ 'no'
          #!grepl("25|unknown", tolower(location_type)) ~ "no" # this includes "other" as well
      ),
      is_during_day = case_when(
        time_day == "During day" ~ "yes",
        time_day == "At night" ~ "no"
      ),
      is_offense_attempted = case_when(
        offense_attempted_completed == "Attempted" ~ "yes",
        TRUE ~ "no"
      ),
      region = case_when(
        grepl("AR|DE|KY|SC|TN|VA|WV", state_abbv) ~ "south",
        grepl("NH|VT", state_abbv) ~ "northeast",
        grepl("CO|ID|MT", state_abbv) ~ "west",
        TRUE ~ "midwest"
      ),
      msa = case_when(
        is.na(msa_code) ~ "not msa",
        tolower(core_city) == "yes" ~ "city of msa",
        tolower(core_city) == "no" ~ "msa but not city"
      ),
      # processing of relationship victim - offender to be moved to previous steps?
      # this is common to all offenders
      relationship_to_offender = case_when(
        grepl(type_relationship, relationship_vic_to_off_1) ~ "known",
        grepl(type_relationship, relationship_vic_to_off_2) ~ "known",
        grepl(type_relationship, relationship_vic_to_off_3) ~ "known",
        grepl(type_relationship, relationship_vic_to_off_4) ~ "known",
        grepl(type_relationship, relationship_vic_to_off_5) ~ "known",
        grepl(type_relationship, relationship_vic_to_off_6) ~ "known",
        grepl(type_relationship, relationship_vic_to_off_7) ~ "known",
        grepl(type_relationship, relationship_vic_to_off_8) ~ "known",
        grepl(type_relationship, relationship_vic_to_off_9) ~ "known",
        grepl(type_relationship, relationship_vic_to_off_10) ~ "known",
        grepl('stranger|undetermined|unknown', relationship_vic_to_off_1) ~ 'stranger',
        grepl('stranger|undetermined|unknown', relationship_vic_to_off_2) ~ 'stranger',
        grepl('stranger|undetermined|unknown', relationship_vic_to_off_3) ~ 'stranger',
        grepl('stranger|undetermined|unknown', relationship_vic_to_off_4) ~ 'stranger',
        grepl('stranger|undetermined|unknown', relationship_vic_to_off_5) ~ 'stranger',
        grepl('stranger|undetermined|unknown', relationship_vic_to_off_6) ~ 'stranger',
        grepl('stranger|undetermined|unknown', relationship_vic_to_off_7) ~ 'stranger',
        grepl('stranger|undetermined|unknown', relationship_vic_to_off_8) ~ 'stranger',
        grepl('stranger|undetermined|unknown', relationship_vic_to_off_9) ~ 'stranger',
        grepl('stranger|undetermined|unknown', relationship_vic_to_off_10) ~ 'stranger',
      ),
      multiple_offender = ifelse(total_offender_segments > 1, 'yes', 'no'), # this will also account for offenders of other races that were dropped
      drinking_drugs_offender = case_when(
        use_alcohol == "Yes" | use_drugs == "Yes" ~ "yes",
        use_alcohol == "No" & use_drugs == "No" ~ "no"
      ),
      #multiple_victim = 1, # CHANGE THIS
      injury = case_when(
        ## CHECK THE DIFFERENT TYEPS IN V4026
        ## # MULTIPLE FIELDS
        injury == "loss of teeth" |
          injury == "apparent broken bones" |
          injury == "other major injury" |
          injury == "possible internal injury" |
          injury == "severe laceration" |
          injury == "unconsciousness" ~ "serious injury",
        injury == "apparent minor injury" ~ "minor injury",
        TRUE ~ "no"
      ),
      weapon = weapon_force, # simple renaming
      race_eth_of_victim = case_when(
        ethnicity_of_victim == "hispanic or latino" ~ "hispanic",
        TRUE ~ race_of_victim
      ),
      race_eth_of_offender = case_when(
        ethnicity_of_offender == "hispanic or latino" ~ "hispanic",
        TRUE ~ race_of_offender
      )
    )
  
  X <- X %>%
    mutate(
      is_multiple_offense = ifelse(total_offense_segments > 1, "yes", "no")
    ) %>%
    rename(
      officer_per_1000capita = officer_rate,
      population_served = population_leoka
    )
  
  X <- X %>%
    select(all_of(c(vars_to_select_from_ncvs, other_vars_to_select))) %>%
    droplevels()

  return(X)
}

write_df_for_offense <- function(list_df, offense_sel) {
  
  cat('Offense selected: ', offense_sel, '\n')
    
  df <- list_df$offense %>%
    filter(ucr_offense_code == offense_sel) %>%
    inner_join(list_df$admin,
               by = c("originating_agency_identifier", "incident_number", "year")
    ) %>%
    inner_join(list_df$offender,
               by = c("originating_agency_identifier", "incident_number", "year")
    ) %>%
    inner_join(list_df$victim,
               by = c("originating_agency_identifier", "incident_number", "year")
    ) %>%
    ungroup()
  
  # check types of offenses included
  print(unique(df$ucr_offense_code))
  
  df <- df %>%
    rename(crime_type = ucr_offense_code) %>%
    mutate(crime_recode = offense_sel)
  
  df <- get_design_matrix(df) 
  
  v <- df %>% 
    write_csv(here("data", "nibrs", paste0("data_", offense_sel, "_multiple.csv")))
}


# get data of interest ----
list_df <- list(
  admin_batch = "admin_batch.csv",
  offender = "offender.csv",
  arrestee = 'arrestee.csv',
  offense = "offense.csv",
  victim = "victim.csv"
) %>%
  purrr::map(~ read_csv(here("data", "nibrs", .x),
                     col_types = cols()
  ))

problems()

# states submitting all their data through nibrs in 2014
states <- "AR|CO|DE|ID|IA|KY|MI|MT|NH|ND|SC|SD|TN|VT|VA|WV"

# subset incidents of interest
which_incidents <- list_df$admin_batch %>%
  # only in the states selected
  filter(grepl(states, state_abbv)) %>%
  # one victim
  filter(total_victim_segments == 1) # to align with NCVS

# at least one of the victims is black or white
which_incidents <- which_incidents %>%
  inner_join(list_df$victim %>% filter(grepl("White|Black", race_of_victim)) %>%
               distinct(originating_agency_identifier, incident_number, year), # unnecessary
             by = c("originating_agency_identifier", "incident_number", "year"))

# all offenders considered in the analysis are black or white
which_incidents <- which_incidents %>%
  inner_join(list_df$offender %>% filter(grepl("White|Black", race_of_offender))
             %>% distinct(originating_agency_identifier, incident_number, year), # unnecessary
             by = c("originating_agency_identifier", "incident_number", "year")
  ) %>%
  distinct(originating_agency_identifier, incident_number, year)

which_incidents %>% count(year)

# restrict data in the list to avoid join that is too cumbersome
list_df <- list_df %>% map(~ .x %>% inner_join(which_incidents,
                                               by = c("originating_agency_identifier", "incident_number", "year")
))


# how often do offenses occur together?
# list_df$offense[1:1e5,] %>%
#   group_by(year, incident_number, originating_agency_identifier) %>% 
#   mutate(n_total = n(), ) %>%
#   ungroup %>%
#   filter(n_total > 1)
# only violent offenses
# list_df$offense[1:1e5,] %>%
#   filter(!is.na(ucr_offense_code)) %>%
#   group_by(year, incident_number, originating_agency_identifier) %>% 
#   summarise(n_total = n()) %>% ungroup %>%
#   count(n_total)



## match offenders and arrestees files ----

df_off <- list_df$offender %>% mutate(n_off = 1:n())
df_arr <- list_df$arrestee %>% mutate(n_arr = 1:n())

# make y = 1 for all crimes that have the same number of arrestees and offenders
# these offenders have one arrest (because we are conditioning on offender segment > 0)
matched <- df_off %>%
  inner_join(list_df$admin_batch %>%
              filter(total_arrestee_segments == total_offender_segments & total_offender_segments > 0),
              by = c("originating_agency_identifier", "incident_number", "year"))
# identifiers of offenders that have been matched
n_off_matched <- matched %>% pull(n_off)
# remove arrestees that have been matched above
df_off_left <- df_off[!(df_off$n_off %in% n_off_matched),]
df_arr_left <- df_arr[!(df_arr$n_arr %in% (df_arr %>% 
  inner_join(matched %>% distinct(originating_agency_identifier, incident_number, year),
  by = c("originating_agency_identifier", "incident_number", "year")) %>%
  pull(n_arr))),]

list_df$victim <- list_df$victim %>%
  mutate(age_of_victim = as.numeric(ifelse(grepl('(00) Unknown', age_of_victim), NA,
                                           ifelse(grepl('NB|NN|BB', age_of_victim), 0, 
                                                  age_of_victim))))

# match on (sex, age, race)
# repeat multiple times to avoid issue that record gets erroneously dropped due to distinct(n_arr)
# because this will drop offenders incidents where the offenders have the same demographics and have all 
# been arrested!
for(i in 1:15){
  matched <- df_off_left %>%
    inner_join(df_arr_left, 
              by = c("originating_agency_identifier", "incident_number", "year",
              "sex_of_offender" = "sex_of_arrestee",
              "race_of_offender" = "race_of_arrestee",
              "age_of_offender" = "age_of_arrestee")) %>%
    distinct(n_arr, .keep_all = TRUE) %>% # arrestee can appear only once
    distinct(n_off, .keep_all = TRUE) # offender can appear only once

  # check matching
  # df_off_left %>% inner_join(df_arr_left) %>% inner_join(matched)

  # add matched offenders to the list
  n_off_matched <- c(n_off_matched, matched$n_off)
  # remove matched offenders from the dataset to be matched with the next iteration
  df_off_left <- df_off_left[!(df_off_left$n_off %in% matched$n_off),]
  df_arr_left <- df_arr_left[!(df_arr_left$n_arr %in% matched$n_arr),]
}

# match on sex, race
for(i in 1:10){
  matched <- df_off_left %>%
  inner_join(df_arr_left, 
             by = c("originating_agency_identifier", "incident_number", "year",
             "sex_of_offender" = "sex_of_arrestee",
             "race_of_offender" = "race_of_arrestee")) %>%
  distinct(n_arr, .keep_all = TRUE) %>%
  distinct(n_off, .keep_all = TRUE)

  # add matched offenders to the list
  n_off_matched <- c(n_off_matched, matched$n_off)
  # remove matched offenders from the dataset to 
  df_off_left <- df_off_left[!(df_off_left$n_off %in% matched$n_off),]
  df_arr_left <- df_arr_left[!(df_arr_left$n_arr %in% matched$n_arr),]
}

# match only on race
for(i in 1:10){
  matched <- df_off_left %>%
    inner_join(df_arr_left, 
              by = c("originating_agency_identifier", "incident_number", "year",
              "race_of_offender" = "race_of_arrestee")) %>%
    distinct(n_arr, .keep_all = TRUE) %>% 
    distinct(n_off, .keep_all = TRUE)
  # add matched offenders to the list
  n_off_matched <- c(n_off_matched, matched$n_off)
  # remove matched offenders from the dataset to 
  df_off_left <- df_off_left[!(df_off_left$n_off %in% matched$n_off),]
  df_arr_left <- df_arr_left[!(df_arr_left$n_arr %in% matched$n_arr),]
}

# identify offenders that have been arrested
df_off$y <- 0
df_off$y[df_off$n_off %in% n_off_matched] <- 1
incidents_to_drop <- df_off %>%
  inner_join(df_arr_left, 
  by = c("originating_agency_identifier", "incident_number", "year")) %>%
  distinct(originating_agency_identifier, incident_number, year)
df_off <- df_off %>%
  left_join(df_arr_left %>% distinct(originating_agency_identifier, incident_number, year) %>% mutate(todrop = TRUE),
            by = c("originating_agency_identifier", "incident_number", "year")) %>%
            filter(is.na(todrop))
df_off <- df_off %>% select(-n_off, -todrop)

# check
list_df$arrestee %>%
# double checking analysis with only individual offenders
  inner_join(list_df$admin %>% filter(total_offender_segments == 1 & total_offense_segments == 1) %>% distinct(incident_number, year, originating_agency_identifier),
  by = c("originating_agency_identifier", "incident_number", "year")) %>%
  inner_join(list_df$offense %>% filter(!is.na(ucr_offense_code)) %>% 
      distinct(incident_number, year, originating_agency_identifier, ucr_offense_code), by = c("originating_agency_identifier", "incident_number", "year")) %>%
  group_by(ucr_offense_code, ucr_arrest_offense_code) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n/sum(n),2)) %>% arrange(ucr_offense_code, desc(prop)) %>% filter(prop > 0) %>% print(n = 500)


# handle incidents cleared by exceptional means
df_off <- df_off %>%
  inner_join(list_df$admin_batch %>% 
  # drop incidents that are not of interest here
  filter(cleared_exceptionally != 'in custody of other jurisdiction (includes extradition denied)' & 
           cleared_exceptionally != 'death of offender') %>%
  distinct(originating_agency_identifier, incident_number, year, cleared_exceptionally),   
    by = c("originating_agency_identifier", "incident_number", "year")) %>%
    mutate(y = case_when(
    cleared_exceptionally == 'juvenile/no custody' ~ 0,
    cleared_exceptionally == 'prosecution declined' ~ 0,
    cleared_exceptionally == 'victim refused to cooperate' ~ 0,
    TRUE ~ y
    )) %>%
    select(-cleared_exceptionally)
list_df$offender <- df_off


list_df$admin_batch <- list_df$admin_batch %>%
  select(originating_agency_identifier, incident_number, 
         total_offense_segments, core_city, country_division,
         state_abbv, year, time_day,
         total_offender_segments,
         cleared_exceptionally,
         time_day, city_name, 
         population_group, officer_rate,
         fips_county, msa_code, population_leoka, total_empl, total_off) %>%
  # next line is just to guarantee absence of duplicates. Should be unnecessary
  distinct(originating_agency_identifier, incident_number, year, .keep_all = T)


# attempt to fix NAs in counties with LEAIC files: some of the FIPS codes are 
# missing in the original batch/admin files released by ICPSR. Perhaps 
# dealing with data released directly from the FBI would solve the issue
leaic <- read_csv(here('data', 'leaic.csv'), col_types = cols()) %>%
  rename(fips = FIPS)

admin_before_2008 <- list_df$admin_batch %>%
  filter(year <= 2007) %>% # arbitrary date between 2005 and 2012 (leaic dates)
  left_join(leaic %>% filter(year == 2005) %>% 
              select(originating_agency_identifier, fips), 
            by = 'originating_agency_identifier')
admin_after_2008 <- list_df$admin_batch %>%
  filter(year >= 2008) %>%
  left_join(leaic %>% filter(year == 2012) %>% 
              select(originating_agency_identifier, fips), 
            by = 'originating_agency_identifier')
list_df$admin_batch <- admin_after_2008 %>%
  bind_rows(admin_before_2008) %>%
  select(-fips_county)

list_df$offender <- list_df$offender %>%
  group_by(originating_agency_identifier) %>%
  mutate(frac_black_off = sum(race_of_offender=='Black')/n()) %>%
    ungroup


# write data ----

list(
  aggravated_assault = "aggravated assault",
  simple_assault = "simple assault",
  robbery = "robbery",
  sex_offense = "sex offense"
) %>%
  purrr::map(~ write_df_for_offense(
    list_df = list_df,
    offense_sel = .x
  ))

warnings()