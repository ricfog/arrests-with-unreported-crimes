
suppressMessages(library(tidyverse))
suppressMessages(library(here))

# create offense data ----

vars_to_select_from_ncvs <- c(
  'crime_recode',
  'year',
  'multiple_victim',
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
  'relationship_to_offender',
  'is_during_day',
  'injury',
  'weapon',
  'is_offense_attempted',
  'msa',
  'region',
  'is_private_location',
  'crime_type',
  'type_sex_offense')

other_vars_to_select <- c('y', 'crime_type',
                          'is_multiple_offense', 'state_abbv', 'fips',
                          'city_name', 'population_group', 'officer_per_1000capita',
                          'population_served', 'total_empl', 'total_off',
                          #'type_of_victim', 
                          'originating_agency_identifier',
                          'cleared_exceptionally')

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
      relationship_vic_to_off = relationship_vic_to_off_1,
      relationship_to_offender = case_when(
        # this classification does not reflect the one employed in the NIBRS codebook
        # offender was a current or former spouse, boyfriend, or girlfriend
        relationship_vic_to_off == "victim was boyfriend/girlfriend" |
          relationship_vic_to_off == "victim was spouse" |
          relationship_vic_to_off == "victim was common-law spouse" |
          relationship_vic_to_off == "victim was ex-spouse" |
          relationship_vic_to_off == "homosexual relationship" ~ "intimate partner",
        # offender was a family member (parent,
        # child, brother, sister, or other relatives)
        relationship_vic_to_off == "victim was sibling" |
          relationship_vic_to_off == "victim was stepsibling" |
          relationship_vic_to_off == "victim was parent" |
          relationship_vic_to_off == "victim was stepparent" |
          relationship_vic_to_off == "victim was child" |
          relationship_vic_to_off == "victim was stepchild" |
          relationship_vic_to_off == "victim was child of boyfriend/girlfriend" |
          relationship_vic_to_off == "victim was grandchild" |
          relationship_vic_to_off == "victim was grandparent" |
          relationship_vic_to_off == "victim was in-law" |
          relationship_vic_to_off == "victim was other family member" ~ "family member",
        # offender was an unrelated acquaintance
        relationship_vic_to_off == "victim was friend" |
          relationship_vic_to_off == "victim was acquaintance" |
          relationship_vic_to_off == "victim was neighbor" |
          relationship_vic_to_off == "victim was babysittee (the baby)" |
          relationship_vic_to_off == "victim was child of boyfriend/girlfriend" |
          relationship_vic_to_off == "victim was employee" |
          relationship_vic_to_off == "victim was employer" |
          relationship_vic_to_off == "victim was otherwise known" ~ "acquaintance",
        # stranger
        relationship_vic_to_off == "victim was stranger" |
          relationship_vic_to_off == "undetermined" |
          grepl("unknown", relationship_vic_to_off) ~ "stranger"
      ),
      multiple_offender = "no", # CHANGE THIS
      drinking_drugs_offender = case_when(
        use_alcohol == "Yes" | use_drugs == "Yes" ~ "yes",
        use_alcohol == "No" & use_drugs == "No" ~ "no"
      ),
      multiple_victim = 1, # CHANGE THIS
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
    filter(grepl('black|white', tolower(race_of_offender))) %>%
    filter(grepl('black|white', tolower(race_of_victim))) %>%
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
    write_csv(here("data", "nibrs", paste0("data_", offense_sel, ".csv")))
}


# get data of interest ----
list_df <- list(
  admin_batch = "admin_batch.csv",
  offender = "offender.csv",
  # arrestee = 'Arrestee_violent_1316.csv',
  offense = "offense.csv",
  victim = "victim.csv"
) %>%
  purrr::map(~ read_csv(here("data", "nibrs", .x),
    col_types = cols()
  ))

# states submitting all their data through nibrs in 2014
states <- "AR|CO|DE|ID|IA|KY|MI|MT|NH|ND|SC|SD|TN|VT|VA|WV"

# subset incidents of interest
which_incidents <- list_df$admin_batch %>%
  # only in the states selected
  filter(grepl(states, state_abbv)) %>%
  # one offender
  filter(total_offender_segments == 1) %>%
  # one victim
  filter(total_victim_segments == 1)
which_incidents <- which_incidents %>%
  # victim is either white or black
  inner_join(list_df$victim %>% filter(grepl("white|black", tolower(race_of_victim))) %>%
    distinct(originating_agency_identifier, incident_number, year), # unnecessary
  by = c("originating_agency_identifier", "incident_number", "year"))

which_incidents <- which_incidents %>%
  # offender is white or black
  inner_join(list_df$offender %>% filter(grepl("white|black", tolower(race_of_offender)))
    %>% distinct(originating_agency_identifier, incident_number, year), # unnecessary
  by = c("originating_agency_identifier", "incident_number", "year")
  ) %>%
  distinct(originating_agency_identifier, incident_number, year)

which_incidents %>% count(year)

# restrict data in the list to avoid join that is too cumbersome
list_df <- list_df %>% map(~ .x %>% inner_join(which_incidents,
  by = c("originating_agency_identifier", "incident_number", "year")
))

# filter data ----

# admin file
list_df$admin_batch <- list_df$admin_batch %>%
  mutate(y = ifelse(total_arrestee_segments>0,1,0)) %>%
  # handle incidents with exceptional clearances
  filter(cleared_exceptionally != 'in custody of other jurisdiction (includes extradition denied)' & 
           cleared_exceptionally != 'death of offender') %>%
  mutate(y = case_when(
    cleared_exceptionally == 'juvenile/no custody' ~ 0,
    cleared_exceptionally == 'prosecution declined' ~ 0,
    cleared_exceptionally == 'victim refused to cooperate' ~ 0,
    TRUE ~ y
  )) %>%
  select(originating_agency_identifier, incident_number, 
         total_offense_segments, core_city, country_division,
         state_abbv, year, time_day,
         cleared_exceptionally,
         time_day, city_name, y, 
         population_group, officer_rate,
         fips_county, msa_code, population_leoka, total_empl, total_off) %>%
  # next line is just to guarantee absence of duplicates. SHould be unnecessary
  distinct(originating_agency_identifier, incident_number, year, .keep_all = T)


list_df$arrestee %>%
# double checking analysis with only individual offenders
  inner_join(list_df$admin %>% filter(total_offender_segments == 1 & total_offense_segments == 1) %>% distinct(incident_number, year, originating_agency_identifier),
  by = c("originating_agency_identifier", "incident_number", "year")) %>%
  #
  inner_join(list_df$offense %>% filter(!is.na(ucr_offense_code)) %>% 
      distinct(incident_number, year, originating_agency_identifier, ucr_offense_code), by = c("originating_agency_identifier", "incident_number", "year")) %>%
  mutate(ucr_arrest_offense_code = case_when(
        grepl('fondling|sodomy|forcible rape|sexual|11a|11b|11c|11d', tolower(ucr_arrest_offense_code)) ~ 'sex offense',
        grepl('robbery|120', tolower(ucr_arrest_offense_code)) ~ 'robbery',
        grepl('aggravated|13a', tolower(ucr_arrest_offense_code))~ 'aggravated assault',
        grepl('simple|13b', tolower(ucr_arrest_offense_code)) ~ 'simple assault',
        TRUE ~ 'other'
  )) %>%
  group_by(ucr_offense_code, ucr_arrest_offense_code) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n))

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

list_df$victim <- list_df$victim %>%
  mutate(age_of_victim = as.numeric(ifelse(grepl('(00) Unknown', age_of_victim), NA,
                                           ifelse(grepl('NB|NN|BB', age_of_victim), 0, 
                                                  age_of_victim))))

list_df$offender <- list_df$offender %>%
  group_by(originating_agency_identifier) %>%
  mutate(frac_black_off = sum(tolower(race_of_offender)=='black')/n()) %>%
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




