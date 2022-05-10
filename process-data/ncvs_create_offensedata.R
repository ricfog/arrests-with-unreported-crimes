
suppressMessages(library(tidymodels))
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(stringr))
source(here('analysis', 'utils.R'))
# ----

args <- commandArgs(trailingOnly = TRUE)

if(length(args)==0){
  data_code <- '0110'
  #data_code <- '0010'
} else{
  data_code <- args[1] 
  sink(file = here('log-files', paste0('ncvs_data_processing_', data_code, '.log')))
}
cat_data_code_used(data_code)
data_code <- str_split(data_code, '')[[1]]
if(!all(data_code=='1' | data_code == '0')) stop('Code not valid')


# ----

# load data
df <- read_csv(here('data', 'ncvs', 'incident.csv'),
            col_types = cols()) %>%
  filter(year >= 2003) %>% # already done
  # outside of the US
  filter(in_what_city_town_village != 'outside us' | is.na(in_what_city_town_village)) %>%
  # offender is seen
  #filter(is_offender_seen == 'yes') %>%
  mutate(multiple_offender = case_when(
        number_of_offenders == "oou" ~ 'no',
        number_of_offenders !=  "oou" ~ 'yes')) %>%
  # only hispanics
  filter(grepl('black|white|mix', race_of_offender)) %>%
  filter(grepl('black|white', race_of_victim)) %>%
  # one victim
  filter(number_of_victims == 1)

vars_to_select <- c('reported_to_police',
                    'crime_recode',
                    'psu',
                    'stratum',
                    'year',
                    #'multiple_victim',
                    'race_of_victim',
                    'race_eth_of_victim',
                    'age_of_victim',
                    'sex_of_victim',
                    'multiple_offender',
                    'race_of_offender',
                    'race_eth_of_offender',
                    'age_of_offender',
                    'sex_of_offender',
                    # drinking_drugs_offender,
                    # 'gang_member_offender',
                    # change in case of multiple offenders
                    'relationship_to_offender',
                    'is_during_day',
                    'injury',
                    'weapon',
                    'is_offense_attempted',
                    'msa',
                    'region',
                    'is_private_location',
                    'type_sex_offense',
                    'multiple_offenders_age_of_youngest',
                    'multiple_offenders_age_of_oldest',
                    'ncvs_id_for_households',
                    'incident_weight_adjusted_for_series_crimes')

df <- df %>%
  mutate(
    reported_to_police = if_else(reported_to_police == 'yes', 1, 0),
    is_private_location = case_when(
      where_did_incident_happen == 'at, in, or near a friends/relatives/neighbors home' |
        where_did_incident_happen == 'near own home' |
        where_did_incident_happen == 'respondents home or lodging' ~ 'yes',
      where_did_incident_happen == 'school' |
        where_did_incident_happen == 'parking lot/garages' |
        where_did_incident_happen == 'open areas, on street or public transportation' |
        where_did_incident_happen == 'commercial places' ~ 'no'
    ),
    is_during_day = case_when( # following the classification in the the codebook
      about_what_time_did_incident_occur_start_1999_q1 == '6am-12am' |
        about_what_time_did_incident_occur_start_1999_q1 == '12am-3pm' |
        about_what_time_did_incident_occur_start_1999_q1 == '3pm-6pm' |
        about_what_time_did_incident_occur_start_1999_q1 == 'day' ~ 'yes',
      about_what_time_did_incident_occur_start_1999_q1 == '12pm-6am' |
        about_what_time_did_incident_occur_start_1999_q1 == '6pm-9pm' |
        about_what_time_did_incident_occur_start_1999_q1 == '9pm-12pm' |
        about_what_time_did_incident_occur_start_1999_q1 == 'night' ~ 'no'
    ),

    region = case_when(
      how_far_from_home != '> 50 miles' ~ region_where_household_is
    ),

    msa = case_when(
      in_what_city_town_village == 'same city etc' ~ msa_status
    ),
    # experiment
    type_sex_offense = case_when(
      grepl('rape', crime_type) ~ 'rape',
      grepl('sex', crime_type) ~ 'sexual assault',
      TRUE ~ 'other'
    ),

    is_offense_attempted = case_when(crime_type == 'attempted rape' |
                                       crime_type == 'at rob inj s asl' |
                                       crime_type == 'at rob inj m asl' |
                                       crime_type == 'at rob w aslt' |
                                       #crime_type == 'at ag aslt w wea' | # to follow NIBRS
                                       crime_type == 'verbal thr rape' |
                                       crime_type == 'ver thr sex aslt' |
                                       crime_type == 'verbal thr aslt' ~  'yes',
                                     TRUE ~ 'no')# this is not really attempted vs no
  )

# ----
#

## one vs multiple years
if(data_code[1]=='0'){
  year_min <- 2003
  year_max <- 2020
} else if(data_code[1]=='1'){
  year_min <- 2006
  year_max <- 2015
}
df <- df %>% filter(year >= year_min & year <= year_max)

##
if (data_code[2] == "0") {
  df <- df %>%
    mutate(
      sex_of_offender = case_when(
        single_offender_sex == 'female' ~ 'female',
        single_offender_sex == 'male' ~ 'male',
        multiple_offenders_sex == 'female' ~ 'female',
        multiple_offenders_sex == 'male' ~ 'male',
        multiple_offenders_sex == 'mix' ~ 'mix' # this may not be the best choice
      ),
      multiple_offenders_age_of_oldest = case_when(
        single_offender_age != 'oou' & !is.na(single_offender_age) ~ single_offender_age,
        multiple_offenders_age_of_oldest != 'oou' & !is.na(multiple_offenders_age_of_oldest) ~ multiple_offenders_age_of_oldest
       ),
      multiple_offenders_age_of_youngest = case_when(
        single_offender_age != 'oou' & !is.na(single_offender_age) ~ single_offender_age,
        multiple_offenders_age_of_youngest != 'oou' & !is.na( multiple_offenders_age_of_youngest) ~ multiple_offenders_age_of_youngest
       ),
      drinking_drugs_offender = case_when(
        multiple_offenders_drinking_drugs == "yes" ~ "yes",
        multiple_offenders_drinking_drugs == "no" ~ "no",
        single_offender_drinking_drugs == "yes" ~ "yes",
        single_offender_drinking_drugs == "no" ~ "no"
      ),
            relationship_to_offender = case_when(
        # offender was a current or former spouse, boyfriend, or girlfriend
        single_off_how_did_resp_know_offender == "boy/girlfrnd, ex" |
          single_off_how_did_resp_know_offender == "ex-spouse" |
          single_off_how_did_resp_know_offender == "spouse" |
          c_mult_off_spouse == "yes" |
          c_mult_off_ex_spouse == "yes" |
          c_mult_off_boy_girlfriend_or_ex == "yes" ~
        "known",
        # offender was a family member (parent,
        # child, brother, sister, or other relatives)
        single_off_how_did_resp_know_offender == "brother/sister" |
          single_off_how_did_resp_know_offender == "other relative" |
          single_off_how_did_resp_know_offender == "par or step-par" |
          single_off_how_did_resp_know_offender == "r child or step" |
          c_mult_off_parent_step == "yes" |
          c_mult_off_child_step == "yes" |
          c_mult_off_brother_sister == "yes" |
          c_mult_off_other_relative == "yes" ~ "known",
        # offender was an unrelated acquaintance
        single_off_how_well_known == "casual acquaint" |
          single_off_how_well_known == "casual acquaint/well known" |
          single_off_how_well_known == "well known" |
          c_mult_off_known_casual_acquaintance == "yes" |
          c_mult_off_known_well_known == "yes" |
          li_mult_off_how_well_known == "yes" ~ "known",
        # stranger
        single_offender_stranger == "stranger" |
          single_off_how_well_known == "sight only" |
          multiple_offenders_all_strangers == "yes" |
          c_mult_off_known_by_sight_only == "yes" ~ "stranger"
      )#,
    #   gang_member_offender = case_when( 
    #     single_offender_gang_member == '(1) Yes' ~ 'yes',
    #     single_offender_gang_member == '(2) No'  ~ 'yes',
    #     multiple_offenders_gang_member == '(1) Yes' ~ 'yes',
    #     multiple_offenders_gang_member == '(2) No' ~ 'no',
    #     single_offenders_gang_member == '(9) Out of universe' ~ 'no'
    #     multiple_offenders_gang_member == '(9) Out of universe' ~ 'no'
    # ),
      #multiple_victim = number_of_victims
    )
  vars_to_select <- setdiff(vars_to_select, "age_of_offender")
} else if (data_code[2] == "1") {
  df <- df %>%
    # one offender
    filter(number_of_offenders == "oou")


  df <- df %>%
    rename(
      sex_of_offender = single_offender_sex,
      age_of_offender = single_offender_age,
      drinking_drugs_offender = single_offender_drinking_drugs#,
      # gang_member_offender = single_offender_gang_member
    ) %>%
    mutate(
      relationship_to_offender = case_when(
        # offender was a current or former spouse, boyfriend, or girlfriend
        single_off_how_did_resp_know_offender == "boy/girlfrnd, ex" |
          single_off_how_did_resp_know_offender == "ex-spouse" |
          single_off_how_did_resp_know_offender == "spouse" ~ "intimate partner",
        # offender was a family member (parent, child, brother, sister, or other relatives)
        single_off_how_did_resp_know_offender == "brother/sister" |
          single_off_how_did_resp_know_offender == "other relative" |
          single_off_how_did_resp_know_offender == "par or step-par" |
          single_off_how_did_resp_know_offender == "r child or step" ~ "family member",
        # offender was an unrelated acquaintance
        single_off_how_well_known == "casual acquaint" |
          single_off_how_well_known == "casual acquaint/well known" |
          single_off_how_well_known == "well known" ~ "acquaintance",
        # offender was someone never seen before or someone known by sight only
        single_offender_stranger == "stranger" |
          single_off_how_well_known == "sight only" ~ "stranger"
      )
    )

  vars_to_select <- setdiff(vars_to_select, c('multiple_offender', 'multiple_victim', 
      'multiple_offenders_age_of_oldest',  'multiple_offenders_age_of_youngest'))
}




if(data_code[3]=='0'){
# do nothing, just including it for potentially future iterations
} else if(data_code[3]=='1'){
  df <- df %>%
    filter(crime_type != 'verbal thr aslt') #&
             #crime_type != 'sex aslt w s aslt' &
             #crime_type != 'sex aslt wo inj' &
             #crime_type != 'unw sex wo force' &
             #crime_type != 'ver thr sex aslt' &
             #crime_type != 'verbal thr rape')
}


# 0 -> race information only
# 1 -> ethnicity information and drop hispanics
if(data_code[4]=='0'){
  # do nothing
  vars_to_select <- setdiff(vars_to_select,
                            c('race_eth_of_offender', 'race_eth_of_victim'))
} else if(data_code[4]=='1'){
  if(data_code[1]=='0'){
    stop('Ethnicity information is not available for offenders before 2012!')
  }
  df <- df %>%
    filter(grepl('black|white', race_eth_of_offender)) %>%
    filter(grepl('black|white', race_eth_of_victim))
  vars_to_select <- setdiff(vars_to_select,
                            c('race_of_offender', 'race_of_victim'))
}


# -----

# save the entire dataset (used in the EDA)
w <- df %>% 
  write_csv(
    here('data', 'ncvs', paste0('all_info_data_', paste0(data_code, collapse = ''), '.csv'))
  )

# select only relevant columns
df <- df %>% select(all_of(vars_to_select))

#glimpse(df)

# convert to numeric / factors
if('age_of_victim' %in% colnames(df)){
  df <- mutate(df, age_of_victim = as.numeric(age_of_victim))
}
cols_to_factors <- setdiff(colnames(df), 'age_of_victim')
df <- df %>% mutate(across(where(is.character), as.factor))

# see the distribution of crimes
sum(df$incident_weight_adjusted_for_series_crimes)
df %>% group_by(crime_recode) %>%
  summarise(n = sum(incident_weight_adjusted_for_series_crimes)) %>%
  mutate(prop = n/sum(n))

# turn all oou into missing values
#colSums(df=='oou', na.rm=TRUE) %>% sort() %>% data.frame()
df[df=='oou'] <- NA

# -----

w <- df %>% 
  write_csv(
    here('data', 'ncvs', paste0('data_', paste0(data_code, collapse = ''), '.csv'))
    )

#