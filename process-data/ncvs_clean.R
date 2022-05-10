
suppressMessages(library(asciiSetupReader))
suppressMessages(library(here))
suppressMessages(library(tidyverse))

# ----

read_file <- function(txt) {
  df <- read_ascii_setup(
    data = txt,
    setup_file = str_replace(txt, "-Data.txt", "-Setup.sas")
  ) %>% tibble(.name_repair = 'universal')

  colnames(df) <- tolower(colnames(df))

  return(df)
}


file_name <- '38136'
# #37698 -> https://www.icpsr.umich.edu/web/NACJD/studies/37689?utm_content=default&utm_medium=email&utm_source=govdelivery

# read file
files <- list.files(
  path = here("downloads", paste0('ICPSR_', file_name)),
  pattern = "\\.txt$",
  recursive = TRUE
)
files <- files[grepl('DS', files)] %>%
  map_chr(~ here("downloads", paste0('ICPSR_', file_name), .x))

incident <- read_file(files[grepl('DS0003', files)])

# ----
# process data

# keep only relevant data
incident <- incident %>% filter(ncvs_year >= 2003)

# -----
# get only offenses of interest

# recode crime into macro-categories
# definition from nibrs @ https://ucr.fbi.gov/nibrs/2013/resources/nibrs-rape-vs.-srs-rape
#  https://ucr.fbi.gov/nibrs/2012/resources/nibrs-offense-definitions
incident <- incident %>%
  rename(crime_type = toc_code_new_ncvs) %>%
  mutate(crime_recode =
           case_when(
             crime_type == '(01) Completed rape' |
               crime_type == '(02) Attempted rape' |
               crime_type == '(03) Sex aslt w s aslt' |
               crime_type == '(04) Sex aslt w m aslt' |
               crime_type == '(15) Sex aslt wo inj' |
               crime_type == '(16) Unw sex wo force' |
               # the last two may have to be excluded
               crime_type == '(18) Verbal thr rape' |
               crime_type == '(19) Ver thr sex aslt' ~ 'sex offense',
             crime_type == '(05) Rob w inj s aslt' |
               crime_type == '(06) Rob w inj m aslt' |
               crime_type == '(07) Rob wo injury' |
               crime_type == '(08) At rob inj s asl' |
               crime_type == '(09) At rob inj m asl' |
               crime_type == '(10) At rob w aslt' ~ 'robbery',
             crime_type == '(11) Ag aslt w injury' |
               crime_type == '(12) At ag aslt w wea' |
               crime_type == '(13) Thr aslt w weap' ~ 'aggravated assault',
             crime_type == '(14) Simp aslt w inj' |
               crime_type == '(17) Asl wo weap, wo inj' |
               crime_type == '(20) Verbal thr aslt' # 20 is included by BJS https://www.bjs.gov/content/pub/pdf/cv15.pdf
             ~ 'simple assault'
           )) %>%
  mutate(crime_type = tolower(str_sub(crime_type, start = 6)))
incident <- incident %>% filter(!is.na(crime_recode))
nrow(incident)

# check for what share of cases the offender is not seen
incident %>%
  mutate(offender_was_seen = case_when(
    ((li_multiple_offender_race == '(9) Out of universe' | li_single_offender_race_start_2012_q1 == '(9) Out of universe' |  
      (single_offender_sex == '(3) Dont know' | single_offender_sex == '(8) Residue' | single_offender_sex == '(9) Out of universe')) & 
       did_you_personally_see_an_offender == '(2) No') ~ 'No',
    did_you_personally_see_an_offender == '(8) Residue' ~ 'residue',
    TRUE ~ 'Yes'
  )) %>%
  group_by(#crime_recode, 
           offender_was_seen) %>%
  summarise(n = sum(incident_weight_adjusted_for_series_crimes)) %>%
  mutate(prop = n/sum(n))
incident %>% count(did_you_personally_see_an_offender)


# check crimes
incident %>% count(crime_recode, crime_type)

# rename incident
incident <- incident %>%
  rename(stratum = pseudostratum_code, # V2117
         psu = secucode_half_sample_code # V2118
  )

# -----

# Taxonomy:
# 1. out of universe: question was not meant to be asked
# 2. residue: should have been asked but was not asked OR keying error in answering.
#    BJS classifies these cases as don't know -> make it NA
# 3. don't know -> make it NA
incident <- incident %>%
  mutate(

    # -----
    # victim's characteristics

    sex_of_victim = case_when( # V3018
      sex_allocated == '(1) Male'~ 'male',
      sex_allocated == '(2) Female'~ 'female'
    ),

    # simple renaming
    age_of_victim = age_allocated, # V3014


    number_of_victims = round(victimization_weight_adjusted_for_series_crimes/incident_weight_adjusted_for_series_crimes),

    msa_status = case_when( # V4022
      msa_status == '(1) City of (S)MSA' ~ 'city of msa',
      msa_status == '(2) (S)MSA not city' ~ 'msa but not city',
      msa_status == '(3) Not (S)MSA' ~ 'not msa'
    ),

    region_where_household_is = case_when( # V2127B
      grepl('1', region_1990_2000_2010_sample_design_start_1995_q3) ~ 'northeast',
      grepl('2', region_1990_2000_2010_sample_design_start_1995_q3) ~ 'midwest',
      grepl('3', region_1990_2000_2010_sample_design_start_1995_q3) ~ 'south',
      grepl('4', region_1990_2000_2010_sample_design_start_1995_q3) ~ 'west',
    ),


    # -----
    ## information about reporting

    # the following question is
    #  Were the police informed or did they find out about this incident in any way?
    # so it' more about police notification rather than reporting
    reported_to_police = case_when( # V4399
      reported_to_police == '(1) Yes' ~ 'yes',
      reported_to_police == '(2) No' ~ 'no',
      reported_to_police == '(3) Dont know' ~ 'no', # following what BJS does
      reported_to_police == '(8) Residue' ~ 'no' # again, follow BJS
    ),

    how_did_police_find_out = case_when( # V4400
      grepl('1', how_did_police_find_out) ~ 'respondent',
      grepl('2', how_did_police_find_out) ~ 'other household member',
      grepl('3', how_did_police_find_out) ~ 'someone official',
      grepl('4', how_did_police_find_out) ~ 'someone else',
      grepl('5', how_did_police_find_out) ~ 'police at scene',
      grepl('6', how_did_police_find_out) ~ 'offender was a police officer',
      grepl('7', how_did_police_find_out) ~ 'some other way',
      grepl('9', how_did_police_find_out) ~ 'oou'
    ),

    # reasons for not reporting
    # not covering all the cases
    most_important_reason_not_reported =
    tolower(str_sub(most_important_reason_not_reported, start = 6)),
    most_important_reason_not_reported = case_when(
      most_important_reason_not_reported == 'out of universe' ~ 'oou',
      most_important_reason_not_reported !=
      'residue' ~ most_important_reason_not_reported
    ),
    reason_not_reported = case_when( # V4402
      li_reason_not_reported == '(9) Out of universe' ~ 'oou',
      li_reason_not_reported == '(1) At least 1 entry' ~ 'at least 1 entry',
      li_reason_not_reported == '(8) No good entry' ~ 'no good entry'
    ),
    c_reason_not_rptd_advised_not_to_report = case_when(
      grepl('1', c_reason_not_rptd_advised_not_to_report) ~ 'yes',
      grepl('2', c_reason_not_rptd_advised_not_to_report) ~ 'no',
      grepl('9', c_reason_not_rptd_advised_not_to_report) ~ 'oou'
    ),
    c_reason_not_rptd_personal_matter = case_when(
      grepl('1', c_reason_not_rptd_personal_matter) ~ 'yes',
      grepl('2', c_reason_not_rptd_personal_matter) ~ 'no',
      grepl('9', c_reason_not_rptd_personal_matter) ~ 'oou'
    ),
    c_reason_not_rptd_not_clear_a_crime = case_when(
      grepl('1', c_reason_not_rptd_not_clear_a_crime) ~ 'yes',
      grepl('2', c_reason_not_rptd_not_clear_a_crime) ~ 'no',
      grepl('9', c_reason_not_rptd_not_clear_a_crime) ~ 'oou'
    ),
    c_reason_not_rptd_lack_of_proof = case_when(
      grepl('1', c_reason_not_rptd_lack_of_proof) ~ 'yes',
      grepl('2', c_reason_not_rptd_lack_of_proof) ~ 'no',
      grepl('9', c_reason_not_rptd_lack_of_proof) ~ 'oou'
    ),
    c_reason_not_rptd_police_biased = case_when(
      grepl('1', c_reason_not_rptd_police_biased) ~ 'yes',
      grepl('2', c_reason_not_rptd_police_biased) ~ 'no',
      grepl('9', c_reason_not_rptd_police_biased) ~ 'oou'
    ),
    c_reason_not_rptd_not_import_to_police = case_when(
      grepl('1', c_reason_not_rptd_not_import_to_police) ~ 'yes',
      grepl('2', c_reason_not_rptd_not_import_to_police) ~ 'no',
      grepl('9', c_reason_not_rptd_not_import_to_police) ~ 'oou'
    ),
    c_reason_not_rptd_police_inefficient = case_when(
      grepl('1', c_reason_not_rptd_police_inefficient) ~ 'yes',
      grepl('2', c_reason_not_rptd_police_inefficient) ~ 'no',
      grepl('9', c_reason_not_rptd_police_inefficient) ~ 'oou'
    ),
    c_reason_not_rptd_offender_police_offcr = case_when(
      grepl('1', c_reason_not_rptd_offender_police_offcr) ~ 'yes',
      grepl('2', c_reason_not_rptd_offender_police_offcr) ~ 'no',
      grepl('9', c_reason_not_rptd_offender_police_offcr) ~ 'oou'
    ),
    c_reason_not_rptd_protect_offender = case_when(
      grepl('1', c_reason_not_rptd_protect_offender) ~ 'yes',
      grepl('2', c_reason_not_rptd_protect_offender) ~ 'no',
      grepl('9', c_reason_not_rptd_protect_offender) ~ 'oou'
    ),
    c_reason_not_rptd_fear_of_reprisal = case_when(
      grepl('1', c_reason_not_rptd_fear_of_reprisal) ~ 'yes',
      grepl('2', c_reason_not_rptd_fear_of_reprisal) ~ 'no',
      grepl('9', c_reason_not_rptd_fear_of_reprisal) ~ 'oou'
    ),

    # reasons reported: just focus on the most important one
    most_important_reason_for_report =
    tolower(str_sub(most_important_reason_for_report, start = 6)),
    most_important_reason_for_report = case_when(
      most_important_reason_for_report == 'out of universe' ~ 'oou',
      most_important_reason_for_report != 'residue' ~ most_important_reason_for_report
    ),


    # -----
    # police response

    police_came_when_notified = case_when( # V4438
      grepl('1', police_came_when_notified) ~ 'yes',
      grepl('2', police_came_when_notified) ~ 'no',
      grepl('4', police_came_when_notified) ~ 'respondent went to police',
      grepl('6', police_came_when_notified) ~ 'yes', # only 3 cases
      grepl('9', police_came_when_notified) ~ 'oou'
    ),

    how_soon_did_police_respond = case_when( # V4439
      grepl('1', how_soon_did_police_respond) ~ '<5 minutes',
      grepl('2', how_soon_did_police_respond) ~ '<10 minutes',
      grepl('3', how_soon_did_police_respond) ~ '<1 hour',
      grepl('4', how_soon_did_police_respond) ~ '<1 day',
      grepl('5', how_soon_did_police_respond) ~ '>1 day',
      grepl('9', how_soon_did_police_respond) ~ 'oou'
    ),

    # type of police response
    li_type_of_police_response = case_when( # V4440
      li_type_of_police_response == '(9) Out of universe' ~ 'oou',
      li_type_of_police_response == '(1) At least 1 entry' ~ 'at least 1 entry',
      li_type_of_police_response == '(8) No good entry' ~ 'no good entry'
    ),
    c_police_resp_took_report = case_when(
      grepl('1', c_police_resp_took_report) ~ 'yes',
      grepl('2', c_police_resp_took_report) ~ 'no',
      grepl('9', c_police_resp_took_report) ~ 'oou'
    ),
    c_police_resp_searched = case_when(
      grepl('1', c_police_resp_searched) ~ 'yes',
      grepl('2', c_police_resp_searched) ~ 'no',
      grepl('9', c_police_resp_searched) ~ 'oou'
    ),
    c_police_resp_took_evidence = case_when(
      grepl('1', c_police_resp_took_evidence) ~ 'yes',
      grepl('2', c_police_resp_took_evidence) ~ 'no',
      grepl('9', c_police_resp_took_evidence) ~ 'oou'
    ),
    c_police_resp_questioned_witness_susp = case_when(
      grepl('1', c_police_resp_questioned_witness_susp) ~ 'yes',
      grepl('2', c_police_resp_questioned_witness_susp) ~ 'no',
      grepl('9', c_police_resp_questioned_witness_susp) ~ 'oou'
    ),

    c_police_resp_promised_investigate = case_when(
      grepl('1', c_police_resp_promised_investigate) ~ 'yes',
      grepl('2', c_police_resp_promised_investigate) ~ 'no',
      grepl('9', c_police_resp_promised_investigate) ~ 'oou'
    ),
    c_police_resp_promised_surveillance = case_when(
      grepl('1', c_police_resp_promised_surveillance) ~ 'yes',
      grepl('2', c_police_resp_promised_surveillance) ~ 'no',
      grepl('9', c_police_resp_promised_surveillance) ~ 'oou'
    ),
    c_police_resp_made_arrest = case_when(
      grepl('1', c_police_resp_made_arrest) ~ 'yes',
      grepl('2', c_police_resp_made_arrest) ~ 'no',
      grepl('9', c_police_resp_made_arrest) ~ 'oou'
    ),


    # V4466: As far as you know, was anyone arrested or were
    # charges brought against anyone in connection with this incident?
    arrests_or_charges_made = case_when(
      grepl('1', arrests_or_charges_made) ~ 'yes',
      grepl('2', arrests_or_charges_made) ~ 'no',
      grepl('9', arrests_or_charges_made) ~ 'oou'
    ),

    help_from_victim_agencies = case_when( # V4467
      grepl('1', help_from_victim_agencies) ~ 'yes',
      grepl('2', help_from_victim_agencies) ~ 'no',
      grepl('9', help_from_victim_agencies) ~ 'oou'
    ),


    # -----
    # series crimes

    how_many_times_incident_occur_last_6_mos = case_when( # V4016
      !is.na(as.integer(how_many_times_incident_occur_last_6_mos)) ~
        how_many_times_incident_occur_last_6_mos
    ),

    how_many_incidents = case_when( # V4017
      how_many_incidents == '(1) 1-5 incidents' ~ 'not a series',
      how_many_incidents == '(2) 6 > incidents' ~ 'series',
      grepl('universe', how_many_incidents) ~ 'oou'
    ),

    are_incidents_similar_in_detail = case_when( # V4018
      are_incidents_similar_in_detail == '(1) Similar' ~ 'series',
      are_incidents_similar_in_detail == '(2) Different' ~ 'not a series',
      are_incidents_similar_in_detail == '(9) Out of universe' ~ 'oou',
    ),

    enough_detail_to_distinguish_incidents = case_when( # V4019
      enough_detail_to_distinguish_incidents == '(1) Yes (not series)' ~ 'not a series',
      enough_detail_to_distinguish_incidents == '(2) No (is series)' ~ 'series',
      enough_detail_to_distinguish_incidents == '(9) Out of universe' ~ 'oou'
    ),

    # -----
    # characteristics of crime

    where_did_incident_happen = case_when( # V4024, all covered
      grepl('(01)|(02)|(03)|(04)',where_did_incident_happen) ~ 'respondents home or lodging',
      grepl('(05)|(06)|(07)', where_did_incident_happen) ~ 'near own home',
      grepl('(08)|(09)|(10)|(11)', where_did_incident_happen) ~ 'at, in, or near a friends/relatives/neighbors home',
      grepl('(12)|(13)|(14)|(24)|(25)|(26)|(27)', where_did_incident_happen) ~ 'commercial places',
      grepl('(15)|(16)|(17)', where_did_incident_happen) ~ 'parking lot/garages',
      grepl('(18)|(19)', where_did_incident_happen) ~ 'school',
      grepl('(20)|(21)|(22)', where_did_incident_happen) ~ 'open areas, on street or public transportation'
    ),

    in_what_city_town_village = case_when( # V4022
      in_what_city_town_village == '(1) Outside U.S.' ~ 'outside us',
      in_what_city_town_village == '(2) Not in city etc' ~ 'not in city etc',
      in_what_city_town_village == '(3) Same city etc' ~ 'same city etc',
      in_what_city_town_village == '(4) Diff city etc' ~ 'diff city etc',
      in_what_city_town_village == '(6) DK if 2, 4, or 5' ~ 'diff city'
    ),

    same_county_and_state_as_residence = case_when( # V4023
      same_county_and_state_as_residence == '(1) Yes' ~ 'yes',
      same_county_and_state_as_residence == '(2) No' ~ 'no',
      same_county_and_state_as_residence == '(9) Out of universe' ~ 'oou'
    ),

    how_far_from_home = case_when( # V4043
      how_far_from_home == '(1) R home/next door' ~ 'r home/next door',
      how_far_from_home == '(2) 1 mile or less' ~ '1 mile or less',
      how_far_from_home == '(3) 5 miles or less' ~ '5 miles or less',
      how_far_from_home == '(4) 50 miles or less' ~ '50 miles or less',
      how_far_from_home == '(5) > 50 miles' ~ '> 50 miles'
    ),

    incident_occur_at_work_site = case_when( # V4484
      grepl('1', incident_occur_at_work_site) ~ 'yes',
      grepl('2', incident_occur_at_work_site) ~ 'no',
      grepl('4', incident_occur_at_work_site) ~ 'no', # other -> NA
      grepl('9', incident_occur_at_work_site) ~ 'oou'
    ),


    about_what_time_did_incident_occur_start_1999_q1 =
      str_sub(about_what_time_did_incident_occur_start_1999_q1, start = 6) %>% tolower(),
    about_what_time_did_incident_occur_start_1999_q1 = case_when(
      about_what_time_did_incident_occur_start_1999_q1 == 'aft 6am-12am' ~ '6am-12am', # DAY
      about_what_time_did_incident_occur_start_1999_q1 == 'aft 12am-3pm' ~ '12am-3pm', # day
      about_what_time_did_incident_occur_start_1999_q1 == 'aft 3pm-6pm' ~ '3pm-6pm', # day
      about_what_time_did_incident_occur_start_1999_q1 == 'dk time of day' ~ 'day',

      about_what_time_did_incident_occur_start_1999_q1 == 'aft 6pm-9pm' ~ '6pm-9pm', # night
      about_what_time_did_incident_occur_start_1999_q1 == 'aft 9pm-12pm' ~ '9pm-12pm', # night
      about_what_time_did_incident_occur_start_1999_q1 == 'aft 12pm-6am' ~ '12pm-6am', # night
      about_what_time_did_incident_occur_start_1999_q1 == 'dk time of night' ~ 'night'
    ),


    offender_hit_or_attack_allocated = case_when( # V4060
      offender_hit_or_attack_allocated == '(1) Yes' ~ 'yes',
      offender_hit_or_attack_allocated == '(2) No' ~ 'no',
      offender_hit_or_attack_allocated == '(9) Out of universe' ~ 'oou'
    ),

    offender_try_to_attack_allocated = case_when(
      offender_try_to_attack_allocated == '(1) Yes' ~ 'yes',
      offender_try_to_attack_allocated == '(2) No' ~ 'no',
      offender_try_to_attack_allocated == '(9) Out of universe' ~ 'oou'
    ),


    injury = case_when( # V112 to 4119
      # serious injury (remove first three conditions?) # possibly change to
      # minor vs serious in terms of hospitalization. See page 148 of
      # here https://onlinelibrary-wiley-com.cmu.idm.oclc.org/doi/epdf/10.1111/j.1745-9125.2010.00182.x
      grepl('1', c_injuries_rape_injuries) |
        grepl('1', c_injuries_attempted_rape_injuries) |
      grepl('1', c_injuries_sexual_assault_injuries) |
        grepl('1', c_injuries_knife_stab_wounds) |
      grepl('1', c_injuries_gun_shot_bullet_wounds) |
        grepl('1', c_injuries_broken_bones_or_teeth) |
      grepl('1', c_injuries_internal_injuries) |
        grepl('1', c_injuries_knocked_unconscious) |
        grepl('1', c_med_care_emergency_room_clinic) |
        grepl('1', c_med_care_hospital_not_emergency_room) ~ 'serious injury',
      # since in NIBRS I consider only the most serious type of injury, code
      # this as 0 whenver a serious injury is present
      # residue -> yes
      grepl('1',c_injuries_bruises_cuts) |
        grepl('1', c_injuries_other_injuries) |
        grepl('8', residue_type_of_injury) ~ 'minor injury',
      # no need to code the out of universe values, which are based on V4110
      TRUE ~ 'no'),


    weapon = case_when( # V4051, V4052, V4057
      grepl('1', c_weapon_hand_gun) | #V4051
        grepl('1', c_weapon_other_gun) |  #V4052
        grepl('1', c_weapon_gun_type_unknown)  # V4057
      ~ 'firearm',
      # to match data processing in NIBRS,
      # consider only a firearm if firearm is present
      grepl('1', c_weapon_knife) | # V4053
        grepl('1', c_weapon_sharp_object) | # V4054
        grepl('1', c_weapon_blunt_object) | # V4055
        grepl('1', c_weapon_other) |  # V4056
        grepl('8', residue_type_of_weapon) ~ 'other weapon', # V4058
      TRUE ~ 'no'
    ),

    # -----
    # offender <-> victim

    is_offender_seen = case_when( # V4048
      did_you_personally_see_an_offender == '(1) Yes' ~ 'yes',
      did_you_personally_see_an_offender == '(2) No' ~ 'no',
      grepl('universe', did_you_personally_see_an_offender) ~ 'oou'
    ),

    is_multiple_offenders = case_when(# V4248
      number_of_offenders > 1 ~ 'yes',
      number_of_offenders == 1 ~ 'no',
      number_of_offenders == '(99) Out of universe' ~ 'oou'
    ),

    single_off_how_did_resp_know_offender = # V4243
      tolower(str_sub(single_off_how_did_resp_know_offender, start = 6)),
    single_off_how_did_resp_know_offender = case_when(
      single_off_how_did_resp_know_offender == 'out of universe' ~ 'oou',
      single_off_how_did_resp_know_offender != 'residue' ~ single_off_how_did_resp_know_offender,
    ),

    # relation for *multiple* offenders
    c_off_relation_spouse = case_when( # V4513
      c_off_relation_spouse == '(0) No' ~ 'no',
      c_off_relation_spouse == '(1) Yes' ~ 'yes',
      c_off_relation_spouse == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_exspouse = case_when( # V4514
      c_off_relation_exspouse == '(0) No' ~ 'no',
      c_off_relation_exspouse == '(1) Yes' ~ 'yes',
      c_off_relation_exspouse == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_parent = case_when( # V4515
      c_off_relation_parent == '(0) No' ~ 'no',
      c_off_relation_parent == '(1) Yes' ~ 'yes',
      c_off_relation_parent == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_other_relative = case_when( # V4516
      c_off_relation_other_relative  == '(0) No' ~ 'no',
      c_off_relation_other_relative  == '(1) Yes' ~ 'yes',
      c_off_relation_other_relative  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_friend_or_ex_friend = case_when( # V4517
      c_off_relation_friend_or_ex_friend  == '(0) No' ~ 'no',
      c_off_relation_friend_or_ex_friend  == '(1) Yes' ~ 'yes',
      c_off_relation_friend_or_ex_friend  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_neighbor = case_when( # V4518
      c_off_relation_neighbor  == '(0) No' ~ 'no',
      c_off_relation_neighbor  == '(1) Yes' ~ 'yes',
      c_off_relation_neighbor  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_schoolmate = case_when( # V4519
      c_off_relation_schoolmate  == '(0) No' ~ 'no',
      c_off_relation_schoolmate  == '(1) Yes' ~ 'yes',
      c_off_relation_schoolmate  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_roommate_boarder = case_when( # V4520
      c_off_relation_roommate_boarder  == '(0) No' ~ 'no',
      c_off_relation_roommate_boarder  == '(1) Yes' ~ 'yes',
      c_off_relation_roommate_boarder  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_other_nonrelative_end_2016_q4 = case_when( # V4521
      # note that this does not cover cases after 2016
      c_off_relation_other_nonrelative_end_2016_q4  == '(0) No' ~ 'no',
      c_off_relation_other_nonrelative_end_2016_q4  == '(1) Yes' ~ 'yes',
      c_off_relation_other_nonrelative_end_2016_q4  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_customer_client_start_2001_q3 = case_when( # V4522B
      c_off_relation_customer_client_start_2001_q3  == '(0) No' ~ 'no',
      c_off_relation_customer_client_start_2001_q3  == '(1) Yes' ~ 'yes',
      c_off_relation_customer_client_start_2001_q3  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_supervisor_cur_or_form_start_2001_q3 = case_when( # V4522C
      c_off_relation_supervisor_cur_or_form_start_2001_q3  == '(0) No' ~ 'no',
      c_off_relation_supervisor_cur_or_form_start_2001_q3  == '(1) Yes' ~ 'yes',
      c_off_relation_supervisor_cur_or_form_start_2001_q3  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_employee_cur_or_former_start_2001_q3 = case_when( # V4522D
      c_off_relation_employee_cur_or_former_start_2001_q3  == '(0) No' ~ 'no',
      c_off_relation_employee_cur_or_former_start_2001_q3  == '(1) Yes' ~ 'yes',
      c_off_relation_employee_cur_or_former_start_2001_q3  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_co_worker_cur_or_form_start_2001_q3 = case_when( # V4522E
      c_off_relation_co_worker_cur_or_form_start_2001_q3  == '(0) No' ~ 'no',
      c_off_relation_co_worker_cur_or_form_start_2001_q3  == '(1) Yes' ~ 'yes',
      c_off_relation_co_worker_cur_or_form_start_2001_q3  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_own_child_or_step_child_start_2007_q1 = case_when( # V4522F
      c_off_relation_own_child_or_step_child_start_2007_q1  == '(0) No' ~ 'no',
      c_off_relation_own_child_or_step_child_start_2007_q1  == '(1) Yes' ~ 'yes',
      c_off_relation_own_child_or_step_child_start_2007_q1  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_brother_sister_start_2007_q1 = case_when( # V4522G
      c_off_relation_brother_sister_start_2007_q1  == '(0) No' ~ 'no',
      c_off_relation_brother_sister_start_2007_q1  == '(1) Yes' ~ 'yes',
      c_off_relation_brother_sister_start_2007_q1  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_boyfriend_girlfriend_or_ex_start_2007_q1 = case_when( # V4522H
      c_off_relation_boyfriend_girlfriend_or_ex_start_2007_q1  == '(0) No' ~ 'no',
      c_off_relation_boyfriend_girlfriend_or_ex_start_2007_q1  == '(1) Yes' ~ 'yes',
      c_off_relation_boyfriend_girlfriend_or_ex_start_2007_q1  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_teacher_school_staff_start_2007_q1_end_2016_q4 = case_when( # V4522H
      c_off_relation_teacher_school_staff_start_2007_q1_end_2016_q4  == '(0) No' ~ 'no',
      c_off_relation_teacher_school_staff_start_2007_q1_end_2016_q4 == '(1) Yes' ~ 'yes',
      c_off_relation_teacher_school_staff_start_2007_q1_end_2016_q4  == '(9) Out of universe' ~ 'oou',
    ),
    c_off_relation_other_nonrelative_teacher_school_staff_start_2017_q1 = case_when(
      c_off_relation_other_nonrelative_teacher_school_staff_start_2017_q1  == '(0) No' ~ 'no',
      c_off_relation_other_nonrelative_teacher_school_staff_start_2017_q1  == '(1) Yes' ~ 'yes',
      c_off_relation_other_nonrelative_teacher_school_staff_start_2017_q1  == '(9) Out of universe' ~ 'oou',
    ),

    # relationship multiple offenders
    c_mult_off_known_by_sight_only = case_when(
      c_mult_off_known_by_sight_only == '(0) No' ~ 'no',
      c_mult_off_known_by_sight_only == '(1) Yes' ~ 'yes',
      c_mult_off_known_by_sight_only == '(9) Out of universe' ~ 'oou'
    ),
    c_mult_off_known_casual_acquaintance = case_when(
      c_mult_off_known_casual_acquaintance == '(0) No' ~ 'no',
      c_mult_off_known_casual_acquaintance == '(1) Yes' ~ 'yes',
      c_mult_off_known_casual_acquaintance == '(9) Out of universe' ~ 'oou'
    ),
    c_mult_off_known_well_known = case_when(
      c_mult_off_known_well_known == '(0) No' ~ 'no',
      c_mult_off_known_well_known == '(1) Yes' ~ 'yes',
      c_mult_off_known_well_known == '(9) Out of universe' ~ 'oou'
    ),
    li_mult_off_how_well_known = case_when(
      li_mult_off_how_well_known == '(1) At least 1 entry' ~ 'yes',
      li_mult_off_how_well_known == '(9) Out of universe' ~ 'oou'
    ),
    c_mult_off_spouse = case_when(
      c_mult_off_spouse  == '(0) No' ~ 'no',
      c_mult_off_spouse == '(1) Yes' ~ 'yes',
      c_mult_off_spouse  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_ex_spouse = case_when(
      c_mult_off_ex_spouse  == '(0) No' ~ 'no',
      c_mult_off_ex_spouse == '(1) Yes' ~ 'yes',
      c_mult_off_ex_spouse  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_parent_step = case_when(
      c_mult_off_parent_step  == '(0) No' ~ 'no',
      c_mult_off_parent_step == '(1) Yes' ~ 'yes',
      c_mult_off_parent_step  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_child_step = case_when(
      c_mult_off_child_step  == '(0) No' ~ 'no',
      c_mult_off_child_step == '(1) Yes' ~ 'yes',
      c_mult_off_child_step  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_brother_sister = case_when(
      c_mult_off_brother_sister  == '(0) No' ~ 'no',
      c_mult_off_brother_sister == '(1) Yes' ~ 'yes',
      c_mult_off_brother_sister  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_other_relative = case_when(
      c_mult_off_other_relative  == '(0) No' ~ 'no',
      c_mult_off_other_relative == '(1) Yes' ~ 'yes',
      c_mult_off_other_relative  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_boy_girlfriend_or_ex = case_when(
      c_mult_off_boy_girlfriend_or_ex  == '(0) No' ~ 'no',
      c_mult_off_boy_girlfriend_or_ex == '(1) Yes' ~ 'yes',
      c_mult_off_boy_girlfriend_or_ex  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_friend_or_ex_friend = case_when(
      c_mult_off_friend_or_ex_friend  == '(0) No' ~ 'no',
      c_mult_off_friend_or_ex_friend == '(1) Yes' ~ 'yes',
      c_mult_off_friend_or_ex_friend  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_roommate= case_when(
      c_mult_off_roommate  == '(0) No' ~ 'no',
      c_mult_off_roommate == '(1) Yes' ~ 'yes',
      c_mult_off_roommate  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_schoolmate= case_when(
      c_mult_off_schoolmate  == '(0) No' ~ 'no',
      c_mult_off_schoolmate == '(1) Yes' ~ 'yes',
      c_mult_off_schoolmate  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_neighbor= case_when(
      c_mult_off_neighbor  == '(0) No' ~ 'no',
      c_mult_off_neighbor == '(1) Yes' ~ 'yes',
      c_mult_off_neighbor  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_customer_client= case_when(
      c_mult_off_customer_client  == '(0) No' ~ 'no',
      c_mult_off_customer_client == '(1) Yes' ~ 'yes',
      c_mult_off_customer_client  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_other_nonrelative_end_2016_q4= case_when(
      c_mult_off_other_nonrelative_end_2016_q4  == '(0) No' ~ 'no',
      c_mult_off_other_nonrelative_end_2016_q4 == '(1) Yes' ~ 'yes',
      c_mult_off_other_nonrelative_end_2016_q4  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_patient_start_2001_q3= case_when(
      c_mult_off_patient_start_2001_q3  == '(0) No' ~ 'no',
      c_mult_off_patient_start_2001_q3 == '(1) Yes' ~ 'yes',
      c_mult_off_patient_start_2001_q3  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_supervisor_curr_or_former_start_2001_q3= case_when(
      c_mult_off_supervisor_curr_or_former_start_2001_q3  == '(0) No' ~ 'no',
      c_mult_off_supervisor_curr_or_former_start_2001_q3 == '(1) Yes' ~ 'yes',
      c_mult_off_supervisor_curr_or_former_start_2001_q3  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_employee_current_or_former_start_2001_q3= case_when(
      c_mult_off_employee_current_or_former_start_2001_q3  == '(0) No' ~ 'no',
      c_mult_off_employee_current_or_former_start_2001_q3 == '(1) Yes' ~ 'yes',
      c_mult_off_employee_current_or_former_start_2001_q3  == '(9) Out of universe' ~ 'oou',
    ),

    c_mult_off_co_worker_curr_or_former_start_2001_q3= case_when(
      c_mult_off_co_worker_curr_or_former_start_2001_q3  == '(0) No' ~ 'no',
      c_mult_off_co_worker_curr_or_former_start_2001_q3 == '(1) Yes' ~ 'yes',
      c_mult_off_co_worker_curr_or_former_start_2001_q3 == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_teacher_school_staff_start_2007_q1_end_2016_q4= case_when(
      c_mult_off_teacher_school_staff_start_2007_q1_end_2016_q4  == '(0) No' ~ 'no',
      c_mult_off_teacher_school_staff_start_2007_q1_end_2016_q4 == '(1) Yes' ~ 'yes',
      c_mult_off_teacher_school_staff_start_2007_q1_end_2016_q4  == '(9) Out of universe' ~ 'oou',
    ),
    c_mult_off_other_nonrelative_teacher_school_staff_start_2017_q1 = case_when(
      c_mult_off_other_nonrelative_teacher_school_staff_start_2017_q1  == '(0) No' ~ 'no',
      c_mult_off_other_nonrelative_teacher_school_staff_start_2017_q1  == '(1) Yes' ~ 'yes',
      c_mult_off_other_nonrelative_teacher_school_staff_start_2017_q1  == '(9) Out of universe' ~ 'oou',
    ),


    # -----
    ## characteristics of single offender

    # single_offender_ethnicity = case_when( # V4237A, valid from 2012 onwards
    #   # don't know -> not hispanic. Following what is done for the victim (residue)
    #   single_offender_hispanic_latino_start_2012_q1 == '(1) Yes' ~ 'hispanic',
    #   single_offender_hispanic_latino_start_2012_q1 == '(2) No' ~ 'not hispanic',
    #   single_offender_hispanic_latino_start_2012_q1 == '(9) Out of universe' ~ 'oou',
    #   single_offender_hispanic_latino_start_2012_q1 == '(3) Dont know' |
    #     single_offender_hispanic_latino_start_2012_q1 == '(8) Residue' ~ 'no'
    # ),

    single_offender_sex = case_when( # V4236, don't know -> NA
      single_offender_sex == '(1) Male' ~ 'male',
      single_offender_sex == '(2) Female' ~ 'female',
      single_offender_sex == '(9) Out of universe' ~ 'oou'
    ),

    single_offender_age = case_when( # V4237
      single_offender_age == '(1) Under 12' ~ '<12',
      single_offender_age == '(2) 12-14' ~ '12-14',
      single_offender_age == '(3) 15-17' ~ '15-17',
      single_offender_age == '(4) 18-20' ~ '18-20',
      single_offender_age == '(5) 21-29' ~ '21-29',
      single_offender_age == '(6) 30+' ~ '30+',
      single_offender_age == '(9) Out of universe' ~ 'oou'
    ),

     single_offender_stranger = case_when( # V4241
       single_offender_stranger == '(1) Knew/had seen' ~ 'knew/had seen',
       single_offender_stranger == '(2) Stranger' ~ 'stranger',
       single_offender_stranger == '(6) DK if 2 or 3' ~ 'stranger',
       single_offender_stranger == '(9) Out of universe' ~ 'oou'
     ),

    single_off_how_well_known = case_when( # V4243
      single_off_how_well_known == '(1) Sight only' ~ 'sight only',
      single_off_how_well_known == '(2) Casual acquaint' ~ 'casual acquaint',
      single_off_how_well_known == '(3) Well known' ~ 'well known',
      single_off_how_well_known == '(6) DK know if 2, 3' ~ 'casual acquaint/well known',
      single_off_how_well_known == '(9) Out of universe' ~ 'oou'
    ),

    single_offender_gang_member = case_when( # V4238
      single_offender_gang_member == '(1) Yes' ~ 'yes',
      single_offender_gang_member == '(2) No' ~ 'no',
      single_offender_gang_member == '(9) Out of universe' ~ 'oou'
    ),


    single_offender_drinking_drugs = case_when(
      single_offender_drinking_drugs == '(1) Yes' ~ 'yes',
      single_offender_drinking_drugs == '(2) No' ~ 'no',
      single_offender_drinking_drugs == '(9) Out of universe' ~ 'oou'
     # grepl('1|2|3|4', single_offender_drink_or_drugs) ~ 'yes', # V4239
    #  grepl('9', single_offender_drink_or_drugs) ~ 'no'
    ),

    #  Was this the only time this offender committed a crime or made threats
    #  against you or your household?
    single_off_only_crime_against_resp_or_hh = case_when( # V4247
      single_off_only_crime_against_resp_or_hh == '(1) Yes' ~ 'yes',
      single_off_only_crime_against_resp_or_hh == '(2) No' ~ 'no',
      single_off_only_crime_against_resp_or_hh == '(9) Out of universe' ~ 'oou'
    ),

    # -----
    ## multiple offenders

    number_of_offenders = case_when( # V4248
      !is.na(as.integer(number_of_offenders)) ~ number_of_offenders,
      number_of_offenders == '(99) Out of universe' ~ 'oou'
    ),

    # this can be improved
    multiple_offenders_sex = case_when(# V4249 & V4250
      multiple_offenders_sex == '(1) All male' ~ 'male',
      multiple_offenders_sex == '(2) All female' ~ 'female',
      (multiple_offenders_sex == '(4) Both m and f' &
         multiple_offenders_mostly_male_or_female == '(1) Mostly male') ~ 'male',
      (multiple_offenders_sex == '(4) Both m and f' &
         multiple_offenders_mostly_male_or_female == '(2) Mostly female') ~ 'female',
      (multiple_offenders_sex == '(4) Both m and f' &
         !grepl('1|2', multiple_offenders_mostly_male_or_female)) ~ 'mix',
      multiple_offenders_sex == '(9) Out of universe' ~ 'oou'
    ),

    multiple_offenders_age_of_oldest = case_when( # V4252
      multiple_offenders_age_of_oldest == '(1) Under 12' ~ '<12',
      multiple_offenders_age_of_oldest == '(2) 12-14' ~ '12-14',
      multiple_offenders_age_of_oldest == '(3) 15-17' ~ '15-17',
      multiple_offenders_age_of_oldest == '(4) 18-20' ~ '18-20',
      multiple_offenders_age_of_oldest == '(5) 21-29' ~ '21-29',
      multiple_offenders_age_of_oldest == '(6) 30+' ~ '30+',
      multiple_offenders_age_of_oldest == '(9) Out of universe' ~ 'oou'
    ),

    multiple_offenders_age_of_youngest = case_when( # V4252
      multiple_offenders_age_of_youngest == '(1) Under 12' ~ '<12',
      multiple_offenders_age_of_youngest == '(2) 12-14' ~ '12-14',
      multiple_offenders_age_of_youngest == '(3) 15-17' ~ '15-17',
      multiple_offenders_age_of_youngest == '(4) 18-20' ~ '18-20',
      multiple_offenders_age_of_youngest == '(5) 21-29' ~ '21-29',
      multiple_offenders_age_of_youngest == '(6) 30+' ~ '30+',
      multiple_offenders_age_of_youngest == '(9) Out of universe' ~ 'oou'
    ),

    multiple_offenders_gang_member = case_when( # V4253
      multiple_offenders_gang_member == '(1) Yes' ~ 'yes',
      multiple_offenders_gang_member == '(2) No' ~ 'no',
      multiple_offenders_gang_member == '(9) Out of universe' ~ 'oou'
    ),

    multiple_offenders_drinking_drugs = case_when( # V4254
      multiple_offenders_drinking_drugs == '(1) Yes' ~ 'yes',
      multiple_offenders_drinking_drugs == '(2) No' ~ 'no',
      multiple_offenders_drinking_drugs == '(9) Out of universe' ~ 'oou'
    ),

    multiple_offenders_all_strangers = case_when(# V4256
      multiple_offenders_all_strangers == '(1) All known' |
        multiple_offenders_all_strangers == '(2) Some known' |
        multiple_offenders_all_strangers == '(6) DK if 1 or 2' ~ 'no',
      multiple_offenders_all_strangers == '(3) All strangers' |
        multiple_offenders_all_strangers == '(5) DK if 3 or 4' ~ 'yes',
      multiple_offenders_all_strangers == '(9) Out of universe' ~ 'oou'
    ),

    multiple_offenders_only_crime = case_when( # V4286
      multiple_offenders_only_crime == '(1) Yes' ~ 'yes',
      multiple_offenders_only_crime == '(2) No' ~ 'no',
      multiple_offenders_only_crime == '(9) Out of universe' ~ 'oou'
    ),

    # -----
    # other variables
    activity_at_time_of_incident =
      tolower(str_sub(activity_at_time_of_incident, start = 6)) # V4478

  )

# -----

incident <- incident %>%
  mutate(

    # -----
    ## victim
    race_of_victim = case_when( # V3023A
      race_recode_start_2003_q1 == '(01) White only'~ 'white',
      race_recode_start_2003_q1 == '(02) Black only'~ 'black',
      race_recode_start_2003_q1 == '(04) Asian only' ~ 'other',
      race_recode_start_2003_q1 == '(03) Am Ind/AK native only' ~ 'other',
      race_recode_start_2003_q1 == '(05) Hawaiian/Pacific IS only' ~ 'other',
      race_recode_start_2003_q1 == '(06) White-Black' ~ '2+races',
      race_recode_start_2003_q1 == '(07) White-Amer Ind' ~ '2+races',
      race_recode_start_2003_q1 == '(08) White-Asian' ~ '2+races',
      race_recode_start_2003_q1 == '(09) White-Hawaiian' ~ '2+races',
      race_recode_start_2003_q1 == '(10) Black-Amer Ind' ~ '2+races',
      race_recode_start_2003_q1 == '(11) Black-Asian' ~ '2+races',
      race_recode_start_2003_q1 == '(12) Black-Hawaiian/Pacific Ils' ~ '2+races',
      race_recode_start_2003_q1 == '(13) American Indian-Asian' ~ 'other',
      race_recode_start_2003_q1 == '(14) Asian-Hawaiian/Pacific Ils' ~ '2+races',
      race_recode_start_2003_q1 == '(15) White-Black-American Ind' ~ '2+races',
      race_recode_start_2003_q1 == '(16) White-Black-Asian' ~ '2+races',
      race_recode_start_2003_q1 == '(17) White-Amer Ind-Asian' ~ '2+races',
      race_recode_start_2003_q1 == '(18) White-Asian-Hawaiian' ~ '2+races',
      race_recode_start_2003_q1 == '(19) 2 or 3 races' ~ '2+races',
      race_recode_start_2003_q1 == '(20) 4 or 5 races' ~ '2+races',
      TRUE ~ as.character('other')
    ), # valid from 2003

    ethnicity_of_victim = case_when( # V3024
      hispanic_origin == '(1) Yes' ~ 'hispanic',
      hispanic_origin == '(2) No' ~ 'not hispanic',
      hispanic_origin == '(8) Residue' ~ 'not hispanic' # following BJS
    )) %>%
  mutate(
    race_eth_of_victim = case_when(
      ethnicity_of_victim == 'hispanic' ~ 'hispanic',
      TRUE ~ race_of_victim
    )
  )


# -----

# fix offenders races >= 2012
incident_after12 <- incident %>% filter(ncvs_year >= 2012) %>%
  mutate(is_single_offender_multiple_races =
         ifelse(c_single_offender_race_white_start_2012_q1 == '(1) Yes', 1, 0) +
         ifelse(c_single_offender_race_black_or_african_american_start_2012_q1 == '(1) Yes', 1, 0) +
         ifelse(c_single_offender_race_american_indian_or_alaska_native_start_2012_q1 == '(1) Yes', 1, 0) +
         ifelse(c_single_offender_race_asian_start_2012_q1 == '(1) Yes', 1, 0) +
         ifelse(c_single_offender_race_native_hawaiian_or_other_pacific_islander_start_2012_q1 == '(1) Yes', 1, 0) +
         ifelse(c_single_offender_race_don_t_know_start_2012_q1 == '(1) Yes', 1, 0) +
         ifelse(residue_single_offender_race_start_2012_q1 == '(8) Residue', 1, 0)
  ) %>%
  mutate(is_multiple_offender_multiple_races =
           ifelse(c_mult_off_race_white == '(1) Yes', 1, 0) +
           ifelse(c_mult_off_race_black == '(1) Yes', 1, 0) +
           ifelse(c_mult_off_race_american_indian_or_alaska_native_start_2012_q1 == '(1) Yes', 1, 0) +
           ifelse(c_mult_off_race_asian_start_2012_q1 == '(1) Yes', 1, 0) +
           ifelse(c_mult_off_race_native_hawaiian_or_pacific_islander_start_2012_q1 == '(1) Yes', 1, 0) +
           ifelse(c_mult_off_race_don_t_know_race == '(1) Yes', 1, 0) #+
           # late addition
           #ifelse(residue_multiple_offender_race == '(8) 1 > out of range', 1, 0)
  )

incident_after12 <- incident_after12 %>%
  mutate(race_eth_of_offender = case_when(
    # remember that the order matters

    # hispanic
    single_offender_hispanic_latino_start_2012_q1 == '(1) Yes' ~ 'hispanic',
    multiple_offenders_hispanic_latino_start_2012_q1 == '(1) Yes' ~ 'hispanic',

    # black
    c_single_offender_race_black_or_african_american_start_2012_q1 == '(1) Yes' &
      is_single_offender_multiple_races == 1 ~ 'black',
    c_mult_off_race_black == '(1) Yes' &
      is_multiple_offender_multiple_races == 1 ~ 'black',

    # white
    c_single_offender_race_white_start_2012_q1 == '(1) Yes' &
      is_single_offender_multiple_races == 1 ~ 'white',
    c_mult_off_race_white == '(1) Yes' &
      is_multiple_offender_multiple_races == 1  ~ 'white',

    # other
    c_single_offender_race_american_indian_or_alaska_native_start_2012_q1 == '(1) Yes' &
      is_single_offender_multiple_races == 1 ~ 'other',
    c_mult_off_race_american_indian_or_alaska_native_start_2012_q1 == '(1) Yes' &
      is_multiple_offender_multiple_races == 1 ~ 'other',

    c_single_offender_race_asian_start_2012_q1 == '(1) Yes' &
      is_single_offender_multiple_races == 1 ~ 'other',
    c_mult_off_race_asian_start_2012_q1 == '(1) Yes' &
      is_multiple_offender_multiple_races == 1 ~ 'other',

    c_single_offender_race_native_hawaiian_or_other_pacific_islander_start_2012_q1 == '(1) Yes' &
      is_single_offender_multiple_races == 1 ~ 'other',
    c_mult_off_race_native_hawaiian_or_pacific_islander_start_2012_q1 == '(1) Yes' &
      is_multiple_offender_multiple_races == 1  ~ 'other',

    # unknown single off
    c_single_offender_race_don_t_know_start_2012_q1 == '(1) Yes' &
      is_single_offender_multiple_races == 1 ~ 'unknown',

    # by reading the documentation, I think there should be this exclusion
    # but BJS does not seem to adopt it
    #residue_single_offender_race_start_2012_q1 == '(8) Residue' &
    #  is_single_offender_multiple_races == 1 ~ 'unknown',

    # single 2+ races (includes 2+ races specified and residue > 1 out of range)
    li_single_offender_race_start_2012_q1 == '(1) At least 1 entry' ~ 'single 2+races',

    # unknown multiple off
    c_mult_off_race_don_t_know_race == '(1) Yes' &
      is_multiple_offender_multiple_races == 1  ~ 'unknown',

    # multiple off
    li_multiple_offender_race == '(1) At least 1 entry' ~ 'mult 2+races',
    TRUE ~ 'unknown' # potentially change the unknown to NA
  )) %>%
  mutate( # identical as above but no hispanics included
    race_of_offender = case_when(
      # black
      c_single_offender_race_black_or_african_american_start_2012_q1 == '(1) Yes' &
        is_single_offender_multiple_races == 1 ~ 'black',
      c_mult_off_race_black == '(1) Yes' & c_mult_off_race_white != '(1) Yes' ~ 'black',
        #is_multiple_offender_multiple_races == 1 ~ 'black',

      # white
      c_single_offender_race_white_start_2012_q1 == '(1) Yes' &
        is_single_offender_multiple_races == 1 ~ 'white',
      c_mult_off_race_white == '(1) Yes' & c_mult_off_race_black != '(1) Yes'  ~ 'white',
        # is_multiple_offender_multiple_races == 1  ~ 'white',

      # keep white and black
      c_mult_off_race_white == '(1) Yes' & c_mult_off_race_black == '(1) Yes' ~ 'mix white_black',

      # other
      c_single_offender_race_american_indian_or_alaska_native_start_2012_q1 == '(1) Yes' &
        is_single_offender_multiple_races == 1 ~ 'other',
      c_mult_off_race_american_indian_or_alaska_native_start_2012_q1 == '(1) Yes' &
        is_multiple_offender_multiple_races == 1 ~ 'other',

      c_single_offender_race_asian_start_2012_q1 == '(1) Yes' &
        is_single_offender_multiple_races == 1 ~ 'other',
      c_mult_off_race_asian_start_2012_q1 == '(1) Yes' &
        is_multiple_offender_multiple_races == 1 ~ 'other',

      c_single_offender_race_native_hawaiian_or_other_pacific_islander_start_2012_q1 == '(1) Yes' &
        is_single_offender_multiple_races == 1 ~ 'other',
      c_mult_off_race_native_hawaiian_or_pacific_islander_start_2012_q1 == '(1) Yes' &
        is_multiple_offender_multiple_races == 1  ~ 'other',


      # unknown single off
      c_single_offender_race_don_t_know_start_2012_q1 == '(1) Yes' &
        is_single_offender_multiple_races == 1 ~ 'unknown',

      # single 2+ races (includes 2+ races specified and residue)
      li_single_offender_race_start_2012_q1 == '(1) At least 1 entry' ~ 'single 2+races',

      # unknown multiple off
      c_mult_off_race_don_t_know_race == '(1) Yes' &
        is_multiple_offender_multiple_races == 1  ~ 'unknown',

      # multiple off
      li_multiple_offender_race == '(1) At least 1 entry' ~ 'mult 2+races',
      TRUE ~ 'unknown' # potentially change the unknown to NA
    )
  ) %>%
  select(-is_multiple_offender_multiple_races, -is_single_offender_multiple_races)

# fix race before 2012
incident_before12 <- incident %>% filter(ncvs_year < 2012) %>%
  mutate(
    is_multiple_offender_multiple_races =
      ifelse(c_mult_off_race_white == '(1) Yes', 1, 0) +
      ifelse(c_mult_off_race_black == '(1) Yes', 1, 0) +
      ifelse(c_mult_off_race_other_race_end_2011_q4 == '(1) Yes', 1, 0) +
      ifelse(c_mult_off_race_don_t_know_race == '(1) Yes', 1, 0) +
      ifelse(residue_multiple_offender_race == '(8) 1 > out of range', 1, 0)
  )
incident_before12 <- incident_before12 %>%
  mutate( #
    race_of_offender = case_when(

      # white
      single_offender_race_end_2011_q4 == '(1) White' ~ 'white',
      c_mult_off_race_white == '(1) Yes' &
        is_multiple_offender_multiple_races == 1  ~ 'white',

      # black
      single_offender_race_end_2011_q4 == '(2) Black' ~ 'black',
      c_mult_off_race_black == '(1) Yes' &
        is_multiple_offender_multiple_races == 1 ~ 'black',

      # other
      single_offender_race_end_2011_q4 == '(3) Other' ~ 'other',
      c_mult_off_race_other_race_end_2011_q4 == '(1) Yes' &
        is_multiple_offender_multiple_races == 1 ~ 'other',

      single_offender_race_end_2011_q4 == '(4) Dont know' ~ 'unknown',

      # unknown multiple off
      c_mult_off_race_don_t_know_race == '(1) Yes' &
        is_multiple_offender_multiple_races == 1  ~ 'unknown',

      # multiple off
      li_multiple_offender_race == '(1) At least 1 entry' ~ 'mult 2+races',
      TRUE ~ 'unknown'
    ),
    race_eth_of_offender = NA
  )

# stack the two datasets
incident <- incident_before12 %>%
  bind_rows(incident_after12)

# -----
# subset variables

incident <- incident %>%
select(

  # structural info
  ncvs_id_for_households, stratum, psu,
  ncvs_year, household_weight,
  adjusted_household_weight_collection_year, person_weight,
  adjusted_person_weight_collection_year, 
  incident_weight_adjusted_for_series_crimes, 
  victimization_weight_adjusted_for_series_crimes,
  adjusted_victimization_weight_collection_year,
  incident_weight_end_2015_q4_start_2019_q1,
  census_victimization_weight_start_2005_q1_end_2016_q4_start_2019_q1,
  

  # location of person
  msa_status,  region_where_household_is,


  # -----
  # characteristics of crime
  crime_type, crime_recode,


  reported_to_police, how_did_police_find_out, police_came_when_notified,
  most_important_reason_not_reported, reason_not_reported,
  c_reason_not_rptd_advised_not_to_report,c_reason_not_rptd_personal_matter,
  c_reason_not_rptd_not_clear_a_crime, c_reason_not_rptd_lack_of_proof,
  c_reason_not_rptd_police_biased, c_reason_not_rptd_not_import_to_police,
  c_reason_not_rptd_police_inefficient,c_reason_not_rptd_offender_police_offcr,
  c_reason_not_rptd_protect_offender, c_reason_not_rptd_fear_of_reprisal,
  most_important_reason_for_report, how_soon_did_police_respond,
  li_type_of_police_response, c_police_resp_took_report,c_police_resp_searched,
  c_police_resp_took_evidence, c_police_resp_questioned_witness_susp,
  c_police_resp_promised_investigate,c_police_resp_promised_surveillance,
  c_police_resp_made_arrest, arrests_or_charges_made, help_from_victim_agencies,


  number_of_victims, race_of_victim, ethnicity_of_victim,
  sex_of_victim, age_of_victim, race_of_victim, race_eth_of_victim,

  single_off_only_crime_against_resp_or_hh,
  how_many_times_incident_occur_last_6_mos, how_many_incidents,
  are_incidents_similar_in_detail, enough_detail_to_distinguish_incidents,

  where_did_incident_happen, incident_occur_at_work_site, how_far_from_home,
  in_what_city_town_village, injury, weapon,
  about_what_time_did_incident_occur_start_1999_q1,
  same_county_and_state_as_residence, offender_try_to_attack_allocated,
  offender_hit_or_attack_allocated, activity_at_time_of_incident,


  is_offender_seen, race_eth_of_offender, race_eth_of_victim, race_of_offender,
  single_offender_sex, single_offender_age, single_offender_stranger,
  single_offender_gang_member, single_offender_drinking_drugs,

  number_of_offenders,
  multiple_offenders_sex, # multiple_offender_race_of_most,
  multiple_offenders_age_of_youngest,
  multiple_offenders_age_of_oldest,
  multiple_offenders_hispanic_latino_start_2012_q1,
  multiple_offenders_gang_member,
  multiple_offenders_drinking_drugs,
  multiple_offenders_all_strangers,
  multiple_offenders_only_crime,


  single_off_how_well_known, single_off_how_did_resp_know_offender,
  c_off_relation_spouse, c_off_relation_exspouse, c_off_relation_parent,
  c_off_relation_other_relative,
  c_off_relation_friend_or_ex_friend, c_off_relation_neighbor,
  c_off_relation_schoolmate, c_off_relation_roommate_boarder,
  c_off_relation_other_nonrelative_end_2016_q4, 
  c_off_relation_customer_client_start_2001_q3,
  c_off_relation_supervisor_cur_or_form_start_2001_q3,
  c_off_relation_employee_cur_or_former_start_2001_q3,
  c_off_relation_co_worker_cur_or_form_start_2001_q3,
  c_off_relation_own_child_or_step_child_start_2007_q1,
  c_off_relation_brother_sister_start_2007_q1,
  c_off_relation_boyfriend_girlfriend_or_ex_start_2007_q1,
  c_off_relation_teacher_school_staff_start_2007_q1_end_2016_q4,
  c_off_relation_other_nonrelative_teacher_school_staff_start_2017_q1,
  # multiple
  c_mult_off_known_by_sight_only, c_mult_off_known_casual_acquaintance,
  c_mult_off_known_well_known, li_mult_off_how_well_known,
  c_mult_off_spouse,c_mult_off_ex_spouse,
  c_mult_off_parent_step,
  c_mult_off_other_relative,
  c_mult_off_child_step,c_mult_off_brother_sister,
  c_mult_off_other_nonrelative_end_2016_q4,
  c_mult_off_boy_girlfriend_or_ex,
  c_mult_off_friend_or_ex_friend,c_mult_off_roommate,
  c_mult_off_schoolmate,c_mult_off_neighbor,c_mult_off_customer_client,
  c_mult_off_patient_start_2001_q3,
  c_mult_off_supervisor_curr_or_former_start_2001_q3,
  c_mult_off_employee_current_or_former_start_2001_q3,
  c_mult_off_co_worker_curr_or_former_start_2001_q3,
  c_mult_off_teacher_school_staff_start_2007_q1_end_2016_q4,
  c_mult_off_other_nonrelative_teacher_school_staff_start_2017_q1
)


incident %>%
  rename(year = ncvs_year) %>%
  write_csv(here('data', 'ncvs', 'incident.csv'))
