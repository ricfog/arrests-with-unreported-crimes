
suppressMessages(library(tidyverse))
suppressMessages(library(here))

# -----

files <- list.files(here("data", "nibrs"))
files <- files[grepl('csv', files)]
files <- files[grepl(paste0(2006:2015, collapse = '|'), files)]
years_sel <- parse_number(files) %>% unique()

# consider only incidents with crime types we are interested in
crime_types <- paste0(c(
  "sex offense", 'aggravated assault', 'simple assault', 'robbery'
),
collapse = "|"
)
which_incidents <- files[grepl("offense", files)] %>%
  purrr::map(~ mutate(read_csv(here("data", "nibrs", .),
    col_select = c(
      originating_agency_identifier,
      incident_number,
      ucr_offense_code
    ),
    show_col_types = FALSE), year = parse_number(.)) %>%
    filter(grepl(crime_types, ucr_offense_code)) %>%
    distinct(originating_agency_identifier, incident_number, year)) %>%
  bind_rows()
which_incidents %>% count(year)

# write offenders file ----
files[grepl("offender", files)] %>%
  purrr::map(~ mutate(read_csv(here("data", "nibrs", .),
    col_select = c(
      age_of_offender, sex_of_offender, race_of_offender, ethnicity_of_offender,
      originating_agency_identifier, incident_number
    ),
    col_types = cols(
      age_of_offender = col_integer(),
      ethnicity_of_offender = col_character()
    )
  ), year = parse_number(.)) %>%
    inner_join(which_incidents %>%
      filter(year == parse_number(.x)),
    by = c("originating_agency_identifier", "incident_number", "year")
    )) %>%
  bind_rows() %>%
  write_csv(here("data", "nibrs", "offender.csv"))

# write arrestee file ----
files[grepl("arrestee", files)] %>%
  purrr::map(~ mutate(read_csv(here("data", "nibrs", .),
    col_select = c(
      age_of_arrestee, sex_of_arrestee, race_of_arrestee, ethnicity_of_arrestee, 
      ucr_arrest_offense_code,
      originating_agency_identifier, incident_number
    ),
    col_types = cols(
      age_of_arrestee = col_integer(),
      ethnicity_of_arrestee = col_character()
    )
  ), year = parse_number(.)) %>%
    inner_join(which_incidents %>%
      filter(year == parse_number(.x)),
    by = c("originating_agency_identifier", "incident_number", "year")
    )) %>%
  bind_rows() %>%
  write_csv(here("data", "nibrs", "arrestee.csv"))


# write offense file ----
files[grepl("offense", files)] %>%
  purrr::map(~ mutate(read_csv(here("data", "nibrs", .),
    col_select = c(
      originating_agency_identifier, incident_number, incident_date, ucr_offense_code,
      offense_attempted_completed, use_alcohol, use_drugs, location_type,
      type_sex_offense,
      weapon_force, weapon_force_2, weapon_force_3
    ),
    col_types = cols(weapon_force = col_character())
  ), year = parse_number(.)) %>%
    inner_join(which_incidents %>%
      filter(year == parse_number(.x)),
    by = c("originating_agency_identifier", "incident_number", "year")
    )) %>%
  bind_rows() %>%
  write_csv(here("data", "nibrs", "offense.csv"))

# victim file ----
files[grepl("victim", files)] %>%
  purrr::map(~ mutate(read_csv(here("data", "nibrs", .),
    col_select = c(
      originating_agency_identifier, incident_number,
      age_of_victim, 
      relationship_vic_to_off_1, 
      relationship_vic_to_off_2, relationship_vic_to_off_3,
      relationship_vic_to_off_4, relationship_vic_to_off_5,
      relationship_vic_to_off_6, relationship_vic_to_off_7,
      relationship_vic_to_off_8, relationship_vic_to_off_9, 
      relationship_vic_to_off_10,
      injury,
      sex_of_victim, race_of_victim, ethnicity_of_victim,
      type_of_victim, `type_of_activity_(officer)` # ,
      # additional_justifiable_homicide_circumstances
    ),
    col_types = cols(age_of_victim = col_character())
  ),
  year = parse_number(.)
  ) %>%
    inner_join(which_incidents %>%
      filter(year == parse_number(.x)),
    by = c("originating_agency_identifier", "incident_number", "year")
    )) %>%
  bind_rows() %>%
  write_csv(here("data", "nibrs", "victim.csv"))

# write admin file ----
files[grepl("admin", files)] %>%
  purrr::map(~ mutate(read_csv(here("data", "nibrs", .),
    col_types = cols(),
    show_col_types = FALSE
  ), year = parse_number(.)) %>%
    select(-segment_level, -incident_date_hour) %>%
    mutate(incident_hour = as.character(incident_hour)) %>%
    inner_join(which_incidents %>%
      filter(year == parse_number(.x)),
    by = c("originating_agency_identifier", "incident_number", "year")
    ) %>%
    inner_join(
      read_csv(here("data", "nibrs", paste0("batch_", parse_number(.x), ".csv")),
        col_types = cols(),
        show_col_types = FALSE
      ),
      by = c("originating_agency_identifier")
    )) %>%
  bind_rows() %>%
  left_join(
    read_csv(here("data", "leoka.csv")),
    by = c("originating_agency_identifier", "year")
  ) %>%
  write_csv(here("data", "nibrs", "admin_batch.csv"))


