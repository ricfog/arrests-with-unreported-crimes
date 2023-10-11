# Data processing

## Download data

Download NIBRS, LEOKA, LEAIC, and NIBRS data from the ICPSR using the
[icpsrdata](https://github.com/fsolt/icpsrdata) package. Alternatively, you can
download the data manually. To obtain the data, run

```
Rscript process-data/acquire_data.R
```

The dataset is saved in the `downloads` directory.

## NCVS data

The relevant files are

- **clean_ncvs.R**: Clean the NCVS data file.
- **ncvs_create_offensedata.R**: Create offense data (see below).
- **ncvs_create_valuesimpdata.R**: Impute missing values via the
  [mice](https://github.com/amices/mice) package.

To clean the data, run

```
Rscript process-data/ncvs_clean.R
```

The processed dataset is saved in the `data/ncvs` directory.
To allow for the creation of datasets including different combinations of crime
types/multiple victims or offenders/racial groups, I use a code made of four
digits, e.g., '0110'. The digits correspond to

- 1st digit: 0 = data between 2003-last year available (e.g., 2020), 1 = data
  between 2006-2015 (included). We drop 2016 data.
- 2nd digit: 0 = incidents with multiple victims/offenders, 1 = data with only
  one victim/offender.
- 3rd digit: 0 = verbal threat of assault included, 1 = verbal threat of assault
  excluded.
- 4th digit: 0 = only race information is kept and Hispanics are included, 1 =
  ethnicity information is kept and Hispanics are excluded (exclusion works only post 2012).

In practice, for the analysis I use the codes '0110' and '0010'.
To create the data containing incidents that involve the type of offenses that are
relevant to the analysis, run

```
Rscript process-data/ncvs_create_offensedata.R 0010;
Rscript process-data/ncvs_create_offensedata.R 0110;
```

Impute the missing values with

```
Rscript process-data/ncvs_create_valuesimpdata.R 0010;
Rscript process-data/ncvs_create_valuesimpdata.R 0110;
```

## NIBRS

The relevant files for the analysis of crimes with one offender are

- **nibrs_clean_nibrs.R**: Extract relevant information from each annual-year
  data file from NIBRS.
- **nibrs_create_jointdata.R**: Merge NIBRS and LEOKA data across multiple
  years. In this step, all incidents containing at least one violent offense are
  still included in the data (i.e., none of the exclusions mentioned in the
  paper applies other than the restriction for offense type).
- **nibrs_create_offensedata.R**: Create separate datasets for each offense.
  Every observation (row) in the data corresponds to one incident. Only the
  incidents with one Black/White victim (including Hispanics, ethnicity
  information is also kept), one Black/White offender (including Hispanics), and
  that occurred in the 16 states considered in the analysis are kept.
- **nibrs_create_valuesimpdata.R**: Impute missing values for each of the
  offense datasets.

To process the data and impute the missing values for both incidents with only
one offenders and those with one or more offenders, run

```
Rscript process-data/leoka_clean.R;
Rscript process-data/leaic_clean.R;
Rscript process-data/nibrs_clean.R;
Rscript process-data/nibrs_create_jointdata.R;
Rscript process-data/nibrs_create_offensedata.R;
Rscript process-data/nibrs_create_valuesimpdata.R 'sex offense';
Rscript process-data/nibrs_create_valuesimpdata.R 'robbery';
Rscript process-data/nibrs_create_valuesimpdata.R 'aggravated assault';
Rscript process-data/nibrs_create_valuesimpdata.R 'simple assault';

Rscript process-data/nibrs_create_offensedata_multiple.R;
Rscript process-data/nibrs_create_valuesimpdata.R 'sex offense' mult;
Rscript process-data/nibrs_create_valuesimpdata.R 'robbery' mult;
Rscript process-data/nibrs_create_valuesimpdata.R 'aggravated assault' mult;
Rscript process-data/nibrs_create_valuesimpdata.R 'simple assault' mult;
```

All datasets are saved in the `data/nibrs` directory. Note that the _one_ NIBRS
dataset that you will obtain by running the code above for each offense type is
"compatible" with only one of the NCVS datasets that are generated, i.e., they
will contain data of incidents with similar characteristics.

## Data Processing

We provide further details on the construction of variables on
NCVS and NIBRS data. For each of the variables considered, we list their coded
values and, within parentheses, their corresponding values in the original data.
We describe the construction of the variable differs across the analyses of
incidents with individual and multiple offenders, we describe each coding
separately indicating them with (i) and (ii) respectively. We start with the NCVS:

- crime is known to police.
- type of crime: sex offense (completed and attempted rape, sexual
  assaults, unwanted sex, verbal threats of rape, and sexual assault), robbery
  (attempted and completed robbery), aggravated assault (attempted or completed
  aggravated assault, threat of assault with weapon), simple assault (simple
  assault and assault without weapon without injury).
- race of victim: (i) black, white.
- age of victim: years.
- sex of victim: male, female.
- race of offender: black, white. (ii) for incidents with multiple offenders, black (at least one of the offenders is black, none is white), white (at least one of the offenders is white, none is black), mix (at least one offender is black and one is white).
- age of offender: (i) below 12, between 12 and 14, between 15 and 17,
  between 18 and 20, between 20 and 29, and 30 or above. (ii) age of youngest, age
  of oldest (same coding as (i)). If there is only one offender, both variables take the same value.
- sex of offender: (i) male, female. (ii) male (all offenders are male,
  majority of offenders are male), female (all offenders are female, majority of
  offenders are female), mix (some offenders are male, others are female).
- relationship of victim and offenders: (i) offender is intimate partner of victim (boyfriend or girlfriend, ex-boyfriend
  or ex-girlfriend, ex-spouse or spouse), family member (brother or sister, other
  relative, parent or step-parent, child or step-child), unrelated acquaintance
  (acquaintance, well known), stranger (stranger, known only by sight). (ii) at least one of the offenders is known to the victim, all offenders are strangers.
- time of the day at which the incident occurred: daytime (between 6am and
  6pm), at night (otherwise).
- injury suffered from the victim: serious injury (injuries due to completed
  rape, attempted rape, or sexual assault. Stab wounds, gunshot wounds, broken
  bones or teeth, internal injuries. Victim was knocked unconscious, had to go to
  the emergency room or needed hospitalization), minor injury (bruises, cuts, or
  other injuries), no injury (otherwise).
- weapon involved: firearm (gun), other weapon (knife, sharp or blunt
  object, other weapon), no weapon (otherwise).
- offense is only attempted: attempted offense (attempted rape, verbal
  threat of rape, verbal threat of sexual assault, attempted robbery), completed
  offense (all other cases). We follow the convention of NIBRS and classify all
  aggravated assaults, including those that are recorded as attempted crimes, as
  completed offenses \citep[p.17]{nibrs_2019}.
- whether the incident occurred in: private location (at, in, or near a
  friends/relatives/neighbors home, near own home, respondents home or lodging),
  public location (school, parking lot/garages, open areas, on street or public
  transportation, commercial places).
- incident occurred in: core city of MSA (if victim resides in a core city
  of an MSA and the incident occurs in the same city where the victim resides), in
  MSA but not in core city (if victims resides in an MSA, but not in its core
  city, and the incident occurs in the same city or village where the victim
  resides), not in MSA (if the victim does not reside in an MSA and the incident
  occurs in the same city or village where the victim resides).
- region where the incident occurred: Northeast, Midwest, South, and West.

For each of the variables, we code the values as missing when none of the
specified criteria are met. We now turn to NIBRS. We discuss only the variables whose construction differed
from those in the NCVS.

- crime: sex offense (rape, sodomy, fondling, sexual assault with an object,
  statutory rape and incest are excluded), robber, aggravated assault, simple
  assault.
- offender is arrested. See Section \ref{sec:data_processing} for a discussion of the crimes cleared by exceptional means.
- age of offender: for variables in $X$, years. For variables in $Z$, same as in the NCVS.
- sex of offender: for $X$, sex of the individual offender.
- race of offender: for $X$, race of the individual offender.
- relationship of victim and offenders: single offender or any of the
  offenders is intimate partner of victim (boyfriend or girlfriend, spouse or
  ex-spouse, common-law spouse, homosexual relationship), family member (sibling,
  stepsibling, parent, stepparent, child, stepchild, child of boyfriend or
  girlfriend, grandchild, grandparent, in-law, other family member), unrelated
  acquaintance (friend, acquaintance, neighbor, babysitee, child of boyfriend or
  girlfriend, employee, employer, otherwise known), stranger (stranger,
  undetermined relationship, or unknown).
- whether the incident occurred in: private location (residence or hotel),
  public location (otherwise).
- weapon involved: firearm (firearm, handgun, rifle, shotgun), other weapon
  (poison drugs, knife or cutting instrument, blunt object, fire or incendiary
  device, drugs or narcotics, motor vehicle, other weapon), no weapon (otherwise).
- incident occurred in: core city of MSA, in MSA but not in core city, not
  in MSA.
- state where the incident occurred.
- population served by the police agency where the incident was recorded (ORI).
- number of police officers per capita (ORI).
