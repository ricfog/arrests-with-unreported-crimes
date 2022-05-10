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

* **clean_ncvs.R**: Clean the NCVS data file.
* **ncvs\_create\_offensedata.R**: Create offense data (see below).
* **ncvs_create\_valuesimpdata.R**: Impute missing values via the
  [mice](https://github.com/amices/mice) package. 

To clean the data, run

```
Rscript process-data/ncvs_clean.R
```
The processed dataset is saved in the `data/ncvs` directory.
To allow for the creation of datasets including different combinations of crime
types/multiple victims or offenders/racial groups, I use a code made of four
digits, e.g., '0110'. The digits correspond to

* 1st digit: 0 = data between 2003-last year available (e.g., 2020), 1 = data
  between 2006-2015 (included). We drop 2016 data. 
* 2nd digit: 0 = incidents with multiple victims/offenders, 1 = data with only
  one victim/offender.
* 3rd digit: 0 = verbal threat of assault included, 1 = verbal threat of assault
  excluded.
* 4th digit: 0 = only race information is kept and Hispanics are included, 1 =
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

* **nibrs\_clean\_nibrs.R**: Extract relevant information from each annual-year
  data file from NIBRS.
* **nibrs\_create\_jointdata.R**: Merge NIBRS and LEOKA data across multiple
  years. In this step, all incidents containing at least one violent offense are
  still included in the data (i.e., none of the exclusions mentioned in the
  paper applies other than the restriction for offense type).
* **nibrs\_create\_offensedata.R**: Create separate datasets for each offense.
  Every observation (row) in the data corresponds to one incident. Only the
  incidents with one Black/White victim (including Hispanics, ethnicity
  information is also kept), one Black/White offender (including Hispanics), and
  that occurred in the 16 states considered in the analysis are kept.
* **nibrs\_create\_valuesimpdata.R**: Impute missing values for each of the
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

All datasets are saved in the `data/nibrs` directory. Note that the *one* NIBRS
dataset that you will obtain by running the code above for each offense type is
"compatible" with only one of the NCVS datasets that are generated, i.e., they
will contain data of incidents with similar characteristics. 