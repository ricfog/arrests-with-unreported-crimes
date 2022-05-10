## Estimation of the likelihood of police notification on NCVS

Estimate the likelihood of police notification on NCVS data via logistic
regression with survey weights using
```
Rscript analysis/ncvs_estimate_weights_logistic.R '0110';
Rscript analysis/ncvs_estimate_weights_logistic.R '0010';
```
and via the SuperLearner with
```
Rscript analysis/ncvs_estimate_weights_superlearner.R '0110' 50;
Rscript analysis/ncvs_estimate_weights_superlearner.R '0010' 50;
```
For this analysis, you will need to have TensorFlow installed. The code to
install it is commented out in the R file. The last number on each line
indicates the number of cores the process is parallelized onto. 


## Estimation of the likelihood of police notification on NIBRS

Obtain the predictions of the likelihood of police notification on NIBRS
with
```
Rscript analysis/nibrs_get_weights.R;
Rscript analysis/nibrs_get_weights.R mult;
```
The code in the second line generates the predictions for the incidents with one
or more offenders. 


## Analysis of NIBRS

Conduct an exploratory data analysis of NIBRS with
```
Rscript analysis/nibrs_eda;
Rscript analysis/nibrs_eda_mult;
```

Fit the regression models with
```
Rscript analysis/nibrs_fit_regression.R;
Rscript analysis/nibrs_fit_regression_multiple.R;
```

Run the sensitivty analysis and the model diagnostics using
```
Rscript analysis/sensitivity.R;
Rscript analysis/nibrs_modeldiagnostics;
```

`utils.R` and `utils_regression.R` include functions used throughout the analysis.
