# Profile Imbalance Analysis

This repository contains the R and Python code used for the statistical analyses accompanying the manuscript. The analyses comprise psychometric evaluation of the study measures and Response Surface Analysis (RSA) of within-person profile alignment and imbalance.

## Psychometric Analysis

`psychometric_analysis.R` contains the psychometric analyses of the HiPIC, BESSI, and E-SWAN-G measures.

The script evaluates the proposed factor structures using confirmatory factor analysis (CFA), including examination of model fit and theoretically motivated model refinements. For BESSI and E-SWAN-G, alternative higher-order factor structures are also evaluated. For E-SWAN-G, additional exploratory factor analysis and bifactor models are examined.

The script also assesses internal consistency and test–retest reliability and constructs the final scale scores used in the subsequent analyses.

The analyses were conducted in R using packages including `lavaan`, `semTools`, `psych`, and `dplyr`.

## Response Surface Analysis

`rsa_analysis.py` contains the Response Surface Analysis (RSA) used to examine whether within-person alignment and imbalance between characteristics are associated with depression and anxiety symptoms (RCADS).

For each eligible pair of indicators, the script estimates a polynomial regression model and derives the standard RSA surface parameters:

* `a1` — linear slope along the line of congruence
* `a2` — curvature along the line of congruence
* `a3` — linear slope along the line of incongruence
* `a4` — curvature along the line of incongruence

Polynomial models are compared with corresponding linear models using nested-model F tests. Age and sex are included as covariates, and continuous variables are standardized before analysis. Pairwise comparisons are restricted to indicators within the same measurement framework and across domains.

The script additionally generates contour plots for visualization of the response surfaces and saves the numerical RSA results to `rsa_output/`.

The RSA was conducted in Python 3.10.20 using `pandas`, `NumPy`, `SciPy`, `statsmodels`, and `Matplotlib`.

## Data

Participant-level data are not included in this repository due to data-protection and ethical requirements. The analysis scripts are provided to support transparency and reproducibility of the statistical procedures reported in the manuscript.

## Citation

Citation information will be added following publication of the accompanying manuscript.
