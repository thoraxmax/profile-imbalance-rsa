# Response Surface Analysis (RSA)

This repository contains the Python code used for the response surface analyses reported in the manuscript. The analysis was conducted in **Python 3.10.20**.

## Files

- `rsa_analysis.py` - complete RSA analysis and contour-plot code.
- `data.csv` - analysis dataset **not included here**. Place the data file in the same directory as the script if it can be shared under the study's data-governance and ethics requirements.
- `rsa_output/` - created automatically when the script is run.

## Analysis

For each eligible pair of indicators, the script fits the polynomial regression model

`RCADS = b0 + b1*X + b2*Y + b3*X^2 + b4*X*Y + b5*Y^2 + age + male + error`.

The standard RSA surface parameters are then calculated as:

- `a1 = b1 + b2`: linear slope along the line of congruence.
- `a2 = b3 + b4 + b5`: curvature along the line of congruence.
- `a3 = b1 - b2`: linear slope along the line of incongruence.
- `a4 = b3 - b4 + b5`: curvature along the line of incongruence.

The polynomial RSA model is compared with the corresponding linear model using a nested-model F test. Age and sex (`male`) are included as covariates. Continuous indicators, RCADS, and age are standardized before analysis. Neuroticism (`p_neuro`) is reverse-scored before standardization, matching the analysis notebook.

The pairwise analysis is restricted to comparisons within the same measurement framework and across domains, as in the original analysis. The script also generates the mean cross-domain contour visualization for BESSI and E-SWAN indicators.

## Important note about bootstrap analyses

The RSA code in the original analysis notebook did **not** use bootstrap confidence intervals for `a1`-`a4`. Inference for the RSA parameters was based on the OLS coefficient covariance matrix and t distribution, and the polynomial-versus-linear comparison used the nested-model F test. The 2,000-iteration bootstrap described elsewhere in the manuscript was used for other OLS/model-comparison analyses, not for this RSA routine. This repository therefore does not add a bootstrap procedure that was not part of the original RSA analysis.

## Requirements

The analysis was run with Python 3.10.20. The principal package versions used in the project were:

- pandas 2.3.3
- NumPy 2.2.6
- SciPy 1.15.3
- statsmodels 0.14.6
- Matplotlib 3.10.9

Install the required packages with, for example:

```bash
python -m pip install pandas==2.3.3 numpy==2.2.6 scipy==1.15.3 statsmodels==0.14.6 matplotlib==3.10.9
```

## Running the analysis

Place `rsa_analysis.py` and `merged_data9.csv` in the same directory and run:

```bash
python rsa_analysis.py
```

The script creates `rsa_output/` containing:

- `rsa_pairwise_results.csv` - pairwise RSA parameters, standard errors, p values, model R-squared values, and polynomial-versus-linear F tests.
- `rsa_contours.png` - high-resolution contour figure.
- `rsa_contours.svg` - vector version of the contour figure.

## Reproducibility and data sharing

Do not upload participant-level data to a public GitHub repository unless public sharing is permitted by the study consent, ethics approval, and applicable data-protection requirements. If the data cannot be shared, the code can still be made public and the README can state how qualified researchers may request access or where controlled-access data are available.

## Citation

If this repository accompanies a manuscript, replace this section with the final article citation and, if archived through Zenodo or a similar service, the repository DOI.
