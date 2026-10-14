# Short-Run Brent–NOK Relationship across Global Financial-Risk Regimes

**An empirical threshold-regression analysis using the Norwegian effective exchange rate and the VIX**

## 1. Overview

This project investigates whether the short-run relationship between Brent crude-oil returns and the Norwegian krone (NOK) varies across global financial-risk regimes.

Norway is a major oil-exporting economy, making movements in international crude-oil prices an important potential driver of its currency. However, the relationship between oil prices and exchange rates may depend on broader financial-market conditions. In periods of heightened uncertainty, changes in global risk sentiment may alter how oil-price movements are reflected in the Norwegian krone.

This research examines whether the short-run oil–exchange-rate relationship differs between low- and high-financial-risk regimes, using the CBOE Volatility Index (VIX) as a measure of expected U.S. equity-market volatility and the Norwegian I-44 nominal effective exchange-rate index to measure NOK movements.

## 2. Research Questions

The analysis addresses three main questions:

1. **Oil–NOK relationship:** How are changes in Brent crude-oil prices associated with short-run movements in the Norwegian effective exchange rate?
2. **Financial-risk regimes:** Does this relationship differ between periods of relatively low and high global financial risk, as classified using the VIX?
3. **U.S.-dollar control:** Does the regime-dependent relationship remain after controlling for broad movements in the U.S. dollar?

## 3. Data and Variables

The empirical framework uses the following financial time series:

| Variable | Description | Role in the analysis |
|---|---|---|
| Brent crude-oil price | International benchmark crude-oil price | Main explanatory variable |
| Norwegian I-44 index | Nominal effective exchange-rate index for the Norwegian krone | Dependent variable |
| CBOE VIX | Index derived from S&P 500 index-option prices that reflects expected near-term U.S. equity-market volatility | Financial-risk regime indicator |
| U.S.-dollar measure | Broad measure of U.S.-dollar movements | Control variable |

The sample period, observation frequency, data sources, and precise variable transformations will be documented alongside the empirical implementation.

## 4. Empirical Methodology

The analysis follows four main steps.

### 4.1 Data Preparation

The financial time series are imported, checked, and aligned by observation date. Missing observations and differences in data availability are examined before constructing the variables used in the regressions.

### 4.2 Return Construction and Exploratory Analysis

Changes in Brent crude-oil prices and the Norwegian effective exchange-rate index are transformed into suitable return or change measures. Exploratory analysis is used to examine their distributions, time-series properties, and preliminary relationships.

### 4.3 VIX-Based Threshold Regression

A threshold framework is used to investigate whether the short-run oil–exchange-rate relationship differs across financial-risk regimes.

The VIX serves as the threshold variable, separating observations into relatively low- and high-risk regimes according to a threshold value, denoted by \(c\). The threshold-selection procedure and corresponding statistical inference will be documented in the empirical analysis.

### 4.4 U.S.-Dollar Controls and Robustness Analysis

The regression framework incorporates a measure of broad U.S.-dollar movements to assess whether the estimated oil–NOK relationship reflects an association beyond general dollar fluctuations. Additional robustness checks will be considered where appropriate.

## 5. Tools and Technologies

- **R** — statistical computing and econometric analysis
- **RStudio** — development environment
- **tidyverse** — data manipulation and preparation, where applicable
- **ggplot2** — data visualization, where applicable
- **Econometric packages** — regression estimation and statistical inference, depending on the final implementation

## 6. Repository Structure

The repository will contain the R scripts used to prepare the data, estimate the empirical models, and produce the visualizations.

```text
brent-nok-threshold-analysis/
├── README.md
├── R/
│   ├── 01_data_preparation.R
│   ├── 02_threshold_regression.R
│   └── 03_robustness_checks.R
└── figures/
```

The structure may evolve as the project develops. Only scripts and figures that are actually included in the repository should be listed as available files.

## 7. Reproducibility and Data Availability

The code is intended to provide a transparent and reproducible workflow for the empirical analysis.

Data sources, variable definitions, package dependencies, and instructions for running the scripts will be documented as the implementation progresses. Third-party financial data will be included only where redistribution is permitted by the relevant data-provider terms.

## 8. Project Status

**Status: Ongoing research project.**

This project is being developed as part of an M.Sc. in Quantitative Finance at Kiel University. The empirical analysis and robustness checks are being developed progressively.

Results and conclusions will be added after the relevant models have been estimated and evaluated.

## 9. Research Scope

The project focuses on the short-run association between Brent crude-oil returns and movements in the Norwegian effective exchange rate under different financial-risk conditions. The empirical results will be interpreted in light of the model specification, data limitations, and the distinction between statistical association and causal effects.
