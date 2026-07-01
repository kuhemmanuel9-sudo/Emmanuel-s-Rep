
# Medicare Eligibility and Mental Health: A Fuzzy Regression Discontinuity Analysis

## Overview

This project estimates the causal effect of Medicare eligibility at age 65 on mental health, healthcare utilization, financial protection, labor-market outcomes, and household well-being using the RAND Health and Retirement Study (HRS).

A **Fuzzy Regression Discontinuity Design (RDD)** is used to exploit the sharp increase in Medicare eligibility at age 65 as an instrumental variable for Medicare coverage. The analysis follows modern causal inference methods and includes extensive robustness, placebo, heterogeneity, and mechanism analyses.

---

## Research Question

**What is the causal impact of Medicare eligibility and Medicare coverage on mental health and financial outcomes among older Americans?**

---

## Dataset

**Source**

- RAND Health and Retirement Study (HRS)
- Waves 9–14 (2008–2018)

The RAND HRS is a nationally representative longitudinal survey of Americans over age 50 containing detailed information on:

- Health
- Insurance
- Employment
- Income
- Wealth
- Mental health
- Healthcare expenditures

**Note:** RAND HRS data are subject to licensing restrictions and are **not included** in this repository.

---

## Methodology

### Identification Strategy

- Fuzzy Regression Discontinuity Design (RDD)
- Running Variable:
  - Age in months relative to the 65th birthday
- Treatment:
  - Medicare Coverage
- Instrument:
  - Medicare Eligibility at Age 65
- Estimand:
  - Local Average Treatment Effect (LATE)

---

## Statistical Methods

- Fuzzy Regression Discontinuity
- Local Linear Regression
- Instrumental Variables
- Cluster-Robust Standard Errors
- Placebo Cutoff Tests
- Density Manipulation Tests
- Bandwidth Sensitivity Analysis
- Donut-Hole Robustness Checks
- Fixed Effects Models
- Heterogeneity Analysis

---

## Outcomes Examined

### Mental Health

- CES-D Depression Score
- Depression Indicator
- Mental Health Index
- Life Satisfaction
- Loneliness
- Positive Affect
- Negative Affect

### Financial Protection

- Out-of-Pocket Medical Spending
- Medicare Premiums
- Prescription Drug Premiums
- Insurance Coverage
- Household Income
- Wealth

### Labor Market

- Employment
- Retirement
- Weekly Hours Worked
- Job Stability

### Health

- Functional Limitations
- Chronic Conditions
- Prescription Drug Use

---

## Robustness Checks

The analysis includes numerous robustness exercises including:

- Alternative bandwidths
- Optimal bandwidth selection
- Donut-hole estimators
- Placebo age cutoffs
- Running-variable density tests
- Survey-weighted estimates
- Medicaid exclusion
- Income-based subsamples
- Individual fixed effects
- Forward outcome analysis

---

## Heterogeneity Analysis

Effects are estimated separately by:

- Sex
- Race
- Baseline depression
- Physical health
- Functional limitations
- Household income
- Employment status
- Pre-65 mental health
- Couple households

---

## Outputs

The project automatically generates:

- Publication-quality figures
- Regression tables
- Robustness tables
- Mechanism analyses
- Heterogeneity analyses
- Summary statistics

---

## Software

- R
- tidyverse
- haven
- rdrobust
- rddensity
- fixest
- ggplot2
- ggtext
- glue

---

## Repository Structure

```
Medicare-Mental-Health-RD/
│
├── analysis/
│   └── medicare_mental_health_pub_ready.R
│
├── data/
│   └── raw/
│
├── outputs/
│   ├── figures/
│   ├── tables/
│   └── analysis_data/
│
├── README.md
└── requirements.txt
```

---

## Reproducibility

To run the project:

1. Obtain access to the RAND HRS dataset.
2. Place the dataset in:

```
data/raw/
```

or specify the file path using:

```r
Sys.setenv(RAND_HRS_DTA="path/to/randhrs1992_2022v1.dta")
```

Run:

```r
source("analysis/medicare_mental_health_pub_ready.R")
```


## Key Contributions

- Estimates the causal effect of Medicare eligibility using a Fuzzy RDD framework.
- Examines mental health, healthcare spending, labor-market outcomes, and financial protection.
- Includes extensive robustness and heterogeneity analyses.
- Produces publication-ready tables and figures suitable for academic research.



## Author

Emmanuel Kuh

M.S. Econometrics & Quantitative Economics  
Towson University

**LinkedIn:** https://www.linkedin.com/in/emmanuel-kuh45

**GitHub:** https://github.com/kuhemmanuel9-sudo