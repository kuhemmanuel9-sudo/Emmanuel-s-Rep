
# Medicare and Mental Health:Causal Evidence From a Fuzzy Regression Discontinuity Design

## Overview

This project estimates the causal effect of Medicare eligibility at age 65 on mental health, healthcare utilization, financial protection, labor-market outcomes, and household well-being using the RAND Health and Retirement Study (HRS).

A **Fuzzy Regression Discontinuity Design (RDD)** is used to exploit the sharp increase in Medicare eligibility at age 65 as an instrumental variable for Medicare coverage. The analysis follows modern causal inference methods and includes extensive robustness, placebo, heterogeneity, and mechanism analyses.

---

## Research Question

How does Medicare eligibility at age 65 affect mental health, financial protection, healthcare expenditures, labor-market outcomes, and household well-being among older Americans? In addition, does Medicare generate measurable spillover effects on spouses and other household members?

---


## Dataset

- RAND Health and Retirement Study (HRS)
- Waves 9–14 (2008–2018)
- **32,894 person-wave observations**

The RAND HRS contains nationally representative longitudinal information on health, insurance, employment, retirement, healthcare expenditures, income, wealth, and mental health.

> **Note:** The RAND HRS data are proprietary and are not included in this repository.

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

- 
## Household Spillovers

In addition to estimating direct treatment effects, the project examines whether Medicare eligibility affects spouses and households through:
- Household depression outcomes
- Spousal mental health
- Labor-market spillovers
- Insurance composition

Results suggest that Medicare's benefits are concentrated primarily on the Medicare-eligible individual, with limited evidence of statistically significant household spillover effects.


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


## Key Findings

The analysis finds that Medicare eligibility:

- Improves mental health.
- Reduces depressive symptoms.
- Lowers out-of-pocket healthcare spending.
- Reduces private insurance premium burdens.
- Produces heterogeneous effects across demographic and employment groups.
- Shows particularly large mental health improvements among respondents working **30–40 hours per week**.


## Key Contributions

- Estimates the causal effect of Medicare eligibility on mental health using a **Fuzzy Regression Discontinuity Design (RDD)** and Instrumental Variables framework with nationally representative longitudinal data from the RAND Health and Retirement Study.
- Integrates mental health, healthcare expenditures, insurance transitions, labor-market outcomes, financial protection, and household well-being into a unified causal analysis rather than examining these outcomes separately.
- Extends the analysis with comprehensive robustness checks, including placebo cutoffs, bandwidth sensitivity, donut-hole estimators, survey-weighted estimation, panel fixed-effects models, and multiple subgroup analyses.
- Investigates heterogeneous treatment effects across demographic, socioeconomic, health, and employment groups, identifying substantially larger mental health improvements among individuals working **30–40 hours per week**.
- Examines household and spousal spillover effects to evaluate whether Medicare's benefits extend beyond the directly eligible individual.
- Produces a fully reproducible, publication-ready research pipeline that automatically generates regression tables, robustness analyses, and publication-quality figures.


## Author

Emmanuel Kuh

M.S. Econometrics & Quantitative Economics  
Towson University

**LinkedIn:** https://www.linkedin.com/in/emmanuel-kuh45

**GitHub:** https://github.com/kuhemmanuel9-sudo
