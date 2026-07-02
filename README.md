
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


**Key Contributions**

* Estimates the **causal effect of Medicare eligibility on mental health** using a Fuzzy Regression Discontinuity (RD) design and instrumental variables framework applied to nationally representative longitudinal data from the RAND Health and Retirement Study (HRS).

* Demonstrates that **financial protection is the primary mechanism** through which Medicare improves mental health by jointly examining changes in insurance coverage, out-of-pocket medical spending, insurance premiums, income, labor-market outcomes, and household financial well-being within a unified empirical framework.

* Provides one of the most comprehensive evaluations of Medicare's broader impacts by integrating mental health, healthcare expenditures, insurance transitions, labor-market behavior, and household outcomes rather than studying these domains independently.

* Documents substantial heterogeneity in Medicare's mental health effects, identifying **significantly larger improvements among individuals working 30–40 hours per week**, suggesting that the psychological benefits of Medicare are concentrated among standard full-time workers rather than being uniformly distributed across beneficiaries.

* Shows that **individuals with elevated depressive symptoms prior to age 65 experience the largest mental health gains from Medicare**, indicating that the program delivers its greatest benefits to those with the highest baseline psychological vulnerability.

* Strengthens the credibility of the causal findings through extensive validation and robustness analyses, including placebo cutoff tests, bandwidth sensitivity analyses, donut-hole specifications, density tests, survey-weighted estimation, panel fixed-effects models, and multiple subgroup analyses.

* Examines household and spousal spillover effects to assess whether Medicare's benefits extend beyond the directly eligible individual, providing evidence on the broader household consequences of public health insurance.

* Develops a fully reproducible, publication-ready empirical workflow that automatically generates regression tables, robustness analyses, and publication-quality figures, enhancing transparency and facilitating replication and future research.


## Author

Emmanuel Kuh

M.S. Economic Analytics (Econometrics & Quantitative Economics) 
Towson University

**LinkedIn:** https://www.linkedin.com/in/emmanuel-kuh45

**GitHub:** https://github.com/kuhemmanuel9-sudo
