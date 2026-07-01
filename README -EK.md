# Medicare Eligibility and Mental Health

This repository contains a publication-ready R replication script for a fuzzy regression discontinuity analysis of Medicare eligibility and mental-health outcomes using RAND HRS Waves 9-14 (2008-2018).

## Analysis Design

- Running variable: age in months relative to age 65.
- Instrument: age-65 eligibility.
- Treatment: observed Medicare coverage.
- Primary estimand: fuzzy-RD local average treatment effect.
- Inference: respondent-clustered standard errors for person-wave data.
- Outputs: CSV tables, PNG figures, and analysis data files.

Mechanism and subgroup analyses that condition on post-threshold variables, including work status, hours, coverage type, or job continuity, should be treated as descriptive robustness checks.

## Files

- `analysis/medicare_mental_health_pub_ready.R`: main analysis script.
- `.gitignore`: excludes raw data, generated analysis data, logs, and local R artifacts.

## Data Setup

The RAND HRS Stata file is not included and should not be committed to GitHub.

Expected portable repository location:

```r
data/raw/randhrs1992_2022v1.dta
```

On JB's local machine, the script also falls back to:

```r
C:/Users/JB/Downloads/randhrs1992_2022v1_STATA/randhrs1992_2022v1.dta
```

Alternatively, set the data path before running:

```r
Sys.setenv(RAND_HRS_DTA = "path/to/randhrs1992_2022v1.dta")
```

## R Packages

The script checks for required packages before running:

```r
install.packages(c(
  "tidyverse",
  "haven",
  "rdrobust",
  "rddensity",
  "fixest",
  "scales",
  "ggtext",
  "glue"
))
```

## Running

From the repository root:

```r
source("analysis/medicare_mental_health_pub_ready.R")
```

Optional custom output directory:

```r
Sys.setenv(MEDICARE_MH_OUTPUT_DIR = "outputs/medicare_mental_health_results")
source("analysis/medicare_mental_health_pub_ready.R")
```

## Output Structure

The script writes results to:

```text
outputs/medicare_mental_health_results/
  tables/
  figures/
  analysis_data/
```

Generated respondent-level files in `analysis_data/` may contain sensitive or restricted data derivatives and should remain out of public version control.

## Significance Stars

Tables with p-values include both numeric p-values and star-formatted columns:

- `p_value`: numeric p-value.
- `p_value_stars`: p-value with significance stars.
- `estimate_stars`: estimate with significance stars, when an estimate is available.

Star convention: `*** p < 0.01`, `** p < 0.05`, `* p < 0.10`.
