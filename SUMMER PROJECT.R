# ==============================================================================
# Medicare Eligibility and Mental Health
# Fuzzy Regression Discontinuity Analysis
# RAND HRS, Waves 9-14 (2008-2018)
#
# Publication-ready, GitHub-ready replication script
# ==============================================================================
#
# Purpose:
#   Estimate the local effect of Medicare take-up at age 65 on mental-health,
#   financial-protection, labor-market, household, and robustness outcomes.
#
# Identification:
#   - Running variable: age in months relative to the 65th birthday.
#   - Instrument: age-65 eligibility.
#   - Treatment: observed Medicare coverage.
#   - Main estimand: fuzzy-RD local average treatment effect for compliers.
#   - Inference: standard errors clustered by respondent ID.
#
# Interpretation rule:
#   Mechanism and subgroup analyses that condition on post-threshold variables
#   such as work status, job continuity, or observed coverage type are descriptive
#   robustness checks. They should not be interpreted as separate causal designs.
#
# Reproducible use:
#   1. Put the RAND HRS Stata file at:
#      data/raw/randhrs1992_2022v1.dta
#   2. Or set the file path before running:
#      Sys.setenv(RAND_HRS_DTA = "path/to/randhrs1992_2022v1.dta")
#   3. Optionally set a custom output directory:
#      Sys.setenv(MEDICARE_MH_OUTPUT_DIR = "outputs/medicare_mental_health_results")
#   4. Run from the repository root:
#      source("analysis/medicare_mental_health_pub_ready.R")
#
# Data note:
#   RAND HRS files may be restricted or proprietary. Do not commit raw data or
#   derived respondent-level analysis files to a public repository.
# ==============================================================================

options(
  stringsAsFactors = FALSE,
  dplyr.summarise.inform = FALSE,
  scipen = 999
)

REQUIRED_PACKAGES <- c(
  "tidyverse",
  "haven",
  "rdrobust",
  "rddensity",
  "fixest",
  "scales",
  "ggtext",
  "glue"
)

missing_packages <- REQUIRED_PACKAGES[
  !vapply(REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  install_hint <- paste0(
    "install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
  stop(
    paste(
      "Missing required R packages:",
      paste(missing_packages, collapse = ", "),
      "\nInstall them with:",
      install_hint
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  invisible(lapply(REQUIRED_PACKAGES, library, character.only = TRUE))
})

set.seed(12345)
fixest::setFixest_notes(FALSE)

# ------------------------------------------------------------------------------
# 0. Configuration
# ------------------------------------------------------------------------------

PROJECT_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

DATA_FILE <- Sys.getenv(
  "RAND_HRS_DTA",
  unset = file.path(PROJECT_ROOT, "data", "raw", "randhrs1992_2022v1.dta")
)

OUTPUT_DIR <- Sys.getenv(
  "MEDICARE_MH_OUTPUT_DIR",
  unset = file.path(PROJECT_ROOT, "outputs", "medicare_mental_health_results")
)
OUTPUT_DIR <- normalizePath(OUTPUT_DIR, winslash = "/", mustWork = FALSE)

TABLE_DIR  <- file.path(OUTPUT_DIR, "tables")
FIGURE_DIR <- file.path(OUTPUT_DIR, "figures")
DATA_DIR   <- file.path(OUTPUT_DIR, "analysis_data")

walk(
  c(OUTPUT_DIR, TABLE_DIR, FIGURE_DIR, DATA_DIR),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

if (!file.exists(DATA_FILE)) {
  stop(
    glue(
      "RAND HRS data file not found.\n",
      "Expected: {DATA_FILE}\n",
      "Set Sys.setenv(RAND_HRS_DTA = '...') or place the file under data/raw/."
    ),
    call. = FALSE
  )
}

WAVES <- 9:14
WAVE_YEARS <- tibble(
  wave = WAVES,
  year = c(2008L, 2010L, 2012L, 2014L, 2016L, 2018L)
)

CUTOFF_AGE    <- 65L
CUTOFF_MONTHS <- CUTOFF_AGE * 12L

# Load a wider age window so placebo cutoffs have full +/-60-month windows.
# Main person-wave and couple analyses use ages 60-70.
LOAD_MIN_MONTHS <- 56L * 12L
LOAD_MAX_MONTHS <- 74L * 12L
MAIN_MIN_MONTHS <- 60L * 12L
MAIN_MAX_MONTHS <- 70L * 12L

MAIN_BW <- 60L
CONT_BW <- 36L

# CPI-U annual averages. Dollar outcomes are converted to 2016 dollars.
CPI_REF <- 240.007
CPI_TABLE <- tibble(
  wave = WAVES,
  cpi = c(215.303, 218.056, 229.594, 236.736, 240.007, 251.107)
)

CLR_MAIN    <- "#1F4E79"
CLR_ACCENT  <- "#B23A48"
CLR_UNDER65 <- "#2878B5"
CLR_OVER65  <- "#B23A48"
CLR_GREY    <- "#8A8A8A"

# ------------------------------------------------------------------------------
# 1. Utility Functions
# ------------------------------------------------------------------------------

section <- function(title) {
  cat("\n", strrep("-", 80), "\n", sep = "")
  cat(title, "\n")
  cat(strrep("-", 80), "\n", sep = "")
}

theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_markdown(face = "bold", size = base_size + 3),
      plot.subtitle = element_markdown(size = base_size - 0.5, color = "#4F4F4F"),
      plot.caption = element_text(size = base_size - 2.5, color = "#777777", hjust = 0),
      axis.title = element_text(face = "bold", size = base_size - 0.5),
      axis.text = element_text(size = base_size - 1.5, color = "#333333"),
      panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "#F3F6F8", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(14, 16, 10, 14)
    )
}

save_figure <- function(plot, file_name, width = 9, height = 5.5) {
  ggsave(
    filename = file.path(FIGURE_DIR, file_name),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
  print(plot)
  invisible(plot)
}

write_table <- function(x, file_name) {
  write_csv(x, file.path(TABLE_DIR, file_name))
  print(x, n = Inf)
  invisible(x)
}

wave_var <- function(wave, stem) paste0("r", wave, stem)
hh_var   <- function(wave, stem) paste0("h", wave, stem)

pull_var <- function(data, candidates, default = NA_real_) {
  candidates <- as.character(candidates)
  hit <- candidates[candidates %in% names(data)]
  if (length(hit) == 0L) return(rep(default, nrow(data)))
  data[[hit[1L]]]
}

clean_negative_missing <- function(data, protect = "hhidpn") {
  to_clean <- intersect(setdiff(names(data), protect), names(data))
  data %>%
    mutate(across(all_of(to_clean), ~ {
      if (is.numeric(.x)) {
        replace(as.numeric(.x), .x < 0, NA_real_)
      } else {
        .x
      }
    }))
}

clean_condition <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 0 ~ 0,
    x %in% c(3, 4) ~ 0,
    TRUE ~ NA_real_
  )
}

z_std <- function(x) {
  out <- rep(NA_real_, length(x))
  observed <- !is.na(x)
  if (sum(observed) < 2L) return(out)
  
  sigma <- sd(x[observed])
  if (is.na(sigma) || sigma == 0) return(out)
  
  out[observed] <- (x[observed] - mean(x[observed])) / sigma
  out
}

yes_no_15 <- function(x) {
  case_when(
    x == 1 ~ 1,
    x %in% c(0, 5) ~ 0,
    TRUE ~ NA_real_
  )
}

star_p <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

empty_rd_table <- function(sample_label = NA_character_) {
  tibble(
    sample = character(),
    estimand = character(),
    outcome = character(),
    estimate = numeric(),
    se = numeric(),
    p_value = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    n = integer(),
    stars = character()
  )
}

empty_estimate_row <- function(estimand, label, n = 0L) {
  tibble(
    estimand = estimand,
    outcome = label,
    estimate = NA_real_,
    se = NA_real_,
    p_value = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    n = as.integer(n),
    stars = ""
  )
}

rd_extract <- function(rd_obj, label, estimand, n) {
  tibble(
    estimand = estimand,
    outcome = label,
    estimate = as.numeric(rd_obj$Estimate[1]),
    se = as.numeric(rd_obj$se[1]),
    p_value = as.numeric(rd_obj$pv[1]),
    ci_lower = as.numeric(rd_obj$ci[1, 1]),
    ci_upper = as.numeric(rd_obj$ci[1, 2]),
    n = as.integer(n),
    stars = star_p(p_value)
  )
}

rd_reduced_form <- function(data, y, label, x = "running", h = MAIN_BW,
                            cluster = "hhidpn", estimand = "Reduced form",
                            min_n = 300) {
  required <- c(y, x, cluster)
  if (!all(required %in% names(data))) {
    missing <- setdiff(required, names(data))
    warning(
      glue("Skipping {label}: missing variable(s) {paste(missing, collapse = ', ')}."),
      call. = FALSE
    )
    return(empty_estimate_row(estimand, label, 0L))
  }
  
  d <- data %>% drop_na(all_of(required))
  if (nrow(d) < min_n || n_distinct(d[[x]] >= 0) < 2L) {
    return(empty_estimate_row(estimand, label, nrow(d)))
  }
  
  rd <- tryCatch(
    rdrobust(
      y = d[[y]],
      x = d[[x]],
      c = 0,
      h = h,
      kernel = "triangular",
      cluster = d[[cluster]]
    ),
    error = function(e) {
      warning(
        glue("rdrobust failed for {label} ({estimand}): {conditionMessage(e)}"),
        call. = FALSE
      )
      NULL
    }
  )
  
  if (is.null(rd)) return(empty_estimate_row(estimand, label, nrow(d)))
  rd_extract(rd, label, estimand, nrow(d))
}

rd_fuzzy <- function(data, y, label, treatment = "medicare", x = "running",
                     h = MAIN_BW, cluster = "hhidpn",
                     estimand = "Fuzzy RD LATE", min_n = 300) {
  required <- c(y, treatment, x, cluster)
  if (!all(required %in% names(data))) {
    missing <- setdiff(required, names(data))
    warning(
      glue("Skipping {label}: missing variable(s) {paste(missing, collapse = ', ')}."),
      call. = FALSE
    )
    return(empty_estimate_row(estimand, label, 0L))
  }
  
  d <- data %>% drop_na(all_of(required))
  if (
    nrow(d) < min_n ||
    n_distinct(d[[x]] >= 0) < 2L ||
    n_distinct(d[[treatment]]) < 2L
  ) {
    return(empty_estimate_row(estimand, label, nrow(d)))
  }
  
  rd <- tryCatch(
    rdrobust(
      y = d[[y]],
      x = d[[x]],
      fuzzy = d[[treatment]],
      c = 0,
      h = h,
      kernel = "triangular",
      cluster = d[[cluster]]
    ),
    error = function(e) {
      warning(
        glue("rdrobust failed for {label} ({estimand}): {conditionMessage(e)}"),
        call. = FALSE
      )
      NULL
    }
  )
  
  if (is.null(rd)) return(empty_estimate_row(estimand, label, nrow(d)))
  rd_extract(rd, label, estimand, nrow(d))
}

make_rd_table <- function(data, specs, sample_label, include_fuzzy = TRUE) {
  specs_use <- specs %>%
    filter(var %in% names(data)) %>%
    mutate(nonmissing = map_int(var, ~ sum(!is.na(data[[.x]])))) %>%
    filter(nonmissing >= 100)
  
  if (nrow(specs_use) == 0) return(empty_rd_table(sample_label))
  
  reduced <- map2_dfr(
    specs_use$var,
    specs_use$label,
    ~ rd_reduced_form(data, .x, .y)
  )
  
  if (!include_fuzzy) {
    return(reduced %>% mutate(sample = sample_label, .before = estimand))
  }
  
  fuzzy <- map2_dfr(
    specs_use$var,
    specs_use$label,
    ~ rd_fuzzy(data, .x, .y)
  )
  
  bind_rows(reduced, fuzzy) %>%
    mutate(sample = sample_label, .before = estimand)
}

local_linear_jump <- function(data, y, label = y, x = "running",
                              h = CONT_BW, cluster = "hhidpn") {
  d <- data %>%
    filter(!is.na(.data[[x]]), abs(.data[[x]]) <= h) %>%
    drop_na(all_of(c(y, x, cluster))) %>%
    mutate(
      above = as.integer(.data[[x]] >= 0),
      w_tri = pmax(0, 1 - abs(.data[[x]]) / h)
    )
  
  if (nrow(d) < 100 || n_distinct(d$above) < 2) {
    return(tibble(variable = y, label = label, estimate = NA_real_, se = NA_real_,
                  p_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
                  n = nrow(d), stars = ""))
  }
  
  fml <- as.formula(paste0(y, " ~ above + ", x, " + above:", x))
  m <- feols(fml, data = d, weights = ~ w_tri,
             cluster = as.formula(paste0("~", cluster)))
  
  est <- coef(m)["above"]
  se1 <- se(m)["above"]
  p1  <- fixest::pvalue(m)["above"]
  
  tibble(
    variable = y,
    label = label,
    estimate = est,
    se = se1,
    p_value = p1,
    ci_lower = est - 1.96 * se1,
    ci_upper = est + 1.96 * se1,
    n = nobs(m),
    stars = star_p(p1)
  )
}

summary_long <- function(data, vars, labels) {
  map2_dfr(vars, labels, function(v, lab) {
    x <- data[[v]]
    tibble(
      variable = lab,
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      n = sum(!is.na(x))
    )
  })
}

summary_by_group <- function(data, vars, labels, group_var) {
  full <- summary_long(data, vars, labels) %>%
    mutate(group = "Full sample", .before = variable)
  
  grouped <- data %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]]) %>%
    group_modify(~ summary_long(.x, vars, labels)) %>%
    ungroup() %>%
    rename(group = all_of(group_var)) %>%
    mutate(group = as.character(group))
  
  bind_rows(full, grouped)
}

rd_bins <- function(data, y, x = "running", bin_width = 3, h = MAIN_BW, min_n = 10) {
  data %>%
    drop_na(all_of(c(y, x))) %>%
    mutate(
      bin = floor(.data[[x]] / bin_width) * bin_width + bin_width / 2,
      side = if_else(.data[[x]] >= 0, "right", "left")
    ) %>%
    group_by(bin, side) %>%
    summarise(
      y_mean = mean(.data[[y]], na.rm = TRUE),
      y_se = sd(.data[[y]], na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(n >= min_n, abs(bin) <= h)
}

rd_xscale <- scale_x_continuous(
  breaks = seq(-60, 60, by = 12),
  labels = function(x) if_else(x == 0, "0\n(Age 65)", as.character(x))
)

rd_vline <- geom_vline(
  xintercept = 0,
  linetype = "dashed",
  color = CLR_ACCENT,
  linewidth = 0.75
)

forest_plot <- function(data, title, subtitle, x_label, file_name,
                        label_col = "outcome", width = 9, height = 5.5) {
  p <- data %>%
    filter(!is.na(estimate)) %>%
    mutate(
      sig = p_value < 0.10,
      label = factor(.data[[label_col]], levels = rev(unique(.data[[label_col]])))
    ) %>%
    ggplot(aes(x = estimate, y = label, color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_linerange(aes(xmin = ci_lower, xmax = ci_upper), linewidth = 1.1, alpha = 0.75) +
    geom_point(size = 4) +
    scale_color_manual(
      values = c("FALSE" = CLR_MAIN, "TRUE" = CLR_ACCENT),
      labels = c("FALSE" = "p >= 0.10", "TRUE" = "p < 0.10"),
      name = "Significance"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = NULL,
      caption = "Notes: Points are estimates; bars show 95% confidence intervals."
    ) +
    theme_pub()
  
  save_figure(p, file_name, width, height)
}

# ------------------------------------------------------------------------------
# 2. Load RAND HRS Data
# ------------------------------------------------------------------------------

respondent_stems <- c(
  "agey_m", "agem_m", "cesd", "depyr",
  "govmr", "higov", "govmd", "govva", "covr", "covs",
  "henum", "oopmd", "prprm1", "prprm2", "prprm3", "mrprem", "ltcprm",
  "work", "jhours", "lbrf", "sayret", "rret", "isret", "ipen", "iearn",
  "samejob", "jnjob", "jjobs",
  "drugs", "rxprem", "ssdi", "issdi", "issi",
  "adl5a", "iadl5a", "mobila", "grossa", "lgmusa", "finea", "hlphrst",
  "hibp", "diab", "cancr", "lung", "heart", "stroke", "arthr",
  "lbsatwlf", "lblonely3", "lbposaffect", "lbnegaffect",
  "lbelig", "lbcomp", "lbwgtr",
  "wtresp", "wthh", "wtr_nh", "wtcrnh"
)

household_stems <- c("itot", "atotb")

section("Loading Wave-3 Baseline Conditions")

baseline_raw <- read_dta(
  DATA_FILE,
  col_select = any_of(c(
    "hhidpn", "r3depyr", "r3cesd",
    "r3adl5a", "r3iadl5a", "r3mobila", "r3grossa", "r3lgmusa"
  ))
) %>%
  clean_negative_missing(protect = "hhidpn")

baseline <- tibble(
  hhidpn = baseline_raw$hhidpn,
  base_depyr = pull_var(baseline_raw, "r3depyr"),
  base_cesd = pull_var(baseline_raw, "r3cesd"),
  base_adl = pull_var(baseline_raw, "r3adl5a"),
  base_iadl = pull_var(baseline_raw, "r3iadl5a"),
  base_mobility = pull_var(baseline_raw, "r3mobila"),
  base_gross_motor = pull_var(baseline_raw, "r3grossa"),
  base_large_muscle = pull_var(baseline_raw, "r3lgmusa")
)

cat("Baseline records loaded:", nrow(baseline), "\n")

section("Loading Waves 9-14")

all_waves <- vector("list", length(WAVES))
all_couples <- vector("list", length(WAVES))

for (idx in seq_along(WAVES)) {
  w <- WAVES[idx]
  spouse_var <- paste0("s", w, "hhidpn")
  age_month_var <- wave_var(w, "agem_m")
  age_year_var <- wave_var(w, "agey_m")
  
  requested_vars <- c(
    "hhidpn", spouse_var, "ragender", "raracem", "raedyrs",
    paste0("r", w, respondent_stems),
    paste0("h", w, household_stems)
  )
  
  raw <- read_dta(DATA_FILE, col_select = any_of(requested_vars)) %>%
    clean_negative_missing(protect = "hhidpn")
  
  if (!(age_month_var %in% names(raw))) {
    stop(glue("Missing age-in-months variable for wave {w}: {age_month_var}"))
  }
  
  wave_df <- tibble(
    hhidpn = raw$hhidpn,
    spouse_id = pull_var(raw, spouse_var),
    wave = w,
    year = WAVE_YEARS$year[idx],
    age_months = raw[[age_month_var]],
    age_years = pull_var(raw, age_year_var),
    running = raw[[age_month_var]] - CUTOFF_MONTHS,
    eligible65 = as.integer(raw[[age_month_var]] >= CUTOFF_MONTHS),
    female = case_when(raw$ragender == 2 ~ 1, raw$ragender == 1 ~ 0, TRUE ~ NA_real_),
    white = case_when(raw$raracem == 1 ~ 1, is.na(raw$raracem) ~ NA_real_, TRUE ~ 0),
    educ = raw$raedyrs,
    cesd = pull_var(raw, wave_var(w, "cesd")),
    depyr = pull_var(raw, wave_var(w, "depyr")),
    medicare = pull_var(raw, wave_var(w, "govmr")),
    any_gov_ins = pull_var(raw, wave_var(w, "higov")),
    medicaid = pull_var(raw, wave_var(w, "govmd")),
    va_ins = pull_var(raw, wave_var(w, "govva")),
    priv_ins_r = pull_var(raw, wave_var(w, "covr")),
    priv_ins_s = pull_var(raw, wave_var(w, "covs")),
    oop = pull_var(raw, wave_var(w, "oopmd")),
    plan_count = pull_var(raw, wave_var(w, "henum")),
    private_premium_1 = pull_var(raw, wave_var(w, "prprm1")),
    private_premium_2 = pull_var(raw, wave_var(w, "prprm2")),
    private_premium_3 = pull_var(raw, wave_var(w, "prprm3")),
    hmo_premium = pull_var(raw, wave_var(w, "mrprem")),
    ltc_premium = pull_var(raw, wave_var(w, "ltcprm")),
    rx_premium = pull_var(raw, wave_var(w, "rxprem")),
    working = pull_var(raw, wave_var(w, "work")),
    work_hours = pull_var(raw, wave_var(w, "jhours")),
    labor_force_status = pull_var(raw, wave_var(w, "lbrf")),
    retired_raw = pull_var(raw, c(wave_var(w, "sayret"), wave_var(w, "rret"))),
    same_job_title = pull_var(raw, wave_var(w, "samejob")),
    num_jobs = pull_var(raw, wave_var(w, "jnjob")),
    job_hist_stat = pull_var(raw, wave_var(w, "jjobs")),
    rx_drugs = pull_var(raw, wave_var(w, "drugs")),
    ssdi_receive = pull_var(raw, wave_var(w, "ssdi")),
    ssi_ssdi_income = pull_var(raw, wave_var(w, "issdi")),
    ssi_income = pull_var(raw, wave_var(w, "issi")),
    social_security = pull_var(raw, wave_var(w, "isret")),
    pension_income = pull_var(raw, wave_var(w, "ipen")),
    earnings = pull_var(raw, wave_var(w, "iearn")),
    total_income = pull_var(raw, hh_var(w, "itot")),
    wealth = pull_var(raw, hh_var(w, "atotb")),
    adl = pull_var(raw, wave_var(w, "adl5a")),
    iadl = pull_var(raw, wave_var(w, "iadl5a")),
    mobility = pull_var(raw, wave_var(w, "mobila")),
    gross_motor = pull_var(raw, wave_var(w, "grossa")),
    large_muscle = pull_var(raw, wave_var(w, "lgmusa")),
    fine_motor = pull_var(raw, wave_var(w, "finea")),
    help_received_hours = pull_var(raw, wave_var(w, "hlphrst")),
    hibp = pull_var(raw, wave_var(w, "hibp")),
    diab = pull_var(raw, wave_var(w, "diab")),
    cancr = pull_var(raw, wave_var(w, "cancr")),
    lung = pull_var(raw, wave_var(w, "lung")),
    heart = pull_var(raw, wave_var(w, "heart")),
    stroke = pull_var(raw, wave_var(w, "stroke")),
    arthr = pull_var(raw, wave_var(w, "arthr")),
    lb_eligible = pull_var(raw, wave_var(w, "lbelig")),
    lb_complete = pull_var(raw, wave_var(w, "lbcomp")),
    lb_weight = pull_var(raw, wave_var(w, "lbwgtr")),
    respondent_weight = pull_var(raw, wave_var(w, "wtresp")),
    household_weight = pull_var(raw, wave_var(w, "wthh")),
    respondent_nh_weight = pull_var(raw, wave_var(w, "wtr_nh")),
    crosssection_nh_weight = pull_var(raw, wave_var(w, "wtcrnh")),
    life_sat = pull_var(raw, wave_var(w, "lbsatwlf")),
    lonely = pull_var(raw, wave_var(w, "lblonely3")),
    pos_affect = pull_var(raw, wave_var(w, "lbposaffect")),
    neg_affect = pull_var(raw, wave_var(w, "lbnegaffect"))
  ) %>%
    mutate(
      age_years = if_else(is.na(age_years), age_months / 12, age_years),
      retired = yes_no_15(retired_raw),
      fulltime_worker = case_when(
        working == 1 & !is.na(work_hours) & between(work_hours, 35, 45) ~ 1,
        working == 1 & !is.na(work_hours) ~ 0,
        TRUE ~ NA_real_
      ),
      across(
        c(hibp, diab, cancr, lung, heart, stroke, arthr),
        clean_condition
      ),
      stable_job_title = yes_no_15(same_job_title),
      job_switcher = case_when(
        stable_job_title == 1 ~ 0,
        stable_job_title == 0 ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(between(age_months, LOAD_MIN_MONTHS, LOAD_MAX_MONTHS))
  
  all_waves[[idx]] <- wave_df
  
  spouse_sample <- wave_df %>% filter(!is.na(spouse_id), spouse_id > 0)
  if (nrow(spouse_sample) > 0) {
    index_df <- spouse_sample %>%
      transmute(
        i_hhidpn = hhidpn,
        spouse_id,
        wave,
        year,
        i_age_months = age_months,
        i_age_years = age_years,
        i_running = running,
        i_eligible65 = eligible65,
        i_cesd = cesd,
        i_depyr = depyr,
        i_medicare = medicare,
        i_working = working,
        i_work_hours = work_hours,
        i_fulltime_worker = fulltime_worker,
        i_retired = retired,
        i_stable_job_title = stable_job_title,
        i_job_switcher = job_switcher,
        i_oop = oop,
        i_help_received_hours = help_received_hours
      )
    
    spouse_df <- wave_df %>%
      transmute(
        s_hhidpn = hhidpn,
        wave,
        s_age_months = age_months,
        s_age_years = age_years,
        s_running = running,
        s_eligible65 = eligible65,
        s_cesd = cesd,
        s_depyr = depyr,
        s_medicare = medicare,
        s_working = working,
        s_work_hours = work_hours,
        s_fulltime_worker = fulltime_worker,
        s_retired = retired,
        s_stable_job_title = stable_job_title,
        s_job_switcher = job_switcher,
        s_oop = oop,
        s_help_received_hours = help_received_hours
      )
    
    all_couples[[idx]] <- inner_join(
      index_df,
      spouse_df,
      by = c("spouse_id" = "s_hhidpn", "wave")
    ) %>%
      filter(
        between(i_age_months, MAIN_MIN_MONTHS, MAIN_MAX_MONTHS),
        between(s_age_months, MAIN_MIN_MONTHS, MAIN_MAX_MONTHS),
        s_age_months < CUTOFF_MONTHS
      ) %>%
      mutate(
        age_diff_months = i_age_months - s_age_months,
        both_working = as.integer(i_working == 1 & s_working == 1),
        both_fulltime = as.integer(i_fulltime_worker == 1 & s_fulltime_worker == 1),
        both_stable_job = as.integer(i_stable_job_title == 1 & s_stable_job_title == 1)
      )
  }
  
  cat(glue("Wave {w} ({WAVE_YEARS$year[idx]}): {nrow(wave_df)} person-wave observations\n"))
}

# ------------------------------------------------------------------------------
# 3. Build Analysis Data
# ------------------------------------------------------------------------------

section("Building Analysis Datasets")

df_full <- bind_rows(all_waves) %>%
  left_join(baseline, by = "hhidpn") %>%
  left_join(CPI_TABLE, by = "wave") %>%
  mutate(
    age_group = if_else(eligible65 == 1L, "Age 65+", "Under 65"),
    inflation_factor = CPI_REF / cpi,
    across(
      c(oop, private_premium_1, private_premium_2, private_premium_3,
        hmo_premium, ltc_premium, rx_premium, earnings, total_income,
        wealth, pension_income, social_security),
      ~ .x * inflation_factor,
      .names = "{.col}_real"
    ),
    physical_conditions = rowSums(
      cbind(hibp, diab, cancr, lung, heart, stroke, arthr),
      na.rm = TRUE
    ),
    functional_limitations = rowSums(
      cbind(adl, iadl),
      na.rm = TRUE
    ),
    private_premium_nonmissing = rowSums(!is.na(across(c(
      private_premium_1_real, private_premium_2_real, private_premium_3_real
    )))),
    private_premiums_real = rowSums(across(c(
      private_premium_1_real, private_premium_2_real, private_premium_3_real
    )), na.rm = TRUE),
    private_premiums_real = if_else(private_premium_nonmissing == 0, NA_real_, private_premiums_real),
    medicare_public_premium_nonmissing = rowSums(!is.na(across(c(
      hmo_premium_real, rx_premium_real
    )))),
    medicare_public_premiums_real = rowSums(across(c(
      hmo_premium_real, rx_premium_real
    )), na.rm = TRUE),
    medicare_public_premiums_real = if_else(medicare_public_premium_nonmissing == 0, NA_real_, medicare_public_premiums_real),
    premium_nonmissing = rowSums(!is.na(across(c(
      private_premium_1_real, private_premium_2_real, private_premium_3_real,
      hmo_premium_real, ltc_premium_real, rx_premium_real
    )))),
    total_premiums_real = rowSums(across(c(
      private_premium_1_real, private_premium_2_real, private_premium_3_real,
      hmo_premium_real, ltc_premium_real, rx_premium_real
    )), na.rm = TRUE),
    total_premiums_real = if_else(premium_nonmissing == 0, NA_real_, total_premiums_real),
    has_private_premium = as.integer(!is.na(private_premiums_real)),
    has_partd_premium = as.integer(!is.na(rx_premium_real)),
    has_medicare_hmo_premium = as.integer(!is.na(hmo_premium_real)),
    medicare_advantage_hmo = case_when(
      medicare == 1 & !is.na(hmo_premium_real) ~ 1,
      medicare == 1 & is.na(hmo_premium_real) ~ 0,
      TRUE ~ NA_real_
    ),
    traditional_medicare_partd = case_when(
      medicare == 1 & !is.na(rx_premium_real) ~ 1,
      medicare == 1 & is.na(rx_premium_real) ~ 0,
      TRUE ~ NA_real_
    ),
    no_observed_medicare_drug_or_hmo_premium = case_when(
      medicare == 1 & is.na(rx_premium_real) & is.na(hmo_premium_real) ~ 1,
      medicare == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    z_cesd_rev = z_std(-cesd),
    z_life_sat = z_std(life_sat),
    z_lonely_rev = z_std(-lonely),
    z_pos_affect = z_std(pos_affect),
    z_neg_affect_rev = z_std(-neg_affect)
  ) %>%
  rowwise() %>%
  mutate(
    mental_index = mean(c(
      z_cesd_rev, z_life_sat, z_lonely_rev, z_pos_affect, z_neg_affect_rev
    ), na.rm = TRUE),
    n_mhi_components = sum(!is.na(c(
      z_cesd_rev, z_life_sat, z_lonely_rev, z_pos_affect, z_neg_affect_rev
    )))
  ) %>%
  ungroup() %>%
  mutate(
    mental_index = if_else(is.nan(mental_index), NA_real_, mental_index),
    mental_index_complete = as.integer(n_mhi_components == 5),
    wave = factor(wave),
    year = factor(year)
  ) %>%
  arrange(hhidpn, as.integer(as.character(wave))) %>%
  group_by(hhidpn) %>%
  mutate(
    lead_cesd = lead(cesd),
    lead_depyr = lead(depyr),
    lead_mental_index = lead(mental_index)
  ) %>%
  ungroup()

df <- df_full %>%
  filter(between(age_months, MAIN_MIN_MONTHS, MAIN_MAX_MONTHS))

df_bw <- df %>% filter(!is.na(running), abs(running) <= MAIN_BW)
df_continuity <- df %>% filter(!is.na(running), abs(running) <= CONT_BW)

physical_condition_median <- median(df_bw$physical_conditions, na.rm = TRUE)
functional_limitation_median <- median(df_bw$functional_limitations, na.rm = TRUE)

# Physical-health heterogeneity uses a sharper comparison than a median split:
#   Better physical health: 0-1 diagnosed physical conditions
#   Poorer physical health: 3 or more diagnosed physical conditions
# Respondent-waves with exactly 2 conditions are left out of this heterogeneity split.
physical_low_max <- 1L
physical_high_min <- 3L
functional_any_cutoff <- 1L

pre65_mh_flags <- df_bw %>%
  filter(running < 0) %>%
  group_by(hhidpn) %>%
  summarise(
    pre65_any_depressed = case_when(
      any(depyr == 1, na.rm = TRUE) ~ 1,
      any(depyr == 0, na.rm = TRUE) ~ 0,
      TRUE ~ NA_real_
    ),
    pre65_mean_cesd = mean(cesd, na.rm = TRUE),
    pre65_mean_mhi = mean(mental_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pre65_mean_cesd = if_else(is.nan(pre65_mean_cesd), NA_real_, pre65_mean_cesd),
    pre65_mean_mhi = if_else(is.nan(pre65_mean_mhi), NA_real_, pre65_mean_mhi)
  )

pre65_cesd_cutoff <- median(pre65_mh_flags$pre65_mean_cesd, na.rm = TRUE)
pre65_mhi_cutoff <- median(pre65_mh_flags$pre65_mean_mhi, na.rm = TRUE)

df_bw <- df_bw %>%
  left_join(pre65_mh_flags, by = "hhidpn") %>%
  mutate(
    physical_burden_group = case_when(
      physical_conditions <= physical_low_max ~ "Better Physical Health (0-1 conditions)",
      physical_conditions >= physical_high_min ~ "Poorer Physical Health (3+ conditions)",
      TRUE ~ NA_character_
    ),
    functional_burden_group = case_when(
      functional_limitations == 0 ~ "No Functional Limitations",
      functional_limitations >= functional_any_cutoff ~ "Any Functional Limitations",
      TRUE ~ NA_character_
    ),
    pre65_depression_group = case_when(
      pre65_any_depressed == 1 ~ "Depressed before 65",
      pre65_any_depressed == 0 ~ "Not depressed before 65",
      TRUE ~ NA_character_
    ),
    pre65_cesd_group = case_when(
      pre65_mean_cesd >= pre65_cesd_cutoff ~ "High pre-65 CES-D",
      pre65_mean_cesd < pre65_cesd_cutoff ~ "Low pre-65 CES-D",
      TRUE ~ NA_character_
    ),
    pre65_mhi_group = case_when(
      pre65_mean_mhi <= pre65_mhi_cutoff ~ "Low pre-65 Mental Health",
      pre65_mean_mhi > pre65_mhi_cutoff ~ "High pre-65 Mental Health",
      TRUE ~ NA_character_
    )
  )

cat("Median physical conditions:", physical_condition_median, "\n")
cat("Physical-health split: 0-1 conditions vs 3+ conditions; exactly 2 conditions excluded from this split.\n")
cat("Median functional limitations:", functional_limitation_median, "\n")
cat("Functional-health split: 0 limitations vs 1+ limitations.\n")
cat("Pre-65 CES-D cutoff:", pre65_cesd_cutoff, "\n")
cat("Pre-65 Mental Health Index cutoff:", pre65_mhi_cutoff, "\n")

df_couples_full <- bind_rows(compact(all_couples)) %>%
  mutate(
    age_group = if_else(i_eligible65 == 1L, "Index age 65+", "Index under 65"),
    household_depyr_any = case_when(
      i_depyr == 1 | s_depyr == 1 ~ 1,
      i_depyr == 0 & s_depyr == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    wave = factor(wave),
    year = factor(year)
  )

df_couples_bw <- df_couples_full %>%
  filter(!is.na(i_running), abs(i_running) <= MAIN_BW)

df_couples_rd <- df_couples_bw %>%
  rename(
    running = i_running,
    eligible65 = i_eligible65,
    hhidpn = i_hhidpn,
    medicare = i_medicare
  )

cat("Main person-wave observations:", nrow(df), "\n")
cat("RD-bandwidth observations:", nrow(df_bw), "\n")
cat("Couple RD-bandwidth observations:", nrow(df_couples_bw), "\n")

section("Survey Weight Diagnostics")

weight_diagnostics <- tibble(
  weight = c("respondent_weight", "lb_weight"),
  nonmissing = c(
    sum(!is.na(df_bw$respondent_weight)),
    sum(!is.na(df_bw$lb_weight))
  ),
  positive = c(
    sum(df_bw$respondent_weight > 0, na.rm = TRUE),
    sum(df_bw$lb_weight > 0, na.rm = TRUE)
  ),
  full_sample_n = nrow(df_bw)
)

write_table(weight_diagnostics, "table_weight_diagnostics.csv")

# ------------------------------------------------------------------------------
# 4. Tables
# ------------------------------------------------------------------------------

section("Table 1: Summary Statistics")

summary_specs <- tribble(
  ~var, ~label,
  "cesd", "CES-D depressive symptoms (0-8)",
  "depyr", "Depressed in past year",
  "mental_index", "Mental Health Index (higher = better)",
  "n_mhi_components", "Number of Mental Health Index components",
  "mental_index_complete", "Complete Mental Health Index",
  "medicare", "Medicare coverage",
  "any_gov_ins", "Any government insurance",
  "medicaid", "Medicaid coverage",
  "va_ins", "VA insurance",
  "priv_ins_r", "Private insurance, respondent",
  "priv_ins_s", "Private insurance through spouse",
  "oop_real", "OOP spending (2016 USD)",
  "private_premiums_real", "Private insurance premiums (2016 USD)",
  "rx_premium_real", "Medicare Part D premium (2016 USD)",
  "hmo_premium_real", "Medicare Advantage/HMO premium (2016 USD)",
  "medicare_public_premiums_real", "Observed Medicare Part D/HMO premiums (2016 USD)",
  "total_premiums_real", "Total observed premiums (2016 USD)",
  "working", "Currently working",
  "fulltime_worker", "Working 35-45 hours/week",
  "work_hours", "Weekly work hours",
  "retired", "Retired",
  "stable_job_title", "Stable job title",
  "job_switcher", "Changed job title since last wave",
  "rx_drugs", "Regularly takes prescription drugs",
  "ssdi_receive", "Receiving SSDI",
  "earnings_real", "Earnings (2016 USD)",
  "total_income_real", "Total household income (2016 USD)",
  "wealth_real", "Net wealth (2016 USD)",
  "age_months", "Age in months",
  "female", "Female",
  "white", "White",
  "educ", "Years of education",
  "adl", "ADL difficulties",
  "iadl", "IADL difficulties",
  "mobility", "Mobility difficulties",
  "base_depyr", "Baseline depression, wave 3",
  "base_cesd", "Baseline CES-D, wave 3",
  "base_adl", "Baseline ADL, wave 3",
  "base_iadl", "Baseline IADL, wave 3",
  "base_mobility", "Baseline mobility, wave 3"
)

summary_specs_use <- summary_specs %>%
  filter(var %in% names(df_bw)) %>%
  mutate(nonmissing = map_int(var, ~ sum(!is.na(df_bw[[.x]])))) %>%
  filter(nonmissing > 0)

t01 <- summary_by_group(df_bw, summary_specs_use$var, summary_specs_use$label, "age_group")
write_table(t01, "table_01_summary_statistics_long.csv")

section("Table 2: Main RD Estimates")

main_specs <- tribble(
  ~var, ~label,
  "cesd", "CES-D",
  "depyr", "Depressed in past year",
  "mental_index", "Mental Health Index",
  "oop_real", "OOP spending"
)

t02 <- bind_rows(
  rd_reduced_form(df_bw, "medicare", "Medicare coverage", estimand = "First stage"),
  make_rd_table(df_bw, main_specs, "Main RD sample", include_fuzzy = TRUE) %>%
    select(-sample)
)
write_table(t02, "table_02_main_rd_results.csv")

section("Table 3: Financial Protection Mechanisms")

financial_specs <- tribble(
  ~var, ~label,
  "oop_real", "OOP medical spending (2016 USD)",
  "private_premiums_real", "Private insurance premiums (2016 USD)",
  "rx_premium_real", "Medicare Part D premium (2016 USD)",
  "hmo_premium_real", "Medicare Advantage/HMO premium (2016 USD)",
  "medicare_public_premiums_real", "Observed Medicare Part D/HMO premiums (2016 USD)",
  "total_premiums_real", "Total observed premiums (2016 USD)",
  "plan_count", "Number of health insurance plans",
  "any_gov_ins", "Any government insurance",
  "medicaid", "Medicaid coverage",
  "va_ins", "VA insurance",
  "priv_ins_r", "Private insurance, respondent",
  "priv_ins_s", "Private insurance through spouse",
  "social_security_real", "Social Security income (2016 USD)",
  "pension_income_real", "Pension income (2016 USD)",
  "earnings_real", "Earnings (2016 USD)",
  "total_income_real", "Total household income (2016 USD)",
  "wealth_real", "Net wealth (2016 USD)"
)

t03 <- make_rd_table(df_bw, financial_specs, "Financial mechanisms", include_fuzzy = FALSE) %>%
  rename(mechanism = outcome)
write_table(t03, "table_03_financial_mechanisms.csv")

section("Table 4: Predetermined Covariate Continuity")

predetermined_specs <- tribble(
  ~var, ~label,
  "female", "Female",
  "white", "White",
  "educ", "Years of education",
  "base_depyr", "Baseline depression, wave 3",
  "base_cesd", "Baseline CES-D, wave 3",
  "base_adl", "Baseline ADL, wave 3",
  "base_iadl", "Baseline IADL, wave 3",
  "base_mobility", "Baseline mobility, wave 3"
)

t04 <- map2_dfr(
  predetermined_specs$var,
  predetermined_specs$label,
  ~ local_linear_jump(df_continuity, .x, .y, h = CONT_BW)
)
write_table(t04, "table_04_predetermined_covariate_continuity_36mo.csv")

section("Table 5: Placebo Cutoff Tests")

placebo_ages <- c(61:64, 66:69)

t05 <- map_dfr(placebo_ages, function(cut_age) {
  d <- df_full %>%
    mutate(
      placebo_running = age_months - cut_age * 12L,
      placebo_above = as.integer(age_months >= cut_age * 12L)
    ) %>%
    filter(abs(placebo_running) <= MAIN_BW) %>%
    drop_na(cesd, placebo_running, placebo_above, wave, hhidpn)
  
  if (nrow(d) < 300 || n_distinct(d$placebo_above) < 2) {
    return(tibble(cutoff_age = cut_age, estimate = NA_real_, se = NA_real_,
                  p_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
                  n = nrow(d)))
  }
  
  d <- d %>% mutate(w_tri = pmax(0, 1 - abs(placebo_running) / MAIN_BW))
  m <- feols(
    cesd ~ placebo_above + placebo_running + I(placebo_running * placebo_above) + factor(wave),
    data = d,
    weights = ~ w_tri,
    cluster = ~ hhidpn
  )
  
  est <- coef(m)["placebo_above"]
  se1 <- se(m)["placebo_above"]
  p1 <- fixest::pvalue(m)["placebo_above"]
  
  tibble(
    cutoff_age = cut_age,
    estimate = est,
    se = se1,
    p_value = p1,
    ci_lower = est - 1.96 * se1,
    ci_upper = est + 1.96 * se1,
    n = nobs(m)
  )
}) %>%
  bind_rows(
    rd_reduced_form(df_bw, "cesd", "CES-D") %>%
      transmute(cutoff_age = 65L, estimate, se, p_value, ci_lower, ci_upper, n)
  ) %>%
  arrange(cutoff_age) %>%
  mutate(true_cutoff = cutoff_age == 65L, stars = star_p(p_value))

write_table(t05, "table_05_placebo_cutoffs.csv")

section("Table 6: Bandwidth Robustness")

bw_grid <- c(12, 24, 36, 48, 60)
t06 <- map_dfr(bw_grid, function(bw) {
  rd_reduced_form(
    data = df %>% filter(!is.na(running), abs(running) <= bw),
    y = "cesd",
    label = "CES-D",
    h = bw,
    min_n = 100
  ) %>%
    mutate(bandwidth = bw, .before = estimand)
})
write_table(t06, "table_06_bandwidth_robustness.csv")

section("Table 7: Donut-Hole Robustness")

donut_sizes <- c(0, 1, 2, 3, 6, 12)
t07 <- map_dfr(donut_sizes, function(ds) {
  d <- df_bw
  if (ds > 0) d <- d %>% filter(abs(running) > ds)
  rd_reduced_form(d, "cesd", "CES-D", min_n = 100) %>%
    mutate(
      donut_size = ds,
      specification = if_else(ds == 0, "No exclusion", glue("Exclude |running| <= {ds} months")),
      .before = estimand
    )
})
write_table(t07, "table_07_donut_robustness.csv")

section("Table 8: Running-Variable Density Test")

density_x <- df$running[!is.na(df$running) & abs(df$running) <= MAIN_BW]
density_test <- rddensity(X = density_x, c = 0)
t08 <- tibble(
  test = "rddensity",
  p_value = density_test$test$p_j,
  n = length(density_x)
)
write_table(t08, "table_08_density_test.csv")

section("Tables 9-25: Mechanisms, Subgroups, and Robustness")

heterogeneity_specs <- list(
  "Male" = expr(female == 0),
  "Female" = expr(female == 1),
  "White" = expr(white == 1),
  "Non-white" = expr(white == 0),
  "Not depressed at baseline" = expr(base_depyr == 0),
  "Depressed at baseline" = expr(base_depyr == 1)
)  

t09 <- imap_dfr(heterogeneity_specs, function(condition, group_name) {
  rd_reduced_form(
    df_bw %>% filter(!!condition),
    "cesd",
    "CES-D",
    estimand = "Reduced form subgroup"
  ) %>%
    mutate(group = group_name, .before = estimand)
})
write_table(t09, "table_09_heterogeneity_cesd.csv")

mechanism_specs <- tribble(
  ~var, ~label,
  "working", "Currently working",
  "fulltime_worker", "Working 35-45 hours/week",
  "work_hours", "Weekly work hours",
  "retired", "Retired",
  "stable_job_title", "Stable job title",
  "job_switcher", "Changed job title since last wave",
  "num_jobs", "Number of jobs reported",
  "rx_drugs", "Regularly takes prescription drugs",
  "rx_premium_real", "Medicare Part D premium (2016 USD)",
  "ssdi_receive", "Receiving SSDI",
  "ssi_ssdi_income", "SSI/SSDI income",
  "ssi_income", "SSI income"
)

t10 <- make_rd_table(df_bw, mechanism_specs, "Labor, prescription-drug, and disability channels", FALSE) %>%
  rename(mechanism = outcome)
write_table(t10, "table_10_labor_rx_disability_channels.csv")

labor_flags <- df_bw %>%
  group_by(hhidpn) %>%
  summarise(
    has_work_obs_below = any(!is.na(working) & running < 0),
    has_work_obs_above = any(!is.na(working) & running >= 0),
    worked_below = any(working == 1 & running < 0, na.rm = TRUE),
    worked_above = any(working == 1 & running >= 0, na.rm = TRUE),
    fulltime_below = any(fulltime_worker == 1 & running < 0, na.rm = TRUE),
    fulltime_above = any(fulltime_worker == 1 & running >= 0, na.rm = TRUE),
    samejob_below = any(stable_job_title == 1 & running < 0, na.rm = TRUE),
    samejob_above = any(stable_job_title == 1 & running >= 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cont_working = as.integer(worked_below & worked_above),
    cont_fulltime = as.integer(fulltime_below & fulltime_above),
    cont_samejob = as.integer(samejob_below & samejob_above),
    cont_nonworking = as.integer(has_work_obs_below & has_work_obs_above & !worked_below & !worked_above)
  )

df_bw_labor <- df_bw %>% left_join(labor_flags, by = "hhidpn")

t11 <- bind_rows(
  rd_reduced_form(df_bw_labor %>% filter(cont_working == 1), "cesd", "Continuously working", estimand = "Descriptive subsample"),
  rd_reduced_form(df_bw_labor %>% filter(cont_fulltime == 1), "cesd", "Continuously full-time", estimand = "Descriptive subsample"),
  rd_reduced_form(df_bw_labor %>% filter(cont_samejob == 1), "cesd", "Stable same job title", estimand = "Descriptive subsample"),
  rd_reduced_form(df_bw_labor %>% filter(cont_nonworking == 1), "cesd", "Continuously nonworking", estimand = "Descriptive subsample")
)
write_table(t11, "table_11_labor_status_stability.csv")

bw_opt <- rdbwselect(
  y = df_bw$cesd[complete.cases(df_bw[, c("cesd", "running")])],
  x = df_bw$running[complete.cases(df_bw[, c("cesd", "running")])],
  c = 0,
  kernel = "triangular"
)
opt_bw <- bw_opt$bws[1, 1]
t12 <- rd_reduced_form(df_bw, "cesd", "CES-D at optimal bandwidth", h = opt_bw) %>%
  mutate(bandwidth = opt_bw, .before = estimand)
write_table(t12, "table_12_optimal_bandwidth.csv")

component_specs <- tribble(
  ~var, ~label,
  "z_cesd_rev", "Reversed standardized CES-D",
  "z_life_sat", "Standardized life satisfaction",
  "z_lonely_rev", "Reversed standardized loneliness",
  "z_pos_affect", "Standardized positive affect",
  "z_neg_affect_rev", "Reversed standardized negative affect"
)

df_work_gt37 <- df_bw %>% filter(working == 1, !is.na(work_hours), work_hours > 37)
df_work_gt10 <- df_bw %>% filter(working == 1, !is.na(work_hours), work_hours > 10)

t16 <- make_rd_table(df_work_gt37, component_specs, "Workers >37 hours/week", TRUE)
t17 <- make_rd_table(df_work_gt10, component_specs, "Workers >10 hours/week", TRUE)
t17a <- bind_rows(
  make_rd_table(df_work_gt37, tribble(~var, ~label, "mental_index", "Mental Health Index"), "Workers >37 hours/week", TRUE),
  make_rd_table(df_work_gt10, tribble(~var, ~label, "mental_index", "Mental Health Index"), "Workers >10 hours/week", TRUE)
)

write_table(t16, "table_16_mhi_components_workers_gt37.csv")
write_table(t17, "table_17_mhi_components_workers_gt10.csv")
write_table(t17a, "table_17a_mental_index_working_samples.csv")

t18 <- bind_rows(
  rd_reduced_form(df_couples_rd, "household_depyr_any", "Respondent or spouse depressed in past year",
                  estimand = "Reduced form, couple sample"),
  rd_fuzzy(df_couples_rd, "household_depyr_any", "Respondent or spouse depressed in past year",
           estimand = "Fuzzy RD LATE, couple sample")
)
write_table(t18, "table_18_household_depression_couple_sample.csv")

t19 <- bind_rows(
  rd_reduced_form(df_couples_rd %>% filter(both_working == 1), "i_cesd", "Respondent and spouse both working",
                  estimand = "Reduced form, couple subsample"),
  rd_reduced_form(df_couples_rd %>% filter(both_fulltime == 1), "i_cesd", "Respondent and spouse both full-time",
                  estimand = "Reduced form, couple subsample"),
  rd_reduced_form(df_couples_rd %>% filter(both_stable_job == 1), "i_cesd", "Respondent and spouse both stable job title",
                  estimand = "Reduced form, couple subsample")
)
write_table(t19, "table_19_spouse_labor_stability.csv")

nonworking_specs <- tribble(
  ~var, ~label,
  "cesd", "CES-D",
  "depyr", "Depressed in past year",
  "mental_index", "Mental Health Index",
  "oop_real", "OOP spending"
)

df_nonworking <- df_bw_labor %>% filter(cont_nonworking == 1)
t20 <- make_rd_table(df_nonworking, nonworking_specs, "Stably nonworking respondents", TRUE)
write_table(t20, "table_20_nonworking_population_rd.csv")

df_nonworking_ins <- df_nonworking %>%
  mutate(
    age_side = if_else(running < 0, "Below 65", "Age 65+"),
    uninsured = case_when(
      medicare == 0 & medicaid == 0 & va_ins == 0 & priv_ins_r == 0 & priv_ins_s == 0 ~ 1,
      medicare == 1 | medicaid == 1 | va_ins == 1 | priv_ins_r == 1 | priv_ins_s == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    medicare_medicaid = case_when(
      medicare == 1 & medicaid == 1 ~ 1,
      medicare %in% c(0, 1) & medicaid %in% c(0, 1) ~ 0,
      TRUE ~ NA_real_
    )
  )

insurance_specs <- tribble(
  ~var, ~label,
  "medicare", "Medicare coverage",
  "medicaid", "Medicaid coverage",
  "medicare_medicaid", "Medicare and Medicaid dual coverage",
  "any_gov_ins", "Any government insurance",
  "priv_ins_r", "Private insurance, respondent",
  "priv_ins_s", "Private insurance through spouse",
  "va_ins", "VA insurance",
  "uninsured", "Uninsured"
)

t20a <- df_nonworking_ins %>%
  group_by(age_side) %>%
  group_modify(~ summary_long(.x, insurance_specs$var, insurance_specs$label)) %>%
  ungroup() %>%
  mutate(share_percent = 100 * mean)
write_table(t20a, "table_20a_nonworking_insurance_composition.csv")

t20b <- df_nonworking %>%
  group_by(hhidpn) %>%
  summarise(
    medicare_below = any(medicare == 1 & running < 0, na.rm = TRUE),
    medicaid_below = any(medicaid == 1 & running < 0, na.rm = TRUE),
    spouse_below = any(priv_ins_s == 1 & running < 0, na.rm = TRUE),
    va_below = any(va_ins == 1 & running < 0, na.rm = TRUE),
    uninsured_below = any(medicare == 0 & medicaid == 0 & va_ins == 0 &
                            priv_ins_r == 0 & priv_ins_s == 0 & running < 0,
                          na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    coverage_group = case_when(
      medicare_below ~ "Medicare before 65",
      spouse_below ~ "Spouse insurance before 65",
      uninsured_below ~ "Uninsured before 65",
      medicaid_below ~ "Medicaid before 65",
      va_below ~ "VA coverage before 65",
      TRUE ~ "Other coverage"
    )
  ) %>%
  count(coverage_group, name = "n") %>%
  mutate(percent = 100 * n / sum(n)) %>%
  arrange(desc(percent))
write_table(t20b, "table_20b_nonworking_primary_pre65_coverage.csv")

panel_iv_row <- function(data, y, label) {
  d <- data %>%
    drop_na(all_of(c(y, "medicare", "eligible65", "running", "hhidpn", "wave"))) %>%
    mutate(wave = factor(wave))
  
  if (nrow(d) < 300 || n_distinct(d$hhidpn) < 100 || n_distinct(d$eligible65) < 2) {
    return(tibble(outcome = label, estimate = NA_real_, se = NA_real_,
                  p_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
                  n = nrow(d), stars = ""))
  }
  
  fml <- as.formula(paste0(
    y,
    " ~ running + I(running * eligible65) | hhidpn + wave | medicare ~ eligible65"
  ))
  m <- feols(fml, data = d, cluster = ~ hhidpn)
  term <- names(coef(m))[str_detect(names(coef(m)), "medicare")][1]
  est <- coef(m)[term]
  se1 <- se(m)[term]
  p1 <- fixest::pvalue(m)[term]
  
  tibble(
    outcome = label,
    estimate = est,
    se = se1,
    p_value = p1,
    ci_lower = est - 1.96 * se1,
    ci_upper = est + 1.96 * se1,
    n = nobs(m),
    stars = star_p(p1)
  )
}

t21 <- map2_dfr(main_specs$var, main_specs$label, ~ panel_iv_row(df_bw, .x, .y)) %>%
  mutate(estimand = "Panel IV with respondent and wave fixed effects", .before = outcome)
write_table(t21, "table_21_panel_iv_individual_fe.csv")

t21a <- {
  d <- df_bw %>%
    drop_na(medicare, eligible65, running, hhidpn, wave) %>%
    mutate(wave = factor(wave))
  m <- feols(
    medicare ~ eligible65 + running + I(running * eligible65) | hhidpn + wave,
    data = d,
    cluster = ~ hhidpn
  )
  est <- coef(m)["eligible65"]
  se1 <- se(m)["eligible65"]
  tibble(
    model = "Panel first stage with respondent and wave fixed effects",
    estimate = est,
    se = se1,
    p_value = fixest::pvalue(m)["eligible65"],
    F_stat = (est / se1)^2,
    n = nobs(m),
    persons = n_distinct(d$hhidpn),
    stars = star_p(p_value)
  )
}
write_table(t21a, "table_21a_panel_iv_first_stage.csv")

forward_specs <- tribble(
  ~var, ~label,
  "lead_cesd", "CES-D next observed wave",
  "lead_depyr", "Depressed in past year next observed wave",
  "lead_mental_index", "Mental Health Index next observed wave"
)
t22 <- make_rd_table(df_bw, forward_specs, "Next-wave outcomes", TRUE)
write_table(t22, "table_22_next_wave_mental_health_outcomes.csv")

rx_channel_specs <- tribble(
  ~var, ~label,
  "rx_drugs", "Regularly takes prescription drugs",
  "rx_premium_real", "Medicare Part D premium (2016 USD)",
  "hmo_premium_real", "Medicare Advantage/HMO premium (2016 USD)",
  "private_premiums_real", "Private insurance premiums (2016 USD)",
  "medicare_public_premiums_real", "Observed Medicare Part D/HMO premiums (2016 USD)",
  "total_premiums_real", "Total observed premiums (2016 USD)",
  "priv_ins_r", "Private insurance, respondent",
  "any_gov_ins", "Any government insurance",
  "plan_count", "Number of health insurance plans"
)
t23 <- make_rd_table(df_bw, rx_channel_specs, "Prescription-drug and insurance channel outcomes", FALSE)
write_table(t23, "table_23_prescription_drug_insurance_channel.csv")

section("Table 23A: Premium Variable Definitions")

t23a <- tribble(
  ~variable, ~concept, ~interpretation,
  "private_premiums_real", "Private health insurance premiums", "Sum of PRPRM1-PRPRM3, converted to 2016 dollars. This captures private health insurance plan premiums, not prescription-only premiums.",
  "rx_premium_real", "Medicare Part D premium", "Medicare prescription drug plan premium, converted to 2016 dollars. This is only observed for Medicare Part D plans.",
  "hmo_premium_real", "Medicare/Medicaid HMO premium", "Medicare Advantage/Medicare HMO or Medicaid HMO plan premium, converted to 2016 dollars. For Medicare beneficiaries, this is interpreted as Medicare Advantage/HMO coverage.",
  "rx_drugs", "Prescription drug use", "Indicator for regularly taking prescription drugs."
)
write_table(t23a, "table_23a_premium_variable_definitions.csv")

section("Table 23B: Premiums and Prescription Drug Use by Age-65 Status")

t23b <- df_bw %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    persons = n_distinct(hhidpn),
    private_premium_obs = sum(!is.na(private_premiums_real)),
    mean_private_premium = mean(private_premiums_real, na.rm = TRUE),
    median_private_premium = median(private_premiums_real, na.rm = TRUE),
    partd_premium_obs = sum(!is.na(rx_premium_real)),
    mean_partd_premium = mean(rx_premium_real, na.rm = TRUE),
    median_partd_premium = median(rx_premium_real, na.rm = TRUE),
    ma_hmo_premium_obs = sum(!is.na(hmo_premium_real)),
    mean_ma_hmo_premium = mean(hmo_premium_real, na.rm = TRUE),
    median_ma_hmo_premium = median(hmo_premium_real, na.rm = TRUE),
    rx_drug_use = mean(rx_drugs, na.rm = TRUE),
    medicare_share = mean(medicare, na.rm = TRUE),
    private_insurance_share = mean(priv_ins_r == 1 | priv_ins_s == 1, na.rm = TRUE),
    medicaid_share = mean(medicaid, na.rm = TRUE),
    .groups = "drop"
  )
write_table(t23b, "table_23b_premiums_rx_use_by_age65_status.csv")

section("Table 23C: Under-65 Premiums by Insurance Status")

t23c <- df_bw %>%
  filter(eligible65 == 0) %>%
  mutate(
    uninsured = case_when(
      medicare == 0 & medicaid == 0 & va_ins == 0 & priv_ins_r == 0 & priv_ins_s == 0 ~ 1,
      medicare == 1 | medicaid == 1 | va_ins == 1 | priv_ins_r == 1 | priv_ins_s == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    insurance_group = case_when(
      uninsured == 1 ~ "Uninsured",
      priv_ins_r == 1 | priv_ins_s == 1 ~ "Private insurance",
      medicaid == 1 ~ "Medicaid",
      medicare == 1 ~ "Medicare before 65",
      va_ins == 1 ~ "VA",
      TRUE ~ "Other/unknown"
    )
  ) %>%
  group_by(insurance_group) %>%
  summarise(
    n = n(),
    persons = n_distinct(hhidpn),
    private_premium_obs = sum(!is.na(private_premiums_real)),
    mean_private_premium = mean(private_premiums_real, na.rm = TRUE),
    median_private_premium = median(private_premiums_real, na.rm = TRUE),
    partd_premium_obs = sum(!is.na(rx_premium_real)),
    mean_partd_premium = mean(rx_premium_real, na.rm = TRUE),
    rx_drug_use = mean(rx_drugs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))
write_table(t23c, "table_23c_under65_premiums_by_insurance_status.csv")

section("Table 23D: RD Estimates for Premium and Drug-Use Outcomes")

premium_rd_specs <- tribble(
  ~var, ~label,
  "private_premiums_real", "Private insurance premiums",
  "rx_premium_real", "Medicare Part D premium",
  "hmo_premium_real", "Medicare Advantage/HMO premium",
  "medicare_public_premiums_real", "Observed Medicare Part D/HMO premiums",
  "total_premiums_real", "Total observed premiums",
  "rx_drugs", "Regularly takes prescription drugs"
)

t23d <- make_rd_table(df_bw, premium_rd_specs, "Premium and drug-use outcomes", include_fuzzy = FALSE)
write_table(t23d, "table_23d_premium_drug_use_rd.csv")

section("Table 23E: Medicare Plan-Type Indicators Among Medicare Beneficiaries")

t23e <- df_bw %>%
  filter(medicare == 1) %>%
  mutate(
    plan_type_observed = case_when(
      medicare_advantage_hmo == 1 ~ "Medicare Advantage/HMO premium observed",
      traditional_medicare_partd == 1 ~ "Traditional Medicare + Part D premium observed",
      no_observed_medicare_drug_or_hmo_premium == 1 ~ "Medicare, no observed Part D/HMO premium",
      TRUE ~ "Other/unknown"
    )
  ) %>%
  count(age_group, plan_type_observed, name = "person_waves") %>%
  group_by(age_group) %>%
  mutate(percent = 100 * person_waves / sum(person_waves)) %>%
  ungroup() %>%
  arrange(age_group, desc(percent))
write_table(t23e, "table_23e_medicare_plan_type_indicators.csv")

section("Table 23F: Medicare Advantage/HMO vs Other Medicare Respondents")

t23f <- bind_rows(
  make_rd_table(df_bw %>% filter(medicare_advantage_hmo == 1), main_specs, "Medicare Advantage/HMO premium observed", include_fuzzy = FALSE),
  make_rd_table(df_bw %>% filter(medicare == 1, medicare_advantage_hmo == 0), main_specs, "Medicare without observed HMO premium", include_fuzzy = FALSE)
)
write_table(t23f, "table_23f_medicare_advantage_hmo_descriptive_rd.csv")

df_work_bins <- df_bw %>%
  filter(working == 1, !is.na(work_hours), work_hours >= 10) %>%
  mutate(
    work_hour_bin = case_when(
      work_hours < 20 ~ "10-20 hours",
      work_hours < 30 ~ "20-30 hours",
      work_hours < 40 ~ "30-40 hours",
      work_hours >= 40 ~ "40+ hours",
      TRUE ~ NA_character_
    ),
    work_hour_bin = factor(work_hour_bin, levels = c("10-20 hours", "20-30 hours", "30-40 hours", "40+ hours"))
  ) %>%
  filter(!is.na(work_hour_bin))

t24_counts <- df_work_bins %>%
  group_by(work_hour_bin) %>%
  summarise(persons = n_distinct(hhidpn), person_waves = n(),
            mean_hours = mean(work_hours, na.rm = TRUE), .groups = "drop")

t24 <- df_work_bins %>%
  group_by(work_hour_bin) %>%
  group_split() %>%
  map_dfr(function(d) {
    lab <- as.character(unique(d$work_hour_bin))
    make_rd_table(d, main_specs, lab, TRUE) %>%
      mutate(work_hour_bin = lab, .before = sample)
  })
write_table(t24_counts, "table_24a_work_hour_bin_counts.csv")
write_table(t24, "table_24_work_hour_bins_rd.csv")

income_cutoff <- median(df_bw$total_income_real, na.rm = TRUE)
df_income <- df_bw %>%
  mutate(
    income_group = case_when(
      total_income_real <= income_cutoff ~ "Below median income",
      total_income_real > income_cutoff ~ "Above median income",
      TRUE ~ NA_character_
    ),
    income_group = factor(income_group, levels = c("Below median income", "Above median income"))
  ) %>%
  filter(!is.na(income_group))

t25_counts <- df_income %>%
  group_by(income_group) %>%
  summarise(persons = n_distinct(hhidpn), person_waves = n(),
            mean_income = mean(total_income_real, na.rm = TRUE),
            median_income = median(total_income_real, na.rm = TRUE),
            .groups = "drop")

t25 <- df_income %>%
  group_by(income_group) %>%
  group_split() %>%
  map_dfr(function(d) {
    lab <- as.character(unique(d$income_group))
    make_rd_table(d, main_specs, lab, TRUE) %>%
      mutate(income_group = lab, .before = sample)
  })
write_table(t25_counts, "table_25a_income_group_counts.csv")
write_table(t25, "table_25_income_heterogeneity.csv")

section("Table 31: Physical Health Burden Measures")

t31 <- df_bw %>%
  summarise(
    persons = n_distinct(hhidpn),
    person_waves = n(),
    mean_physical_conditions = mean(physical_conditions, na.rm = TRUE),
    median_physical_conditions = median(physical_conditions, na.rm = TRUE),
    mean_functional_limitations = mean(functional_limitations, na.rm = TRUE),
    median_functional_limitations = median(functional_limitations, na.rm = TRUE),
    physical_condition_median = physical_condition_median,
    functional_limitation_median = functional_limitation_median,
    physical_low_max = physical_low_max,
    physical_high_min = physical_high_min,
    functional_any_cutoff = functional_any_cutoff
  )

write_table(t31, "table_31_physical_health_burden_summary.csv")

t31a <- df_bw %>%
  count(physical_conditions, name = "person_waves") %>%
  mutate(percent = 100 * person_waves / sum(person_waves))

write_table(t31a, "table_31a_physical_conditions_distribution.csv")

t31b <- df_bw %>%
  count(functional_limitations, name = "person_waves") %>%
  mutate(percent = 100 * person_waves / sum(person_waves))

write_table(t31b, "table_31b_functional_limitations_distribution.csv")

section("Table 32: Medicare Effects by Physical Health Status")

t32 <- df_bw %>%
  filter(!is.na(physical_burden_group)) %>%
  group_by(physical_burden_group) %>%
  group_split() %>%
  map_dfr(function(d) {
    lab <- unique(d$physical_burden_group)
    make_rd_table(d, main_specs, lab, TRUE) %>%
      mutate(physical_burden_group = lab, .before = sample)
  })

write_table(t32, "table_32_physical_health_status_heterogeneity.csv")

section("Table 33: Medicare Effects by Functional Limitation Burden")

t33 <- df_bw %>%
  filter(!is.na(functional_burden_group)) %>%
  group_by(functional_burden_group) %>%
  group_split() %>%
  map_dfr(function(d) {
    lab <- unique(d$functional_burden_group)
    make_rd_table(d, main_specs, lab, TRUE) %>%
      mutate(functional_burden_group = lab, .before = sample)
  })

write_table(t33, "table_33_functional_limitation_burden_heterogeneity.csv")

section("Table 34: Pre-65 Mental Health Group Counts")

t34 <- df_bw %>%
  distinct(hhidpn, pre65_any_depressed, pre65_mean_cesd, pre65_mean_mhi,
           pre65_depression_group, pre65_cesd_group, pre65_mhi_group) %>%
  summarise(
    persons_with_pre65_depression_info = sum(!is.na(pre65_depression_group)),
    persons_depressed_before65 = sum(pre65_depression_group == "Depressed before 65", na.rm = TRUE),
    persons_not_depressed_before65 = sum(pre65_depression_group == "Not depressed before 65", na.rm = TRUE),
    share_depressed_before65 = mean(pre65_depression_group == "Depressed before 65", na.rm = TRUE),
    pre65_cesd_cutoff = pre65_cesd_cutoff,
    pre65_mhi_cutoff = pre65_mhi_cutoff,
    persons_low_pre65_mhi = sum(pre65_mhi_group == "Low pre-65 Mental Health", na.rm = TRUE),
    persons_high_pre65_mhi = sum(pre65_mhi_group == "High pre-65 Mental Health", na.rm = TRUE)
  )

write_table(t34, "table_34_pre65_mental_health_group_counts.csv")

section("Table 35: Medicare Effects by Pre-65 Depression Status")

t35 <- df_bw %>%
  filter(!is.na(pre65_depression_group)) %>%
  group_by(pre65_depression_group) %>%
  group_split() %>%
  map_dfr(function(d) {
    lab <- unique(d$pre65_depression_group)
    make_rd_table(d, main_specs, lab, TRUE) %>%
      mutate(pre65_depression_group = lab, .before = sample)
  })

write_table(t35, "table_35_pre65_depression_heterogeneity.csv")
print(t35, n = Inf)

section("Table 36: Medicare Effects by Pre-65 CES-D Severity")

t36 <- df_bw %>%
  filter(!is.na(pre65_cesd_group)) %>%
  group_by(pre65_cesd_group) %>%
  group_split() %>%
  map_dfr(function(d) {
    lab <- unique(d$pre65_cesd_group)
    make_rd_table(d, main_specs, lab, TRUE) %>%
      mutate(pre65_cesd_group = lab, .before = sample)
  })

write_table(t36, "table_36_pre65_cesd_severity_heterogeneity.csv")
print(t36, n = Inf)

section("Table 37: Medicare Effects by Pre-65 Mental Health Index")

t37 <- df_bw %>%
  filter(!is.na(pre65_mhi_group)) %>%
  group_by(pre65_mhi_group) %>%
  group_split() %>%
  map_dfr(function(d) {
    lab <- unique(d$pre65_mhi_group)
    make_rd_table(d, main_specs, lab, TRUE) %>%
      mutate(pre65_mhi_group = lab, .before = sample)
  })

write_table(t37, "table_37_pre65_mental_health_index_heterogeneity.csv")
print(t37, n = Inf)

# ------------------------------------------------------------------------------
# 4B. Survey Weights and Medicaid Robustness
# ------------------------------------------------------------------------------

section("Survey Weights and Medicaid Robustness")

# This section keeps the same RD design and adds the checks requested for Week 4:
#   1. survey-weighted version of the preferred main RD specification
#   2. excluding Medicaid recipients
#   3. excluding lower-income respondents
#   4. excluding both Medicaid recipients and lower-income respondents
#
# The preferred main specification remains the unweighted fuzzy RD.
# The weighted estimates use the general respondent weight, not the Leave-Behind
# psychosocial questionnaire weight.

rd_reduced_form_weighted <- function(data, y, label,
                                     weight_var = "respondent_weight",
                                     x = "running",
                                     h = MAIN_BW,
                                     cluster = "hhidpn",
                                     estimand = "Weighted reduced form",
                                     min_n = 300) {
  if (!all(c(y, x, cluster, weight_var) %in% names(data))) {
    return(tibble(
      estimand = estimand,
      outcome = label,
      estimate = NA_real_,
      se = NA_real_,
      p_value = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      n = 0L,
      stars = ""
    ))
  }
  
  d <- data %>%
    drop_na(all_of(c(y, x, cluster, weight_var))) %>%
    filter(.data[[weight_var]] > 0)
  
  if (nrow(d) < min_n || n_distinct(d[[x]] >= 0) < 2) {
    return(tibble(
      estimand = estimand,
      outcome = label,
      estimate = NA_real_,
      se = NA_real_,
      p_value = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      n = nrow(d),
      stars = ""
    ))
  }
  
  rd <- rdrobust(
    y = d[[y]],
    x = d[[x]],
    c = 0,
    h = h,
    kernel = "triangular",
    weights = d[[weight_var]],
    cluster = d[[cluster]]
  )
  
  rd_extract(rd, label, estimand, nrow(d))
}

rd_fuzzy_weighted <- function(data, y, label,
                              treatment = "medicare",
                              weight_var = "respondent_weight",
                              x = "running",
                              h = MAIN_BW,
                              cluster = "hhidpn",
                              estimand = "Weighted fuzzy RD LATE",
                              min_n = 300) {
  if (!all(c(y, treatment, x, cluster, weight_var) %in% names(data))) {
    return(tibble(
      estimand = estimand,
      outcome = label,
      estimate = NA_real_,
      se = NA_real_,
      p_value = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      n = 0L,
      stars = ""
    ))
  }
  
  d <- data %>%
    drop_na(all_of(c(y, treatment, x, cluster, weight_var))) %>%
    filter(.data[[weight_var]] > 0)
  
  if (nrow(d) < min_n || n_distinct(d[[x]] >= 0) < 2 || n_distinct(d[[treatment]]) < 2) {
    return(tibble(
      estimand = estimand,
      outcome = label,
      estimate = NA_real_,
      se = NA_real_,
      p_value = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      n = nrow(d),
      stars = ""
    ))
  }
  
  rd <- rdrobust(
    y = d[[y]],
    x = d[[x]],
    fuzzy = d[[treatment]],
    c = 0,
    h = h,
    kernel = "triangular",
    weights = d[[weight_var]],
    cluster = d[[cluster]]
  )
  
  rd_extract(rd, label, estimand, nrow(d))
}

make_weighted_rd_table <- function(data, specs, sample_label,
                                   weight_var = "respondent_weight",
                                   include_fuzzy = TRUE) {
  specs_use <- specs %>%
    filter(var %in% names(data)) %>%
    mutate(nonmissing = map_int(var, ~ sum(!is.na(data[[.x]])))) %>%
    filter(nonmissing >= 100)
  
  if (nrow(specs_use) == 0) return(empty_rd_table(sample_label))
  
  reduced <- map2_dfr(
    specs_use$var,
    specs_use$label,
    ~ rd_reduced_form_weighted(
      data = data,
      y = .x,
      label = .y,
      weight_var = weight_var
    )
  )
  
  if (!include_fuzzy) {
    return(reduced %>% mutate(sample = sample_label, .before = estimand))
  }
  
  fuzzy <- map2_dfr(
    specs_use$var,
    specs_use$label,
    ~ rd_fuzzy_weighted(
      data = data,
      y = .x,
      label = .y,
      weight_var = weight_var
    )
  )
  
  bind_rows(reduced, fuzzy) %>%
    mutate(sample = sample_label, .before = estimand)
}

section("Table 26: Survey-Weighted Main RD Estimates")

week4_specs <- tribble(
  ~var, ~label,
  "cesd", "CES-D",
  "depyr", "Depressed in past year",
  "mental_index", "Mental Health Index",
  "oop_real", "OOP spending"
)

t26 <- bind_rows(
  rd_reduced_form_weighted(
    df_bw,
    "medicare",
    "Medicare coverage",
    estimand = "Weighted first stage"
  ),
  make_weighted_rd_table(
    df_bw,
    week4_specs,
    "Survey-weighted main RD sample",
    include_fuzzy = TRUE
  ) %>%
    select(-sample)
)

write_table(t26, "table_26_survey_weighted_main_rd.csv")

section("Table 27: Main RD Estimates Excluding Medicaid Recipients")

df_no_medicaid <- df_bw %>%
  filter(!is.na(medicaid), medicaid != 1)

t27_counts <- tibble(
  sample = c("Main RD sample", "Excluding Medicaid recipients"),
  persons = c(n_distinct(df_bw$hhidpn), n_distinct(df_no_medicaid$hhidpn)),
  person_waves = c(nrow(df_bw), nrow(df_no_medicaid))
)

t27 <- make_rd_table(
  df_no_medicaid,
  week4_specs,
  "Excluding Medicaid recipients",
  include_fuzzy = TRUE
)

write_table(t27_counts, "table_27a_no_medicaid_sample_counts.csv")
write_table(t27, "table_27_no_medicaid_rd.csv")

section("Table 28: Main RD Estimates Excluding Lower-Income Respondents")

income_cutoff_q25 <- quantile(
  df_bw$total_income_real,
  probs = 0.25,
  na.rm = TRUE
)

df_above_q25_income <- df_bw %>%
  filter(
    !is.na(total_income_real),
    total_income_real > income_cutoff_q25
  )

t28_counts <- tibble(
  sample = c("Main RD sample", "Above 25th percentile income"),
  income_cutoff_q25 = income_cutoff_q25,
  persons = c(n_distinct(df_bw$hhidpn), n_distinct(df_above_q25_income$hhidpn)),
  person_waves = c(nrow(df_bw), nrow(df_above_q25_income))
)

t28 <- make_rd_table(
  df_above_q25_income,
  week4_specs,
  "Above 25th percentile income",
  include_fuzzy = TRUE
)

write_table(t28_counts, "table_28a_above_q25_income_sample_counts.csv")
write_table(t28, "table_28_above_q25_income_rd.csv")

section("Table 29: Main RD Estimates Excluding Medicaid and Lower-Income Respondents")

df_no_medicaid_above_q25 <- df_bw %>%
  filter(
    !is.na(medicaid),
    medicaid != 1,
    !is.na(total_income_real),
    total_income_real > income_cutoff_q25
  )

t29_counts <- tibble(
  sample = c("Main RD sample", "No Medicaid + above 25th percentile income"),
  income_cutoff_q25 = income_cutoff_q25,
  persons = c(n_distinct(df_bw$hhidpn), n_distinct(df_no_medicaid_above_q25$hhidpn)),
  person_waves = c(nrow(df_bw), nrow(df_no_medicaid_above_q25))
)

t29 <- make_rd_table(
  df_no_medicaid_above_q25,
  week4_specs,
  "No Medicaid + above 25th percentile income",
  include_fuzzy = TRUE
)

write_table(t29_counts, "table_29a_no_medicaid_above_q25_sample_counts.csv")
write_table(t29, "table_29_no_medicaid_above_q25_rd.csv")

section("Table 30: Compact Week 4 Comparison")

extract_week4_core <- function(tab, spec_name) {
  tab %>%
    filter(
      outcome %in% c("CES-D", "Mental Health Index", "OOP spending"),
      estimand %in% c(
        "Reduced form",
        "Fuzzy RD LATE",
        "Weighted reduced form",
        "Weighted fuzzy RD LATE"
      )
    ) %>%
    mutate(specification = spec_name, .before = estimand) %>%
    select(specification, estimand, outcome, estimate, se, p_value, ci_lower, ci_upper, n, stars)
}

t30 <- bind_rows(
  extract_week4_core(t02, "Unweighted main RD"),
  extract_week4_core(t26, "Survey-weighted main RD"),
  extract_week4_core(t27, "Excluding Medicaid recipients"),
  extract_week4_core(t28, "Above 25th percentile income"),
  extract_week4_core(t29, "No Medicaid + above 25th percentile income")
)

write_table(t30, "table_30_week4_comparison.csv")

section("Figure 36: Week 4 Robustness Comparison")

t30_plot <- t30 %>%
  filter(outcome %in% c("CES-D", "Mental Health Index")) %>%
  mutate(
    outcome = factor(outcome, levels = c("CES-D", "Mental Health Index")),
    specification = factor(
      specification,
      levels = c(
        "Unweighted main RD",
        "Survey-weighted main RD",
        "Excluding Medicaid recipients",
        "Above 25th percentile income",
        "No Medicaid + above 25th percentile income"
      )
    ),
    estimand_clean = case_when(
      str_detect(estimand, "fuzzy|Fuzzy") ~ "Fuzzy RD LATE",
      TRUE ~ "Reduced form"
    )
  )

p_week4 <- ggplot(
  t30_plot,
  aes(x = estimate, y = specification, color = outcome)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = CLR_GREY) +
  geom_linerange(
    aes(xmin = ci_lower, xmax = ci_upper),
    position = position_dodge(width = 0.55),
    linewidth = 0.9,
    alpha = 0.75
  ) +
  geom_point(
    position = position_dodge(width = 0.55),
    size = 3
  ) +
  facet_wrap(~ estimand_clean, scales = "free_x") +
  scale_color_manual(
    values = c("CES-D" = CLR_ACCENT, "Mental Health Index" = CLR_MAIN),
    name = "Outcome"
  ) +
  labs(
    title = "**Figure 36.** Week 4 Robustness Checks",
    subtitle = "Survey weighting, Medicaid exclusion, and income-restriction checks",
    x = "Estimated effect",
    y = NULL,
    caption = "Notes: Points are estimates; bars show 95% confidence intervals. Medicaid and income checks preserve the original RD structure."
  ) +
  theme_pub()

save_figure(
  p_week4,
  "figure_36_week4_robustness_comparison.png",
  width = 11,
  height = 6.5
)

week4_notes <- c(
  "Survey weights and Medicaid robustness checks",
  "",
  "This section keeps the original fuzzy RD design and reruns the preferred main specification under three additional checks.",
  "",
  "1. Survey weights:",
  "   Table 26 applies the general HRS respondent weight variable respondent_weight to the main RD specification.",
  "   The Leave-Behind weight is not used as the main survey-weight robustness check because it applies only to the psychosocial leave-behind subsample.",
  "",
  "2. Medicaid exclusion:",
  "   Table 27 removes respondents observed with Medicaid coverage.",
  "   If the Medicare effect remains, this supports the claim that the main result is not driven by Medicaid.",
  "",
  "3. Income restriction:",
  paste0(
    "   Table 28 removes respondents at or below the 25th percentile of household income. ",
    "The cutoff is ",
    round(income_cutoff_q25, 2),
    " in 2016 dollars."
  ),
  "",
  "4. Combined restriction:",
  "   Table 29 removes both Medicaid recipients and respondents at or below the 25th percentile of household income.",
  "   This is the strictest check for whether the result is really Medicare-driven rather than Medicaid- or low-income-composition-driven.",
  "",
  "Interpretation note:",
  "The main conclusions are stronger if the estimates remain similar in sign and magnitude across Tables 26-29, even when standard errors increase because the samples are smaller."
)

writeLines(
  week4_notes,
  con = file.path(TABLE_DIR, "week4_interpretation_notes.txt")
)

# ------------------------------------------------------------------------------
# 5. Figures
# ------------------------------------------------------------------------------

section("Publication Figures")

make_rd_plot <- function(data, y, rd_row, title, y_label, file_name,
                         percent_y = FALSE, dollar_y = FALSE) {
  plot_data <- rd_bins(data, y)
  p <- ggplot(plot_data, aes(bin, y_mean)) +
    rd_vline +
    geom_errorbar(
      aes(ymin = y_mean - 1.96 * y_se, ymax = y_mean + 1.96 * y_se, color = side),
      width = 1,
      linewidth = 0.5,
      alpha = 0.6
    ) +
    geom_point(aes(color = side, fill = side), shape = 21, size = 2.7, stroke = 0.6) +
    geom_smooth(
      data = data %>% drop_na(all_of(c(y, "running"))) %>% filter(running < 0),
      aes(x = running, y = .data[[y]]),
      method = "lm",
      se = TRUE,
      color = CLR_UNDER65,
      fill = CLR_UNDER65,
      alpha = 0.10,
      linewidth = 1
    ) +
    geom_smooth(
      data = data %>% drop_na(all_of(c(y, "running"))) %>% filter(running >= 0),
      aes(x = running, y = .data[[y]]),
      method = "lm",
      se = TRUE,
      color = CLR_OVER65,
      fill = CLR_OVER65,
      alpha = 0.10,
      linewidth = 1
    ) +
    scale_color_manual(values = c(left = CLR_UNDER65, right = CLR_OVER65),
                       labels = c("Under 65", "Age 65+"), name = NULL) +
    scale_fill_manual(values = c(left = CLR_UNDER65, right = CLR_OVER65),
                      labels = c("Under 65", "Age 65+"), name = NULL) +
    rd_xscale +
    labs(
      title = title,
      subtitle = glue("RD estimate = {round(rd_row$estimate[1], 3)}; clustered SE = {round(rd_row$se[1], 3)}; p = {round(rd_row$p_value[1], 3)}"),
      x = "Age relative to 65 (months)",
      y = y_label,
      caption = "Notes: Three-month bins. Lines are linear fits on each side of age 65."
    ) +
    theme_pub()
  
  if (percent_y) p <- p + scale_y_continuous(labels = percent_format(accuracy = 1))
  if (dollar_y) p <- p + scale_y_continuous(labels = dollar_format(big.mark = ","))
  save_figure(p, file_name, 9, 6)
}

make_rd_plot(df_bw, "medicare", t02 %>% filter(estimand == "First stage"),
             "**Figure 1.** Medicare Coverage Around Age 65",
             "Share with Medicare coverage", "figure_01_first_stage_medicare.png", percent_y = TRUE)

make_rd_plot(df_bw, "cesd", t02 %>% filter(outcome == "CES-D", estimand == "Reduced form"),
             "**Figure 2.** CES-D Around Age 65",
             "CES-D depressive symptoms", "figure_02_cesd_reduced_form.png")

make_rd_plot(df_bw, "depyr", t02 %>% filter(outcome == "Depressed in past year", estimand == "Reduced form"),
             "**Figure 3.** Past-Year Depression Around Age 65",
             "Share depressed in past year", "figure_03_depression_indicator.png", percent_y = TRUE)

make_rd_plot(df_bw, "mental_index", t02 %>% filter(outcome == "Mental Health Index", estimand == "Reduced form"),
             "**Figure 4.** Mental Health Index Around Age 65",
             "Mental Health Index", "figure_04_mental_health_index.png")

make_rd_plot(df_bw, "oop_real", t02 %>% filter(outcome == "OOP spending", estimand == "Reduced form"),
             "**Figure 5.** Out-of-Pocket Medical Spending Around Age 65",
             "OOP spending (2016 USD)", "figure_05_oop_spending.png", dollar_y = TRUE)

p_density <- df %>%
  filter(!is.na(running), abs(running) <= MAIN_BW) %>%
  ggplot(aes(running)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 3, boundary = 0,
                 fill = CLR_MAIN, color = "white", alpha = 0.65) +
  geom_density(color = CLR_ACCENT, linewidth = 1, adjust = 1.3) +
  rd_vline +
  annotate("label", x = 35, y = Inf, vjust = 1.3,
           label = glue("rddensity p = {round(t08$p_value[1], 3)}"),
           fill = "white", color = "#333333", size = 3.8) +
  rd_xscale +
  labs(
    title = "**Figure 6.** Distribution of the Running Variable",
    subtitle = "Density test for sorting at the age-65 threshold",
    x = "Age relative to 65 (months)",
    y = "Density",
    caption = "Notes: Three-month bins; red curve is a kernel density estimate."
  ) +
  theme_pub()
save_figure(p_density, "figure_06_running_variable_density.png", 9, 5.5)

p_placebo <- t05 %>%
  mutate(type = if_else(true_cutoff, "True cutoff", "Placebo cutoff")) %>%
  ggplot(aes(cutoff_age, estimate, color = type, shape = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = CLR_GREY) +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), linewidth = 0.8, alpha = 0.75) +
  geom_point(size = 4) +
  scale_color_manual(values = c("True cutoff" = CLR_ACCENT, "Placebo cutoff" = CLR_MAIN)) +
  scale_shape_manual(values = c("True cutoff" = 18, "Placebo cutoff" = 16)) +
  scale_x_continuous(breaks = 61:69) +
  labs(
    title = "**Figure 7.** Placebo Cutoff Tests",
    subtitle = "CES-D reduced-form estimates at false and true age cutoffs",
    x = "Cutoff age",
    y = "Estimated effect on CES-D",
    color = NULL,
    shape = NULL,
    caption = "Notes: Placebo tests use the wider loaded age range to avoid truncated windows."
  ) +
  theme_pub()
save_figure(p_placebo, "figure_07_placebo_cutoffs.png", 9, 5.5)

forest_plot(
  t02 %>% filter(estimand == "Reduced form"),
  "**Figure 8.** Main Reduced-Form RD Estimates",
  "Estimated age-65 discontinuities for primary outcomes",
  "RD estimate",
  "figure_08_main_reduced_form_forest.png"
)

forest_plot(
  t02 %>% filter(estimand == "Fuzzy RD LATE"),
  "**Figure 9.** Main Fuzzy RD LATE Estimates",
  "Estimated effect of Medicare coverage at age 65",
  "Fuzzy RD LATE",
  "figure_09_main_fuzzy_rd_forest.png"
)

forest_plot(
  t03,
  "**Figure 10.** Financial Protection Mechanisms",
  "Reduced-form estimates for financial and insurance outcomes",
  "RD estimate",
  "figure_10_financial_mechanisms.png",
  label_col = "mechanism",
  width = 10,
  height = 6.5
)

forest_plot(
  t10,
  "**Figure 11.** Labor, Prescription-Drug, and Disability Channels",
  "Reduced-form estimates for mechanism outcomes",
  "RD estimate",
  "figure_11_labor_rx_disability_channels.png",
  label_col = "mechanism",
  width = 10,
  height = 7
)

forest_plot(
  t11,
  "**Figure 12.** Labor-Status Stability Robustness",
  "Descriptive RD estimates for stable labor-force subsamples",
  "RD estimate on CES-D",
  "figure_12_labor_status_stability.png",
  width = 9,
  height = 4.8
)

# Figure 13: Bandwidth sensitivity.
p_bandwidth <- t06 %>%
  filter(!is.na(estimate)) %>%
  ggplot(aes(bandwidth, estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = CLR_GREY) +
  geom_vline(xintercept = MAIN_BW, linetype = "dotted", color = CLR_ACCENT, linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = CLR_MAIN, alpha = 0.15) +
  geom_line(color = CLR_MAIN, linewidth = 1.1) +
  geom_point(color = CLR_MAIN, fill = "white", shape = 21, size = 3.2, stroke = 1.1) +
  scale_x_continuous(breaks = bw_grid) +
  labs(
    title = "**Figure 13.** Bandwidth Sensitivity of the CES-D Estimate",
    subtitle = "Reduced-form CES-D estimates across bandwidth choices",
    x = "Bandwidth (months)",
    y = "RD estimate on CES-D",
    caption = "Notes: Triangular kernel with respondent-clustered inference. Shading shows 95% confidence intervals."
  ) +
  theme_pub()
save_figure(p_bandwidth, "figure_13_bandwidth_sensitivity.png", 9, 5.5)

# Figure 14: Donut-hole robustness.
base_est <- t07$estimate[t07$donut_size == 0][1]
p_donut <- t07 %>%
  filter(!is.na(estimate)) %>%
  ggplot(aes(donut_size, estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = CLR_GREY) +
  geom_hline(yintercept = base_est, linetype = "dotted", color = CLR_ACCENT, linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = CLR_MAIN, alpha = 0.15) +
  geom_line(color = CLR_MAIN, linewidth = 1.1) +
  geom_point(aes(fill = donut_size == 0), shape = 21, size = 3.5, stroke = 1, color = CLR_MAIN) +
  scale_fill_manual(values = c("TRUE" = CLR_ACCENT, "FALSE" = "white"), guide = "none") +
  scale_x_continuous(breaks = donut_sizes) +
  labs(
    title = "**Figure 14.** Donut-Hole Robustness: CES-D",
    subtitle = "Excluding observations immediately around the age-65 cutoff",
    x = "Donut exclusion radius (months)",
    y = "RD estimate on CES-D",
    caption = "Notes: Triangular kernel, 60-month bandwidth, respondent-clustered inference."
  ) +
  theme_pub()
save_figure(p_donut, "figure_14_donut_robustness.png", 9, 5.5)

# Figure 15: Predetermined covariate continuity, 36-month window.
p_cov_36 <- t04 %>%
  filter(!is.na(estimate)) %>%
  mutate(sig = p_value < 0.10, label = factor(label, levels = rev(unique(label)))) %>%
  ggplot(aes(x = estimate, y = label, color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = CLR_GREY) +
  geom_linerange(aes(xmin = ci_lower, xmax = ci_upper), linewidth = 1.0, alpha = 0.75) +
  geom_point(size = 4) +
  scale_color_manual(
    values = c("FALSE" = CLR_MAIN, "TRUE" = CLR_ACCENT),
    labels = c("FALSE" = "p >= 0.10", "TRUE" = "p < 0.10"),
    name = "Significance"
  ) +
  labs(
    title = "**Figure 15.** Predetermined Covariate Continuity",
    subtitle = glue("Pre-treatment balance checks within +/-{CONT_BW} months of age 65"),
    x = "Estimated discontinuity",
    y = NULL,
    caption = "Notes: Local-linear triangular WLS with respondent-clustered standard errors."
  ) +
  theme_pub()
save_figure(p_cov_36, "figure_15_predetermined_covariate_continuity_36mo.png", 9, 5.5)

# Figure 16: Binary predetermined covariate balance.
binary_balance_vars <- c("female", "white", "base_depyr")
binary_balance_vars <- binary_balance_vars[binary_balance_vars %in% names(df_continuity)]

if (length(binary_balance_vars) > 0) {
  p_binary_balance <- df_continuity %>%
    mutate(side = if_else(running < 0, "Under 65", "Age 65+")) %>%
    select(side, all_of(binary_balance_vars)) %>%
    pivot_longer(-side, names_to = "covariate", values_to = "value") %>%
    drop_na() %>%
    mutate(
      value = factor(value, levels = c(0, 1), labels = c("No", "Yes")),
      covariate = recode(
        covariate,
        female = "Female",
        white = "White",
        base_depyr = "Baseline\ndepression (W3)"
      )
    ) %>%
    ggplot(aes(x = value, fill = side)) +
    geom_bar(
      aes(y = after_stat(prop), group = side),
      position = position_dodge(0.75),
      width = 0.65,
      alpha = 0.85,
      color = "white"
    ) +
    facet_wrap(~ covariate, ncol = 3) +
    scale_fill_manual(values = c("Under 65" = CLR_UNDER65, "Age 65+" = CLR_OVER65)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "**Figure 16.** Binary Predetermined Covariate Balance",
      subtitle = glue("Sample restricted to +/-{CONT_BW} months of age 65"),
      x = NULL,
      y = "Proportion",
      fill = NULL,
      caption = "Notes: Binary predetermined covariates only."
    ) +
    theme_pub()
  save_figure(p_binary_balance, "figure_16_binary_covariate_balance.png", 10, 5)
}

# Figure 17: Continuous predetermined covariate balance.
continuous_balance_vars <- c("educ", "base_cesd", "base_adl", "base_iadl", "base_mobility")
continuous_balance_vars <- continuous_balance_vars[continuous_balance_vars %in% names(df_continuity)]

if (length(continuous_balance_vars) > 0) {
  p_continuous_balance <- df_continuity %>%
    mutate(side = if_else(running < 0, "Under 65", "Age 65+")) %>%
    select(side, all_of(continuous_balance_vars)) %>%
    pivot_longer(-side, names_to = "covariate", values_to = "value") %>%
    drop_na() %>%
    mutate(
      covariate = recode(
        covariate,
        educ = "Education\n(years)",
        base_cesd = "Baseline CES-D\n(W3)",
        base_adl = "Baseline ADL\n(W3)",
        base_iadl = "Baseline IADL\n(W3)",
        base_mobility = "Baseline mobility\n(W3)"
      )
    ) %>%
    ggplot(aes(x = value, fill = side, color = side)) +
    geom_density(alpha = 0.30, linewidth = 0.8) +
    facet_wrap(~ covariate, scales = "free", ncol = 3) +
    scale_fill_manual(values = c("Under 65" = CLR_UNDER65, "Age 65+" = CLR_OVER65)) +
    scale_color_manual(values = c("Under 65" = CLR_UNDER65, "Age 65+" = CLR_OVER65)) +
    labs(
      title = "**Figure 17.** Continuous Predetermined Covariate Balance",
      subtitle = glue("Kernel density estimates within +/-{CONT_BW} months of age 65"),
      x = "Covariate value",
      y = "Density",
      fill = NULL,
      color = NULL,
      caption = "Notes: Continuous predetermined covariates only."
    ) +
    theme_pub()
  save_figure(p_continuous_balance, "figure_17_continuous_covariate_balance.png", 11, 7)
}

# Figure 18: Heterogeneity in CES-D reduced-form effects.
forest_plot(
  t09 %>% mutate(outcome = group),
  "**Figure 18.** Heterogeneity in CES-D Reduced-Form Effects",
  "Estimated age-65 discontinuity by subgroup",
  "Estimated effect on CES-D",
  "figure_18_heterogeneity_cesd.png",
  width = 9,
  height = 5.5
)

# Figure 19: Bandwidth and optimal-bandwidth sensitivity.
opt_row <- t12 %>%
  filter(!is.na(estimate)) %>%
  transmute(bandwidth, estimate, ci_lower, ci_upper, preferred = FALSE)

if (nrow(opt_row) > 0) {
  p_opt_bw <- t06 %>%
    filter(!is.na(estimate)) %>%
    mutate(preferred = bandwidth == MAIN_BW) %>%
    ggplot(aes(bandwidth, estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = CLR_MAIN, alpha = 0.13) +
    geom_line(color = CLR_MAIN, linewidth = 1) +
    geom_point(aes(fill = preferred), shape = 21, size = 3, stroke = 0.9, color = CLR_MAIN) +
    geom_point(data = opt_row, aes(bandwidth, estimate), shape = 23, size = 4.5,
               fill = "#F39C12", color = "#E67E22", stroke = 1.2) +
    scale_fill_manual(values = c("TRUE" = CLR_ACCENT, "FALSE" = "white"), guide = "none") +
    scale_x_continuous(breaks = bw_grid) +
    labs(
      title = "**Figure 19.** Bandwidth and Optimal-Bandwidth Sensitivity: CES-D",
      subtitle = "Preferred 60-month bandwidth and data-driven optimal bandwidth",
      x = "Bandwidth (months)",
      y = "RD estimate on CES-D",
      caption = "Notes: Diamond marks the data-driven optimal bandwidth."
    ) +
    theme_pub()
  save_figure(p_opt_bw, "figure_19_bandwidth_and_optimal.png", 9, 5.5)
}

# Figure 20: Spouse CES-D by index partner's eligibility.
spill_data <- df_couples_bw %>%
  drop_na(s_cesd, i_eligible65, i_running, i_hhidpn) %>%
  filter(abs(i_running) <= MAIN_BW)

if (nrow(spill_data) > 0) {
  p_spouse <- spill_data %>%
    mutate(group = if_else(i_eligible65 == 1L, "Index age 65+", "Index under 65")) %>%
    ggplot(aes(group, s_cesd, fill = group)) +
    geom_violin(alpha = 0.30, color = NA, trim = TRUE) +
    geom_boxplot(width = 0.18, alpha = 0.85, outlier.shape = NA, color = "#333333") +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = CLR_ACCENT) +
    scale_fill_manual(values = c("Index under 65" = CLR_UNDER65, "Index age 65+" = CLR_OVER65), guide = "none") +
    labs(
      title = "**Figure 20.** Spouse CES-D by Index Partner's Medicare Eligibility",
      subtitle = "Exploratory spousal spillover visualization",
      x = NULL,
      y = "Spouse CES-D score",
      caption = "Notes: Diamond is the group mean. Couple sample preserves the original 60-70 analytic window."
    ) +
    theme_pub()
  save_figure(p_spouse, "figure_20_spousal_spillover.png", 7, 5.5)
}

# Figure 21: Mental-health components among workers >37 hours.
if (nrow(t16) > 0) {
  forest_plot(
    t16 %>% filter(estimand == "Reduced form"),
    "**Figure 21.** Mental Health Components Among Workers >37 Hours",
    "Reduced-form RD estimates for each component separately",
    "RD estimate",
    "figure_21_mhi_components_workers_gt37.png",
    width = 9,
    height = 5.5
  )
}

# Figure 22: Mental-health components among workers >10 hours.
if (nrow(t17) > 0) {
  forest_plot(
    t17 %>% filter(estimand == "Reduced form"),
    "**Figure 22.** Mental Health Components Among Workers >10 Hours",
    "Reduced-form RD estimates for each component separately",
    "RD estimate",
    "figure_22_mhi_components_workers_gt10.png",
    width = 9,
    height = 5.5
  )
}

# Figure 23: Mental Health Index among working samples.
if (nrow(t17a) > 0) {
  p_work_index <- t17a %>%
    filter(!is.na(estimate)) %>%
    mutate(
      sig = p_value < 0.10,
      label = paste(sample, estimand, sep = " | "),
      label = factor(label, levels = rev(unique(label)))
    ) %>%
    ggplot(aes(x = estimate, y = label, color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_linerange(aes(xmin = ci_lower, xmax = ci_upper), linewidth = 1.1, alpha = 0.75) +
    geom_point(size = 4.5) +
    scale_color_manual(values = c("FALSE" = CLR_MAIN, "TRUE" = CLR_ACCENT),
                       labels = c("FALSE" = "p >= 0.10", "TRUE" = "p < 0.10"),
                       name = "Significance") +
    labs(
      title = "**Figure 23.** Mental Health Index Among Working Samples",
      subtitle = "Reduced-form and fuzzy RD estimates for the overall Mental Health Index",
      x = "RD estimate",
      y = NULL,
      caption = "Notes: Working-sample analyses are descriptive because work hours may be post-threshold outcomes."
    ) +
    theme_pub()
  save_figure(p_work_index, "figure_23_mental_index_working_samples.png", 10, 5.5)
}

# Figure 24: Respondent-or-spouse depression outcome.
if (nrow(t18) > 0) {
  p_household <- t18 %>%
    filter(!is.na(estimate)) %>%
    mutate(
      sig = p_value < 0.10,
      estimand = factor(estimand, levels = rev(unique(estimand)))
    ) %>%
    ggplot(aes(x = estimate, y = estimand, color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_linerange(aes(xmin = ci_lower, xmax = ci_upper), linewidth = 1.1, alpha = 0.75) +
    geom_point(size = 4.5) +
    scale_color_manual(values = c("FALSE" = CLR_MAIN, "TRUE" = CLR_ACCENT),
                       labels = c("FALSE" = "p >= 0.10", "TRUE" = "p < 0.10"),
                       name = "Significance") +
    labs(
      title = "**Figure 24.** Respondent-or-Spouse Depression Outcome",
      subtitle = "Couple-sample RD estimates for household depression indicator",
      x = "RD estimate",
      y = NULL,
      caption = "Notes: Outcome equals one if respondent or spouse reports depression in the past year."
    ) +
    theme_pub()
  save_figure(p_household, "figure_24_household_depression_couple_sample.png", 9, 4.8)
}

# Figure 25: Spouse labor-market stability.
if (nrow(t19) > 0) {
  forest_plot(
    t19,
    "**Figure 25.** Spouse Labor-Market Stability",
    "Couple-sample RD estimates under spouse/respondent labor stability restrictions",
    "RD estimate",
    "figure_25_spouse_labor_stability.png",
    width = 9,
    height = 5.2
  )
}

# Figure 26: Labor-market continuity around age 65.
labor_rate_bins <- df_bw %>%
  select(running, working, retired, fulltime_worker) %>%
  pivot_longer(
    cols = c(working, retired, fulltime_worker),
    names_to = "outcome",
    values_to = "value"
  ) %>%
  drop_na(running, value) %>%
  mutate(
    bin = floor(running / 3) * 3 + 1.5,
    outcome = recode(
      outcome,
      working = "Currently working",
      retired = "Retired",
      fulltime_worker = "Working 35-45 hours/week"
    )
  ) %>%
  group_by(bin, outcome) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(panel = "A. Employment and retirement rates")

labor_hours_bins <- df_bw %>%
  drop_na(running, work_hours) %>%
  mutate(bin = floor(running / 3) * 3 + 1.5) %>%
  group_by(bin) %>%
  summarise(value = mean(work_hours, na.rm = TRUE), .groups = "drop") %>%
  mutate(outcome = "Average work hours", panel = "B. Work intensity")

p_labor_continuity <- bind_rows(labor_rate_bins, labor_hours_bins) %>%
  ggplot(aes(x = bin, y = value, color = outcome)) +
  rd_vline +
  geom_point(size = 2.3, alpha = 0.85) +
  geom_line(linewidth = 0.8, alpha = 0.85) +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  rd_xscale +
  labs(
    title = "**Figure 26.** Labor-Market Continuity Around Age 65",
    subtitle = "Employment, retirement, full-time status, and hours worked near the Medicare threshold",
    x = "Age relative to 65 (months)",
    y = NULL,
    color = NULL,
    caption = "Notes: Three-month bins within +/-60 months of age 65."
  ) +
  theme_pub()
save_figure(p_labor_continuity, "figure_26_labor_market_continuity.png", 10, 7)

# Figure 27: Stably nonworking population RD estimates.
if (nrow(t20) > 0) {
  p_nonworking <- t20 %>%
    filter(!is.na(estimate)) %>%
    mutate(
      sig = p_value < 0.10,
      label = paste(outcome, estimand, sep = " | "),
      label = factor(label, levels = rev(unique(label)))
    ) %>%
    ggplot(aes(x = estimate, y = label, color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_linerange(aes(xmin = ci_lower, xmax = ci_upper), linewidth = 1.1, alpha = 0.75) +
    geom_point(size = 4.3) +
    scale_color_manual(values = c("FALSE" = CLR_MAIN, "TRUE" = CLR_ACCENT),
                       labels = c("FALSE" = "p >= 0.10", "TRUE" = "p < 0.10"),
                       name = "Significance") +
    labs(
      title = "**Figure 27.** RD Estimates Among Stably Nonworking Respondents",
      subtitle = "Respondents observed as nonworking on both sides of the threshold",
      x = "RD estimate",
      y = NULL,
      caption = "Notes: Subsample conditions on work histories and is descriptive."
    ) +
    theme_pub()
  save_figure(p_nonworking, "figure_27_nonworking_population_rd.png", 10, 5.8)
}

# Figure 28: Insurance composition among stably nonworking respondents.
if (nrow(t20a) > 0) {
  p_nonworking_ins <- t20a %>%
    filter(variable %in% c(
      "Medicare coverage",
      "Medicaid coverage",
      "Medicare and Medicaid dual coverage",
      "Private insurance through spouse",
      "VA insurance",
      "Uninsured"
    )) %>%
    mutate(
      age_side = factor(age_side, levels = c("Below 65", "Age 65+")),
      variable = factor(variable, levels = rev(c(
        "Medicare coverage",
        "Medicaid coverage",
        "Medicare and Medicaid dual coverage",
        "Private insurance through spouse",
        "VA insurance",
        "Uninsured"
      )))
    ) %>%
    ggplot(aes(x = share_percent, y = variable, fill = age_side)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.9) +
    scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
    scale_fill_manual(values = c("Below 65" = CLR_UNDER65, "Age 65+" = CLR_OVER65)) +
    labs(
      title = "**Figure 28.** Insurance Composition Among Stably Nonworking Respondents",
      subtitle = "Coverage shares before and after age 65",
      x = "Share of observations",
      y = NULL,
      fill = NULL,
      caption = "Notes: Categories can overlap."
    ) +
    theme_pub()
  save_figure(p_nonworking_ins, "figure_28_nonworking_insurance_composition.png", 10, 6)
}

# Figure 29: Primary pre-65 insurance group among stably nonworking respondents.
if (nrow(t20b) > 0) {
  p_pre65_coverage <- t20b %>%
    mutate(
      coverage_group = factor(coverage_group, levels = rev(coverage_group)),
      label = paste0(round(percent, 1), "%")
    ) %>%
    ggplot(aes(x = percent, y = coverage_group)) +
    geom_col(fill = CLR_MAIN, alpha = 0.9, width = 0.65) +
    geom_text(aes(label = label), hjust = -0.1, size = 3.6) +
    scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "**Figure 29.** Primary Pre-65 Coverage in the Stably Nonworking Sample",
      subtitle = "Each respondent is assigned to one primary pre-65 coverage category",
      x = "Share of persons",
      y = NULL,
      caption = "Notes: Coverage group is based on observed pre-65 coverage."
    ) +
    theme_pub()
  save_figure(p_pre65_coverage, "figure_29_nonworking_primary_pre65_coverage.png", 10, 5.8)
}

# Figure 30: Panel IV first stage.
if (nrow(t21a) > 0 && !is.na(t21a$estimate[1])) {
  p_panel_fs <- t21a %>%
    mutate(label = glue("First stage estimate = {round(estimate, 3)}\nF-statistic = {round(F_stat, 1)}")) %>%
    ggplot(aes(x = model, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                  width = 0.15, linewidth = 0.9) +
    geom_point(size = 4.5, color = CLR_MAIN) +
    geom_label(aes(label = label), nudge_y = 0.05, size = 3.7,
               label.padding = unit(0.35, "lines")) +
    labs(
      title = "**Figure 30.** Panel IV First Stage",
      subtitle = "Medicare coverage regressed on age-65 eligibility with respondent and wave fixed effects",
      x = NULL,
      y = "First-stage estimate",
      caption = "Notes: Standard errors are clustered by respondent."
    ) +
    theme_pub() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  save_figure(p_panel_fs, "figure_30_panel_iv_first_stage.png", 8, 5)
}

# Figure 31: Panel IV estimates.
if (nrow(t21) > 0) {
  forest_plot(
    t21,
    "**Figure 31.** Panel IV Estimates with Respondent Fixed Effects",
    "Medicare coverage instrumented with age-65 eligibility",
    "IV estimate",
    "figure_31_panel_iv_individual_fe.png",
    width = 10,
    height = 5.5
  )
}

# Figure 32: Next-wave mental health outcomes.
if (nrow(t22) > 0) {
  p_forward <- t22 %>%
    filter(!is.na(estimate)) %>%
    mutate(
      sig = p_value < 0.10,
      label = paste(outcome, estimand, sep = " | "),
      label = factor(label, levels = rev(unique(label)))
    ) %>%
    ggplot(aes(x = estimate, y = label, color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_linerange(aes(xmin = ci_lower, xmax = ci_upper), linewidth = 1.1, alpha = 0.75) +
    geom_point(size = 4.3) +
    scale_color_manual(values = c("FALSE" = CLR_MAIN, "TRUE" = CLR_ACCENT),
                       labels = c("FALSE" = "p >= 0.10", "TRUE" = "p < 0.10"),
                       name = "Significance") +
    labs(
      title = "**Figure 32.** Next-Wave Mental Health Outcomes",
      subtitle = "Effects estimated on mental health outcomes in the next observed HRS wave",
      x = "RD estimate",
      y = NULL,
      caption = "Notes: HRS waves are biennial, so one lead is approximately two years later."
    ) +
    theme_pub()
  save_figure(p_forward, "figure_32_next_wave_mental_health_outcomes.png", 10, 6)
}

# Figure 33: Prescription-drug and insurance channel details.
if (nrow(t23) > 0) {
  forest_plot(
    t23,
    "**Figure 33.** Prescription-Drug and Insurance Channel Details",
    "Reduced-form RD estimates for drug-use, premium, and insurance outcomes",
    "RD estimate",
    "figure_33_prescription_drug_insurance_channel.png",
    width = 10,
    height = 6
  )
}

# Figure 33A: Premium and drug-use RD estimates.
if (exists("t23d") && nrow(t23d) > 0) {
  forest_plot(
    t23d,
    "**Figure 33A.** Premium and Prescription-Drug Use Outcomes",
    "Reduced-form RD estimates at age 65",
    "RD estimate",
    "figure_33a_premium_drug_use_rd.png",
    width = 10,
    height = 6
  )
}

# Figure 33B: Descriptive premium comparison by age-65 status.
if (exists("t23b") && nrow(t23b) > 0) {
  premium_bar <- t23b %>%
    select(age_group, mean_private_premium, mean_partd_premium, mean_ma_hmo_premium) %>%
    pivot_longer(
      cols = starts_with("mean_"),
      names_to = "premium_type",
      values_to = "mean_premium"
    ) %>%
    mutate(
      premium_type = recode(
        premium_type,
        mean_private_premium = "Private insurance",
        mean_partd_premium = "Medicare Part D",
        mean_ma_hmo_premium = "Medicare Advantage/HMO"
      ),
      premium_type = factor(
        premium_type,
        levels = c("Private insurance", "Medicare Part D", "Medicare Advantage/HMO")
      )
    )
  
  p_premium_bar <- ggplot(premium_bar, aes(x = premium_type, y = mean_premium, fill = age_group)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    scale_y_continuous(labels = dollar_format()) +
    scale_fill_manual(values = c("Under 65" = CLR_UNDER65, "Age 65+" = CLR_OVER65), name = NULL) +
    labs(
      title = "**Figure 33B.** Observed Monthly Premiums by Age-65 Status",
      subtitle = "Private premiums are total private plan premiums; Part D and HMO premiums are Medicare-related premiums",
      x = NULL,
      y = "Mean monthly premium, 2016 USD",
      caption = "Notes: Missing premium amounts are not treated as zero. Private premiums are not prescription-only premiums."
    ) +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  
  save_figure(p_premium_bar, "figure_33b_premium_comparison_by_age65_status.png", 9, 5.5)
}

# Figure 33C: Medicare plan-type indicators.
if (exists("t23e") && nrow(t23e) > 0) {
  p_plan_type <- t23e %>%
    ggplot(aes(x = age_group, y = percent, fill = plan_type_observed)) +
    geom_col(position = "stack", width = 0.65) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title = "**Figure 33C.** Observed Medicare Plan-Type Indicators",
      subtitle = "Among Medicare beneficiaries in the RD bandwidth",
      x = NULL,
      y = "Percent of Medicare person-waves",
      fill = NULL,
      caption = "Notes: HMO premium observed is interpreted as Medicare Advantage/HMO among Medicare beneficiaries."
    ) +
    theme_pub()
  
  save_figure(p_plan_type, "figure_33c_medicare_plan_type_indicators.png", 9, 5.5)
}

# Figure 34: Work-hour-bin heterogeneity.
if (nrow(t24) > 0) {
  p_work_bins <- t24 %>%
    filter(outcome %in% c("CES-D", "Mental Health Index"), !is.na(estimate)) %>%
    mutate(
      sig = p_value < 0.10,
      work_hour_bin = factor(
        work_hour_bin,
        levels = c("10-20 hours", "20-30 hours", "30-40 hours", "40+ hours")
      )
    ) %>%
    ggplot(aes(x = work_hour_bin, y = estimate, color = sig)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), linewidth = 0.9, alpha = 0.75) +
    geom_point(size = 3.6) +
    facet_grid(outcome ~ estimand, scales = "free_y") +
    scale_color_manual(
      values = c("FALSE" = CLR_MAIN, "TRUE" = CLR_ACCENT),
      labels = c("FALSE" = "p >= 0.10", "TRUE" = "p < 0.10"),
      name = "Significance"
    ) +
    labs(
      title = "**Figure 34.** Mental-Health Effects by Work-Hour Bin",
      subtitle = "Reduced-form and fuzzy RD estimates by weekly work hours",
      x = "Weekly work hours",
      y = "Estimated effect",
      caption = "Notes: Higher Mental Health Index values indicate better mental health."
    ) +
    theme_pub()
  save_figure(p_work_bins, "figure_34_work_hour_bin_effects.png", 11, 7)
}

# Figure 35: Income heterogeneity.
if (nrow(t25) > 0) {
  p_income <- t25 %>%
    filter(outcome %in% c("CES-D", "Mental Health Index", "OOP spending"), !is.na(estimate)) %>%
    mutate(
      sig = p_value < 0.10,
      income_group = factor(income_group, levels = c("Below median income", "Above median income"))
    ) %>%
    ggplot(aes(x = income_group, y = estimate, color = sig)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), linewidth = 0.9, alpha = 0.75) +
    geom_point(size = 3.6) +
    facet_grid(outcome ~ estimand, scales = "free_y") +
    scale_color_manual(
      values = c("FALSE" = CLR_MAIN, "TRUE" = CLR_ACCENT),
      labels = c("FALSE" = "p >= 0.10", "TRUE" = "p < 0.10"),
      name = "Significance"
    ) +
    labs(
      title = "**Figure 35.** Effects by Household Income Group",
      subtitle = "Reduced-form and fuzzy RD estimates by below- versus above-median household income",
      x = NULL,
      y = "Estimated effect",
      caption = "Notes: Income groups are defined using median household income in the RD sample."
    ) +
    theme_pub()
  save_figure(p_income, "figure_35_income_heterogeneity.png", 11, 7)
}
# Figure 37: Physical disease burden heterogeneity.
if (nrow(t32) > 0) {
  p_physical_burden <- t32 %>%
    filter(outcome %in% c("CES-D", "Mental Health Index", "OOP spending"), !is.na(estimate)) %>%
    mutate(
      sig = p_value < 0.10,
      physical_burden_group = factor(
        physical_burden_group,
        levels = c("Better Physical Health (0-1 conditions)", "Poorer Physical Health (3+ conditions)")
      )
    ) %>%
    ggplot(aes(x = physical_burden_group, y = estimate, color = sig)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), linewidth = 0.9, alpha = 0.75) +
    geom_point(size = 3.6) +
    facet_grid(outcome ~ estimand, scales = "free_y") +
    scale_color_manual(
      values = c("FALSE" = CLR_MAIN, "TRUE" = CLR_ACCENT),
      labels = c("FALSE" = "p >= 0.10", "TRUE" = "p < 0.10"),
      name = "Significance"
    ) +
    labs(
      title = "**Figure 37.** Medicare Effects by Physical Health Status",
      subtitle = "Physical health status is based on the count of hypertension, diabetes, cancer, lung disease, heart disease, stroke, and arthritis",
      x = NULL,
      y = "Estimated effect",
      caption = "Notes: Better physical health is defined as 0-1 diagnosed conditions; poorer physical health is defined as 3 or more diagnosed conditions. Respondent-waves with exactly 2 conditions are excluded from this split. Psychiatric and sleep conditions are excluded."
    ) +
    theme_pub()
  
  save_figure(p_physical_burden, "figure_37_physical_health_status_heterogeneity.png", 11, 7)
}

# Figure 38: Functional limitation burden heterogeneity.
if (nrow(t33) > 0) {
  p_functional_burden <- t33 %>%
    filter(outcome %in% c("CES-D", "Mental Health Index", "OOP spending"), !is.na(estimate)) %>%
    mutate(
      sig = p_value < 0.10,
      functional_burden_group = factor(
        functional_burden_group,
        levels = c("No Functional Limitations", "Any Functional Limitations")
      )
    ) %>%
    ggplot(aes(x = functional_burden_group, y = estimate, color = sig)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), linewidth = 0.9, alpha = 0.75) +
    geom_point(size = 3.6) +
    facet_grid(outcome ~ estimand, scales = "free_y") +
    scale_color_manual(
      values = c("FALSE" = CLR_MAIN, "TRUE" = CLR_ACCENT),
      labels = c("FALSE" = "p >= 0.10", "TRUE" = "p < 0.10"),
      name = "Significance"
    ) +
    labs(
      title = "**Figure 38.** Medicare Effects by Functional Limitation Burden",
      subtitle = "Functional burden is the sum of ADL and IADL limitations",
      x = NULL,
      y = "Estimated effect",
      caption = "Notes: No functional limitations means ADL + IADL equals 0; any functional limitations means ADL + IADL is at least 1."
    ) +
    theme_pub()
  
  save_figure(p_functional_burden, "figure_38_functional_limitation_burden_heterogeneity.png", 11, 7)
}

# Figure 39: Pre-65 depression heterogeneity.
if (nrow(t35) > 0) {
  p_pre65_dep <- t35 %>%
    filter(outcome %in% c("CES-D", "Mental Health Index", "OOP spending"), !is.na(estimate)) %>%
    mutate(
      sig = p_value < 0.10,
      pre65_depression_group = factor(
        pre65_depression_group,
        levels = c("Not depressed before 65", "Depressed before 65")
      )
    ) %>%
    ggplot(aes(x = pre65_depression_group, y = estimate, color = sig)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), linewidth = 0.9, alpha = 0.75) +
    geom_point(size = 3.6) +
    facet_grid(outcome ~ estimand, scales = "free_y") +
    scale_color_manual(
      values = c("FALSE" = CLR_MAIN, "TRUE" = CLR_ACCENT),
      labels = c("FALSE" = "p >= 0.10", "TRUE" = "p < 0.10"),
      name = "Significance"
    ) +
    labs(
      title = "**Figure 39.** Medicare Effects by Pre-65 Depression Status",
      subtitle = "Groups are defined using observed depression reports before age 65",
      x = NULL,
      y = "Estimated effect",
      caption = "Notes: This is a descriptive subgroup analysis. Higher Mental Health Index values indicate better mental health."
    ) +
    theme_pub()
  
  save_figure(p_pre65_dep, "figure_39_pre65_depression_heterogeneity.png", 11, 7)
}

# Figure 40: Pre-65 CES-D severity heterogeneity.
if (nrow(t36) > 0) {
  p_pre65_cesd <- t36 %>%
    filter(outcome %in% c("CES-D", "Mental Health Index", "OOP spending"), !is.na(estimate)) %>%
    mutate(
      sig = p_value < 0.10,
      pre65_cesd_group = factor(
        pre65_cesd_group,
        levels = c("Low pre-65 CES-D", "High pre-65 CES-D")
      )
    ) %>%
    ggplot(aes(x = pre65_cesd_group, y = estimate, color = sig)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), linewidth = 0.9, alpha = 0.75) +
    geom_point(size = 3.6) +
    facet_grid(outcome ~ estimand, scales = "free_y") +
    scale_color_manual(
      values = c("FALSE" = CLR_MAIN, "TRUE" = CLR_ACCENT),
      labels = c("FALSE" = "p >= 0.10", "TRUE" = "p < 0.10"),
      name = "Significance"
    ) +
    labs(
      title = "**Figure 40.** Medicare Effects by Pre-65 CES-D Severity",
      subtitle = "Groups are split at the median average CES-D score before age 65",
      x = NULL,
      y = "Estimated effect",
      caption = "Notes: This is a descriptive subgroup analysis. High pre-65 CES-D means worse depressive symptoms before age 65."
    ) +
    theme_pub()
  
  save_figure(p_pre65_cesd, "figure_40_pre65_cesd_severity_heterogeneity.png", 11, 7)
}

# Figure 41: Pre-65 Mental Health Index heterogeneity.
if (nrow(t37) > 0) {
  p_pre65_mhi <- t37 %>%
    filter(outcome %in% c("CES-D", "Mental Health Index", "OOP spending"), !is.na(estimate)) %>%
    mutate(
      sig = p_value < 0.10,
      pre65_mhi_group = factor(
        pre65_mhi_group,
        levels = c("High pre-65 Mental Health", "Low pre-65 Mental Health")
      )
    ) %>%
    ggplot(aes(x = pre65_mhi_group, y = estimate, color = sig)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = CLR_GREY) +
    geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), linewidth = 0.9, alpha = 0.75) +
    geom_point(size = 3.6) +
    facet_grid(outcome ~ estimand, scales = "free_y") +
    scale_color_manual(
      values = c("FALSE" = CLR_MAIN, "TRUE" = CLR_ACCENT),
      labels = c("FALSE" = "p >= 0.10", "TRUE" = "p < 0.10"),
      name = "Significance"
    ) +
    labs(
      title = "**Figure 41.** Medicare Effects by Pre-65 Mental Health Index",
      subtitle = "Groups are split at the median pre-65 Mental Health Index",
      x = NULL,
      y = "Estimated effect",
      caption = "Notes: This is a descriptive subgroup analysis. Low pre-65 mental health means a lower average Mental Health Index before age 65."
    ) +
    theme_pub()
  
  save_figure(p_pre65_mhi, "figure_41_pre65_mental_health_index_heterogeneity.png", 11, 7)
}

# ------------------------------------------------------------------------------
# 6. Save Analysis Data
# ------------------------------------------------------------------------------
section("Saving Analysis Data")

saveRDS(df, file.path(DATA_DIR, "df_r9_r14_analysis.rds"))
saveRDS(df_bw, file.path(DATA_DIR, "df_r9_r14_rd_bandwidth_analysis.rds"))
saveRDS(df_couples_full, file.path(DATA_DIR, "df_couples_r9_r14_analysis.rds"))

stata_clean <- function(x) {
  x %>%
    mutate(across(where(is.factor), as.character)) %>%
    rename(
      mcare_pub_prem_nm = medicare_public_premium_nonmissing,
      no_mcare_rx_hmo = no_observed_medicare_drug_or_hmo_premium
    )
}

df_stata <- stata_clean(df)
df_bw_stata <- stata_clean(df_bw)

df_couples_stata <- df_couples_full %>%
  mutate(across(where(is.factor), as.character))

write_dta(df_stata, file.path(DATA_DIR, "df_r9_r14_analysis.dta"))
write_dta(df_bw_stata, file.path(DATA_DIR, "df_r9_r14_rd_bandwidth_analysis.dta"))
write_dta(df_couples_stata, file.path(DATA_DIR, "df_couples_r9_r14_analysis.dta"))

cat(glue("Saved main analysis data: {nrow(df)} rows x {ncol(df)} columns\n"))
cat(glue("Saved results to: {OUTPUT_DIR}\n"))

section("Complete")
