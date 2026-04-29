# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**syngenR** is an R package for generating synthetic raw clinical trial datasets (CDISC-compliant). It produces realistic test data for SDTM programming workflows using probability-weighted sampling, normal distributions, and metadata-driven configuration.

## Common Commands

```r
# Load package during development
devtools::load_all()

# Regenerate documentation (NAMESPACE + man/*.Rd)
devtools::document()

# Build and install
devtools::install()

# Check package
devtools::check()

# Install from GitHub
devtools::install_github("bjungbogati/syngenR")
```

No formal test suite exists — `inst/scripts/` serves as functional examples.

## Architecture

### Data generation flow

1. **`inst/extdata/meta.csv`** — central metadata: study configuration, site/subject counts, visit schedules, domain-specific enumerations (sex, race, AE terms, lab tests, etc.)
2. **`inst/scripts/config.R`** — sourced first by all generation scripts; loads `meta` data object and sets global config variables
3. **`inst/scripts/raw_*.R`** — one script per clinical domain (DM, AE, LB, EX, DS, IE, RAND, VISIT); each generates and saves a `data/raw_*.rda` dataset

### R source files (`R/`)

| File | Role |
|------|------|
| `random_sampling.R` | Core sampling: `prob_gen()` (probability-weighted), `bell_gen()`/`float_bell_gen()` (normal distributions), `gen_site_subj()` (subject IDs) |
| `date_time_gen.R` | Date/datetime generation: `gen_date_range()`, `gen_dt()`, `gen_after_dt()`, `gen_day_after_dt()` |
| `data_conversion.R` | Type coercion utilities: `get_char()`, `get_num()`, `date_to_char()`, `env_to_df()` |
| `var_transform.R` | Variable derivation: `high_low()` (categorization), `gen_visit_days()`, `gen_folder()` |
| `stats.R` | Summary stats and lab reference ranges: `get_stats()`, `lb_high()`, `lb_low()` |

### Key patterns

- **Metadata-driven**: all enumeration values (formats, codes, labels) live in `meta.csv` — query with `dplyr::filter(meta, type == "...", name == "...")`
- **`std_gen(code, decode)`**: standardizes codes to decoded labels across all categorical variables
- **`env_to_df()`**: converts an R environment of vectors (different lengths) to a dataframe — used at the end of each domain script to assemble the final dataset
- **`gen_site_subj(n_subj, n_site)`**: generates unique `SUBJID`/`SITEID` combinations; output drives all downstream joins
- All scripts use the native pipe `|>` and `dplyr` verbs

## Dependencies

Core: `dplyr`, `tidyr`, `lubridate`, `stats`  
Documentation: `roxygen2` (v7.3.3)
