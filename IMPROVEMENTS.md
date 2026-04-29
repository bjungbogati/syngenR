# syngenR Improvement Plan
> Oncology-focused clinical trial raw dataset generation. These datasets represent CRF forms; SDTM derivation is downstream.

---

## 1. Critical Bugs (Fix First)

| # | File | Issue | Fix |
|---|------|--------|-----|
| B1 | `raw_ds.R:29` | Saves to `raw_visit.rda` instead of `raw_ds.rda` — overwrites visit data | Change save path to `data/raw_ds.rda` |
| B2 | `data_conversion.R` | `get_num()` defined twice (lines ~31 and ~131) with different logic; second shadows first | Rename one: `get_num_str()` for string extraction, keep `get_num()` for numeric conversion |
| B3 | `date_time_gen.R:birth_date()` | `sample(1:31)` produces invalid dates (Feb 30, Apr 31, etc.) | Use `seq.Date()` with valid range per month, or `tryCatch(as.Date(...))` loop |
| B4 | `raw_ie.R` | No I/E criteria generated — only a date field | Add criteria rows with `IEORRES` (Y/N) per criterion |
| B5 | `raw_ds.R` | No disposition status (completed/withdrawn/etc.) | Add `dsstatus` column |

---

## 2. Missing Datasets (Add These)

> Meta.csv already defines `cm`, `vs` — scripts just don't exist yet.

### Priority order for oncology:
1. `raw_vs.R` — Vital Signs (ECOG performance status critical for oncology)
2. `raw_cm.R` — Concomitant Medications (supportive care, pre-meds)
3. `raw_mh.R` — Medical History (cancer diagnosis history, prior treatments)
4. `raw_pe.R` — Physical Exam / Tumor Measurements (target/non-target lesions, RECIST)
5. `raw_tu.R` — Tumor Assessments (new — oncology-specific, RECIST 1.1)
6. `raw_pr.R` — Prior Therapies (new — prior chemo/radiation regimens)
7. `raw_su.R` — Substance Use (smoking/alcohol history)

---

## 3. Oncology-Specific Realism Improvements

### 3.1 Demographics (`raw_dm.R`)
- Age range: shift to **45–75** (oncology population skews older)
- Sex probabilities: **0.5/0.5** or study-specific
- Add `DIAGDT` — diagnosis date (6–36 months before `trtstdt`)
- Add `STAGE` — cancer stage (I/II/III/IV; oncology trials mostly III/IV)
- Add `HISTTYPE` — histology type (e.g., adenocarcinoma, squamous cell)
- Add `ECOG` baseline score (0/1/2 probs: 0.4/0.45/0.15)

### 3.2 Adverse Events (`raw_ae.R`)
Current AE list is generic. Replace/expand for oncology:

**Oncology-specific MedDRA terms to add:**
```
NEUTROPENIA (Grade 3/4 common), FEBRILE NEUTROPENIA, ANEMIA, THROMBOCYTOPENIA,
PERIPHERAL NEUROPATHY, ALOPECIA, MUCOSITIS, HAND-FOOT SYNDROME,
NAUSEA, VOMITING, FATIGUE, DECREASED APPETITE, CONSTIPATION, DIARRHEA,
ELEVATED ALT/AST (hepatotoxicity), RASH (immunotherapy-related),
PNEUMONITIS (immunotherapy-related), INFUSION REACTION
```

**Severity realism:**
- Grade 3-4 rate: ~20–30% (increase from current 10%)
- `AESER = Yes` if Grade ≥ 3 (currently only Grade 3; extend to Grade 4)
- `AESDTH = Yes` only if subject is in `raw_dd` (link datasets)
- `AESLIFE = Yes` if Grade 4 or subject in raw_dd
- `AEREL`: chemotherapy → "Related" probability 0.6, immunotherapy → 0.5
- `AEOUT`: tie to `aetoxgr` — Grade 1/2 mostly recovered; Grade 3/4 more sequelae

**Counts per subject:**
- Increase to 1–8 AEs (oncology patients experience more)
- Probs: `c(0.10, 0.20, 0.25, 0.20, 0.12, 0.07, 0.04, 0.02)`

### 3.3 Lab Values — Hematology (`raw_lb1.R`)
Oncology patients have suppressed values (post-chemotherapy nadir). Apply visit-dependent shifts:

| Analyte | Baseline (Screening) | Nadir (Week 4) | Recovery |
|---------|---------------------|----------------|----------|
| HGB | mean=11.7, sd=1.4 | mean=9.5, sd=1.8 | mean=10.5, sd=1.5 |
| NEUT | mean=4.85, sd=3.18 | mean=1.2, sd=0.8 | mean=2.8, sd=1.4 |
| PLAT | mean=188, sd=89 | mean=95, sd=45 | mean=140, sd=60 |
| WBC | mean=6.77, sd=3.57 | mean=2.8, sd=1.5 | mean=4.2, sd=2.0 |

**Implementation:** Pass `visit_factor` argument to `float_bell_gen()` or use visit-specific lookup rows in `hema_lut`.

### 3.4 Lab Values — Chemistry (`raw_lb2.R`)
Add hepatotoxicity signal (ALT/AST elevation in ~15% of subjects):
- `ALT`: ~15% chance of 2–5× ULN elevation post-treatment
- `BILI`: ~5% chance of elevation (drug-induced liver injury pattern)
- Add `LDH` (Lactate Dehydrogenase) — tumor marker, important for lymphoma/solid tumors

### 3.5 Vital Signs (`raw_vs.R` — new)
Key oncology vitals:
- `WEIGHT`: 55–90 kg (decreases ~2–5% over treatment)
- `HEIGHT`: 155–185 cm (constant)
- `BSA` (body surface area): calculated from weight/height
- `SYSBP`/`DIABP`: 110–140 / 70–90
- `TEMP`: 36.0–38.5°C (flag > 38.0 as potential febrile neutropenia)
- `PULSE`: 60–100 bpm
- `ECOG`: 0/1/2 per visit (can worsen over time)
- Collect at: Screening, Day 1 (pre-dose), each cycle visit, EOS

### 3.6 Tumor Assessment (`raw_tu.R` — new, oncology critical)
RECIST 1.1 measurements:
```r
# Columns per lesion
TULOC    # lesion location (LIVER, LUNG, LYMPH NODE, etc.)
TUMETHOD # imaging method (CT, MRI, PET-CT)
TUORRES  # diameter in mm (baseline 20–80mm)
TUORRES  # follow-up: 60–120% of prior (progression/stable/response)
TUSTAT   # Target / Non-Target / New
TUEVAL   # CR/PR/SD/PD per visit (RECIST)
```

### 3.7 Prior Therapies (`raw_pr.R` — new)
```r
PRTRT    # Prior treatment name (CARBOPLATIN, PACLITAXEL, PEMBROLIZUMAB, etc.)
PRINDIC  # Indication (cancer type)
PRSTDTC  # Start date (relative to diagnosis date)
PRENDDTC # End date
PROUT    # Outcome (COMPLETED, PROGRESSIVE DISEASE, TOXICITY)
```

### 3.8 End of Study (`raw_ds.R`)
Add `DSSTATUS` and link to AE deaths:
```r
# Disposition reasons
COMPLETED          # ~60%
ADVERSE EVENT      # ~15%
PROGRESSIVE DISEASE # ~15%
WITHDRAWAL         # ~10%
```

---

## 4. Dataset Generation Framework

### 4.1 Proposed Architecture
```
inst/
  config/
    meta.csv          # study-level config (existing)
    oncology_luts.csv # MedDRA, RECIST, prior therapy lookup tables
    lab_ranges.csv    # visit-adjusted lab reference ranges
  scripts/
    _run_all.R        # orchestrator — runs all scripts in correct order
    config.R          # (existing) study globals
    raw_dm.R          # always first (subject roster)
    raw_visit.R       # second (visit schedule)
    raw_rand.R        # after visit
    raw_ie.R          # after rand
    raw_ds.R          # after all clinical data
    ...
  post_process/
    pp_ae.R           # post-process AE (grade corrections, cross-domain consistency)
    pp_lb.R           # post-process labs (visit trend injection)
    pp_tu.R           # post-process tumor (RECIST derivation)
```

### 4.2 `_run_all.R` Orchestrator (implement this)
```r
# inst/scripts/_run_all.R
scripts <- c(
  "config.R",
  "raw_dm.R",
  "raw_visit.R",
  "raw_rand.R",
  "raw_ie.R",
  "raw_ex.R",
  "raw_ae.R",
  "raw_lb1.R",
  "raw_lb2.R",
  "raw_vs.R",
  "raw_cm.R",
  "raw_mh.R",
  "raw_tu.R",
  "raw_dd.R",   # needs raw_ae (death linkage)
  "raw_ds.R"    # always last
)
base <- system.file("scripts", package = "syngenR")
for (s in scripts) source(file.path(base, s))
```

### 4.3 Condition-Based Configuration
Replace hard-coded values with condition-aware config:

```r
# meta.csv — add a "condition" column
# condition: oncology_solid_tumor | oncology_lymphoma | oncology_lung | general

# config.R pattern
condition <- get_meta(meta, "study", "condition")  # e.g., "oncology_solid_tumor"
ae_profile   <- get_meta(meta, "ae_profile", condition)
lab_profile  <- get_meta(meta, "lab_profile", condition)
```

**Meta.csv additions needed:**
```
condition, study, oncology_solid_tumor
ae_grade34_rate, ae_profile, 0.25
febrile_neutropenia_rate, ae_profile, 0.10
tumor_type, study, NSCLC
prior_lines_range, study, 1-3
```

### 4.4 Helper Function: `get_meta()`
Currently config.R manually filters meta. Add a reusable function:
```r
# R/meta_utils.R
get_meta <- function(meta, type, name, col = "value") {
  meta |> dplyr::filter(type == !!type, name == !!name) |> dplyr::pull(!!col)
}

get_meta_vec <- function(meta, type, name) {
  meta |> dplyr::filter(type == !!type, name == !!name) |>
    dplyr::select(value, subvalue)
}
```

---

## 5. Cross-Domain Consistency Rules

These rules should be applied as post-processing after all raw datasets are generated:

| Rule | Datasets | Logic |
|------|----------|-------|
| Death consistency | raw_dd, raw_ae, raw_ds | If subject in raw_dd: AE with AESDTH=Y must exist; DSSTATUS="DEATH" |
| Febrile neutropenia | raw_ae, raw_lb1 | If AE=FEBRILE NEUTROPENIA: NEUT at that visit < 1.0 |
| Hepatotoxicity | raw_ae, raw_lb2 | If AE=ELEVATED ALT/AST: ALT/AST at that visit > 3×ULN |
| ECOG progression | raw_vs, raw_ds | If DSSTATUS=PROGRESSIVE DISEASE: last ECOG ≥ 2 |
| Prior therapy count | raw_pr, raw_dm | prior_lines matches study eligibility (1–3 lines) |
| Randomization linkage | raw_rand, raw_ex | exvolamt should differ by arm if doses differ |
| Withdrawal date | raw_ds, raw_visit | Last valid visdt ≤ dsendt |

**Implementation:** `inst/post_process/pp_consistency.R` — run after all raw scripts.

---

## 6. Function Improvements

### `gen_site_subj()` — add seed parameter
```r
gen_site_subj <- function(n_subj, n_site, seed = 123) {
  set.seed(seed)
  ...
}
```

### `float_bell_gen()` — add visit shift
```r
float_bell_gen <- function(n, mean, sd, digits = 1, shift = 0) {
  round(abs(rnorm(n, mean + shift, sd)), digits)
}
```

### `std_gen()` — decouple special cases
Currently embeds ethnicity + CBP logic inline. Move to separate functions:
```r
gen_ethnicity(race_values, meta)
gen_cbp(sex_values, age_values, meta)
```

### `lb_low()` / `lb_high()` — use ±2 SD (clinical standard)
Current: `mean ± 1 SD` (flags ~32% of normal values as abnormal).
Fix: `mean ± 2 SD` (~5% flagged — more realistic).

### `birth_date()` — fix invalid dates
```r
birth_date <- function(age, ref_date = Sys.Date(), size = length(age)) {
  years <- birth_year(age, ref_date, size)
  purrr::map_chr(years, ~ format(
    as.Date(paste(.x, sample(1:12,1), sample(1:28,1), sep="-")), "%Y-%m-%d"
  ))
}
```

### `gen_visit_days()` — add window variability
Clinical visits have ±3-day windows. Add jitter:
```r
gen_visit_days <- function(subject, visit_fmt, sdate, window_days = 3) {
  # ... existing logic ...
  visdt + sample(-window_days:window_days, 1)  # add per-visit jitter
}
```

---

## 7. meta.csv Additions Needed

```csv
type,name,value,subvalue
# Oncology profile
condition,study,oncology_solid_tumor,
tumor_type,study,NSCLC,
ae_grade34_rate,ae_profile,0.25,
febrile_neutropenia_rate,ae_profile,0.10,
prior_lines_min,eligibility,1,
prior_lines_max,eligibility,3,
# Cancer stage
format,stage,Stage I,0.05
format,stage,Stage II,0.10
format,stage,Stage III,0.30
format,stage,Stage IV,0.55
# ECOG
format,ecog,0,0.40
format,ecog,1,0.45
format,ecog,2,0.15
# Disposition
format,ds_status,COMPLETED,0.60
format,ds_status,ADVERSE EVENT,0.15
format,ds_status,PROGRESSIVE DISEASE,0.15
format,ds_status,WITHDRAWAL BY SUBJECT,0.10
# Death causes (expand)
format,dd_cause,PROGRESSIVE DISEASE,0.70
format,dd_cause,ADVERSE EVENT,0.20
format,dd_cause,OTHER,0.10
# Visit windows (days)
visit_window,SCN,3,
visit_window,D1,0,
visit_window,W4,3,
visit_window,M6,5,
# New visits for oncology
format,visit,Cycle 1 Day 1,C1D1
format,visit,Cycle 2 Day 1,C2D1
format,visit,Cycle 3 Day 1,C3D1
format,visit,Cycle 4 Day 1,C4D1
format,visit,End of Treatment,EOT
format,visit,30-Day Follow-up,FU30
format,visit,Survival Follow-up 1,SFU1
```

---

## 8. Implementation Priority

```
Phase 1 — Bugs & Consistency (1–2 days)
  B1: Fix raw_ds.R save path
  B2: Rename duplicate get_num()
  B3: Fix birth_date() invalid dates
  Add _run_all.R orchestrator
  Add get_meta() helper

Phase 2 — Missing Core Datasets (2–3 days)
  raw_vs.R (vital signs + ECOG)
  raw_cm.R (concomitant meds)
  raw_mh.R (medical history)
  Complete raw_ie.R (I/E criteria)
  Fix raw_ds.R (add DSSTATUS)

Phase 3 — Oncology Realism (2–3 days)
  Expand AE list + adjust probabilities
  Visit-adjusted lab ranges (hema/chem)
  raw_tu.R (tumor measurements)
  raw_pr.R (prior therapies)
  Update meta.csv (stage, ECOG, disposition)

Phase 4 — Post-Processing & Framework (2–3 days)
  pp_consistency.R (cross-domain rules)
  Condition-based config system
  lb_low/lb_high: switch to ±2 SD
  gen_visit_days() window jitter
  Visit-shift in float_bell_gen()
```

---

## 9. Dataset Dependency Map

```
raw_dm      ← always first (subject roster)
raw_visit   ← depends on: raw_dm
raw_rand    ← depends on: raw_dm, raw_visit
raw_ie      ← depends on: raw_dm, raw_visit
raw_ex      ← depends on: raw_dm, raw_visit (vis), raw_rand
raw_ae      ← depends on: raw_dm, raw_visit (vis)
raw_lb1     ← depends on: raw_dm, raw_visit (lb_visit dates)
raw_lb2     ← depends on: raw_dm, raw_visit (lb_visit dates)
raw_vs      ← depends on: raw_dm, raw_visit
raw_cm      ← depends on: raw_dm, raw_visit
raw_mh      ← depends on: raw_dm
raw_tu      ← depends on: raw_dm, raw_visit
raw_pr      ← depends on: raw_dm
raw_dd      ← depends on: raw_dm, raw_ae (death AEs)
raw_ds      ← depends on: raw_dm, raw_visit, raw_dd (last)
```
