# ── AE generation ─────────────────────────────────────────────────────────────
rm(list = ls())
source("config.R")

# ── MedDRA lookup table ───────────────────────────────────────────────────────
meddra_lut <- tribble(
  ~aeterm,           ~aedecod,          ~aellt,                          ~aehlt,                                      ~aehlgt,                                    ~aesoc,                                        ~llt_code, ~pt_code, ~hlt_code, ~hlgt_code, ~soc_code,
  "NAUSEA",          "Nausea",          "Nausea",                        "Nausea and vomiting symptoms",              "Gastrointestinal signs and symptoms",       "Gastrointestinal disorders",                  10028813,  10028813, 10028817,  10017999,   10017947,
  "FATIGUE",         "Fatigue",         "Fatigue",                       "Asthenic conditions",                       "Neurological signs and symptoms NEC",       "General disorders and administration site",   10016256,  10016256, 10003236,  10029205,   10018065,
  "VOMITING",        "Vomiting",        "Vomiting",                      "Nausea and vomiting symptoms",              "Gastrointestinal signs and symptoms",       "Gastrointestinal disorders",                  10047700,  10047700, 10028817,  10017999,   10017947,
  "DIARRHEA",        "Diarrhoea",       "Diarrhea",                      "Diarrhoea (excl infective)",                "Gastrointestinal signs and symptoms",       "Gastrointestinal disorders",                  10012735,  10012735, 10012736,  10017999,   10017947,
  "CONSTIPATION",    "Constipation",    "Constipation",                  "Constipation and ileus",                    "Gastrointestinal signs and symptoms",       "Gastrointestinal disorders",                  10010774,  10010774, 10010785,  10017999,   10017947,
  "DEHYDRATION",     "Dehydration",     "Dehydration",                   "Fluid and electrolyte disorders NEC",       "Fluid and electrolyte disorders",           "Metabolism and nutrition disorders",           10012174,  10012174, 10016783,  10016780,   10027433,
  "DIZZINESS",       "Dizziness",       "Dizziness",                     "Vestibular disorders NEC",                  "Ear and labyrinthine disorders NEC",        "Nervous system disorders",                    10013573,  10013573, 10047355,  10014138,   10029205,
  "HYPOTENSION",     "Hypotension",     "Hypotension",                   "Vascular hypotensive disorders",            "Vascular disorders NEC",                   "Vascular disorders",                          10021097,  10021097, 10021105,  10047065,   10047065,
  "PAIN (ABDOMEN)",  "Abdominal pain",  "Abdominal pain",                "Gastrointestinal signs and symptoms NEC",   "Gastrointestinal signs and symptoms",       "Gastrointestinal disorders",                  10000081,  10000081, 10000082,  10017999,   10017947,
  "NEUTROPENIA",     "Neutropenia",     "Neutropenia",                   "Decreased white blood cell disorders",      "White blood cell disorders",               "Blood and lymphatic system disorders",         10029354,  10029354, 10011878,  10049066,   10005329,
  "ANEMIA",          "Anaemia",         "Anemia",                        "Anaemias NEC",                              "Anaemias",                                  "Blood and lymphatic system disorders",         10002034,  10002034, 10002036,  10049066,   10005329,
  "THROMBOCYTOPENIA","Thrombocytopenia","Thrombocytopenia",               "Platelet disorders NEC",                    "Platelet disorders",                        "Blood and lymphatic system disorders",         10043554,  10043554, 10043560,  10049066,   10005329,
  "DEATH",           "Death",           "Death",                         "Death and sudden death",                    "Death and sudden death",                    "General disorders and administration site",   10011906,  10011906, 10011907,  10011905,   10018065
)


# ── Formats from metadata ─────────────────────────────────────────────────────
ae_term_fmt <- subset(data, name == "ae_term", value) |> get_char()
yn_fmt      <- subset(data, name == "yes_no",  value) |> get_char()
grade_fmt   <- c("Grade 1 (Mild)", "Grade 2 (Moderate)", "Grade 3 (Severe)")
aerel_fmt   <- c("Possibly Related", "Related", "Unrelated")
aeout_fmt   <- c("Not Recovered/Not Resolved", "Recovered/Resolved",
                 "Recovered/Resolved with Sequelae")
aeacn_fmt   <- c("Dose Not Changed", "Dose Reduced", "Not Applicable")

# ── Helpers ───────────────────────────────────────────────────────────────────

# eq_prob: uniform prob_gen wrapper — prob_gen requires explicit prob arg
eq_prob <- function(fmt, n) {
  prob_gen(fmt, n, prob = rep(1 / length(fmt), length(fmt)))
}

# std_code: consistent factor-to-integer coding
#   - no lvls → natural factor levels (for grade, rel, out, acn)
#   - lvls    → fixed levels from yn_fmt for Yes/No fields
std_code <- function(x, lvls = NULL) {
  if (is.null(lvls)) as.factor(x) |> as.integer()
  else factor(x, levels = lvls) |> as.integer()
}

# ── Subject / AE counts ───────────────────────────────────────────────────────
subj_list <- gen_site_subj(seed)
n_subj    <- nrow(subj_list)

# prob_gen needs character vector as first arg (not numeric 1:4)
ae_counts <- prob_gen(
  as.character(1:4), n_subj,
  prob = c(0.40, 0.30, 0.20, 0.10)
) |> as.integer()

ae_size <- sum(ae_counts)

# ── Date anchors: computed once, reused in gen + process ─────────────────────
# vis$visdt is character — must convert to Date before arithmetic
# Use vis directly — subjects in vis must match subj_list$subject exactly
d1_dates <- vis |>
  filter(folder == "D1", subject %in% subj_list$subject) |>
  mutate(visdt = as.Date(visdt)) |>
  select(subject, visdt)

last_dates <- vis |>
  filter(subject %in% subj_list$subject) |>
  mutate(visdt = as.Date(visdt)) |>
  group_by(subject) |>
  summarise(last_visdt = max(visdt), .groups = "drop")

# Verify alignment
stopifnot(all(subj_list$subject %in% d1_dates$subject))
stopifnot(all(subj_list$subject %in% last_dates$subject))

# ── Core AE generator ─────────────────────────────────────────────────────────
gen_core_ae <- function(subj_expanded) {
  
  size <- nrow(subj_expanded)
  
  # AE term
  aeterm     <- eq_prob(ae_term_fmt, size)
  aeterm_std <- std_code(aeterm)
  
  # Toxicity grade
  aetoxgr     <- prob_gen(grade_fmt, size, prob = c(0.6, 0.3, 0.1))
  aetoxgr_std <- std_code(aetoxgr)
  
  # Serious AE
  aeser     <- ifelse(aetoxgr_std == 3, "Yes", "No")
  aeser_std <- std_code(aeser, yn_fmt)
  
  # Fixed safety flags
  aesdth      <- rep("No", size)
  aesdth_std  <- std_code(aesdth, yn_fmt)
  aeslife     <- rep("No", size)
  aeslife_std <- std_code(aeslife, yn_fmt)
  
  # Relationship / Outcome / Action
  aerel     <- prob_gen(aerel_fmt, size, prob = c(0.3, 0.2, 0.5))
  aerel_std <- std_code(aerel)
  aeout     <- prob_gen(aeout_fmt, size, prob = c(0.4, 0.4, 0.2))
  aeout_std <- std_code(aeout)
  aeacn     <- prob_gen(aeacn_fmt, size, prob = c(0.7, 0.1, 0.2))
  aeacn_std <- std_code(aeacn)
  
  # AE onset date
  aeonset_date <- mapply(function(d1, dlast) {
    d1    <- as.Date(d1)
    dlast <- as.Date(dlast)
    as.character(date_gen(start = d1, end = dlast, size = 1))
  }, subj_expanded$visdt, subj_expanded$last_visdt)
  
  # Start datetime
  aest_range <- gen_date_range(
    start = paste(min(aeonset_date), "00:00:00"),
    end   = paste(max(aeonset_date), "23:45:00"),
    by    = "15 mins",
    size  = size
  )
  
  aestdtm <- gen_date_time(
    date    = aest_range,
    stime   = 7,
    etime   = 18,
    size    = size,
    min     = 15,
    replace = TRUE
  ) |> format("%H:%M")
  
  aest <- as.POSIXct(paste(aeonset_date, aestdtm))
  
  # End datetime: 1-30 days after start
  aeen_raw <- gen_day_after_dt(
    date    = aest,
    days    = 1:30,
    range   = seq(0, 55, by = 5),
    size    = size,
    replace = TRUE
  )
  
  aeen_raw[aeout == "Not Recovered/Not Resolved"] <- NA
  
  # Format dates via ymd_gen
  aest_fmt <- ymd_gen(aest,     time_off = "N")
  aeen_fmt <- ymd_gen(aeen_raw, time_off = "N")
  aeen_fmt[is.na(aeen_raw), ] <- NA
  
  # Ongoing flag
  aeongoing     <- ifelse(is.na(aeen_raw), "Yes", "No")
  aeongoing_std <- std_code(aeongoing, yn_fmt)
  
  
  # ── Build df explicitly — avoids env_to_df capturing subj_expanded/size ──
  df <- data.frame(
    aeterm, aeterm_std,
    aetoxgr, aetoxgr_std,
    aeser, aeser_std,
    aesdth, aesdth_std,
    aeslife, aeslife_std,
    aerel, aerel_std,
    aeout, aeout_std,
    aeacn, aeacn_std,
    aestdat = format(aest, format = "%d-%b-%Y"), 
    
    aest_fmt,
    aeendat = format(aeen_raw, format = "%d-%b-%Y"),
    aeen_fmt,
    aeongoing, aeongoing_std
  )
  
  return(df)
}

# ── Process function ──────────────────────────────────────────────────────────
process_ae <- function() {
  
  subject <- data.frame(
    subject = rep(subj_list$subject, times = ae_counts),
    aeno = sequence(ae_counts)
  ) |> left_join(subj_list, by = "subject")
  
  subj_expanded <- subject |>
    left_join(d1_dates,   by = "subject") |>
    left_join(last_dates, by = "subject")
  
  # Guard: confirm no NA dates before proceeding
  na_check <- sum(is.na(subj_expanded$visdt) | is.na(subj_expanded$last_visdt))
  if (na_check > 0) stop(sprintf("%d rows have NA visdt/last_visdt — check subject ID alignment", na_check))
  
  df1 <- gen_folder(
    crfpagename = "Adverse Events",
    foldername  = "Adverse Events",
    folder      = "AE",
    size        = ae_size
  )
  
  df2 <- gen_core_ae(subj_expanded) |>
    left_join(meddra_lut, by = "aeterm")
  
  df3 <- cbind(subject, df1, df2) |>
    select(-c(
      aest_year, aest_year_raw, aest_month, 
      aest_month_raw, aest_day, aest_day_raw,
      aeen_year, aeen_year_raw, aeen_month,
      aeen_month_raw, aeen_day,
      aeen_day_raw
    ))
  
  date_to_char(df3)
}
raw_ae <- process_ae()
names(raw_ae) <- toupper(names(raw_ae))
save(raw_ae, file = "data/raw_ae.rda")
