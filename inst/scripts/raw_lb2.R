# ── LB2 (Chemistry) generation ────────────────────────────────────────────────
rm(list = ls())
source("config.R")

# ── Chemistry analyte lookup table ───────────────────────────────────────────
chem_lut <- tribble(
  ~lbtestcd,   ~lbtest,                          ~lbcat,      ~col,       ~start,  ~end,    ~mean,    ~sd,      ~digits,
  "UREAN",     "Urea Nitrogen",                  "CHEMISTRY", "urean",     8.0,    32.0,    15.00,    5.00,     1,
  "CREAT",     "Creatinine",                     "CHEMISTRY", "creat",     1.0,     1.26,    0.77,    0.20,     2,
  "CREATCLR",  "Creatinine Clearance",           "CHEMISTRY", "creatclr",  1.0,   165.0,    91.85,   44.60,    1,
  "GLUC",      "Glucose",                        "CHEMISTRY", "gluc",     75.0,   224.0,   119.00,   32.91,    1,
  "K",         "Potassium",                      "CHEMISTRY", "k",         3.2,     5.0,     4.00,    0.40,     1,
  "SODIUM",    "Sodium",                         "CHEMISTRY", "sodium",  131.0,   148.0,   139.32,    2.85,    1,
  "CA",        "Calcium",                        "CHEMISTRY", "ca",        8.4,    10.7,     9.34,    0.45,     1,
  "CL",        "Chloride",                       "CHEMISTRY", "cl",       97.0,   111.0,   105.10,    3.11,    1,
  "CO2",       "Carbon Dioxide",                 "CHEMISTRY", "co2",      16.0,    31.0,    24.95,    2.67,     1,
  "PHOS",      "Phosphate",                      "CHEMISTRY", "phos",      1.6,     4.9,     3.46,    0.71,     1,
  "AST",       "Aspartate Aminotransferase",     "CHEMISTRY", "ast",      97.0,   111.0,   105.10,    3.11,    1,
  "ALT",       "Alanine Aminotransferase",       "CHEMISTRY", "alt",       9.0,   398.0,    42.86,   58.52,    1,
  "GGT",       "Gamma Glutamyl Transferase",     "CHEMISTRY", "ggt",      16.0,   810.0,   158.78,  200.77,    1,
  "BILI",      "Bilirubin Total",                "CHEMISTRY", "bili",      0.1,     2.8,     0.57,    0.48,     1,
  "BILDIR",    "Bilirubin Direct",               "CHEMISTRY", "bildir",    0.1,     1.1,     0.36,    0.31,     1,
  "ALB",       "Albumin",                        "CHEMISTRY", "alb",       2.7,     4.7,     3.82,    0.40,     1,
  "ALP",       "Alkaline Phosphatase",           "CHEMISTRY", "alp",      61.0,   744.0,   193.41,  156.25,    1,
  "URATE",     "Urate",                          "CHEMISTRY", "urate",     2.7,     8.4,     5.25,    1.46,     1
)

# Unit pools — fixed for most chemistry analytes; variable for electrolytes
unit_pools <- list(
  UREAN    = list(units = c("mg/dL"),           prob = c(1.0)),
  CREAT    = list(units = c("mg/dL"),           prob = c(1.0)),
  CREATCLR = list(units = c("mL/min"),          prob = c(1.0)),
  GLUC     = list(units = c("mg/dL"),           prob = c(1.0)),
  K        = list(units = c("mmol/L", "mEq/L"), prob = c(0.5, 0.5)),
  SODIUM   = list(units = c("mmol/L", "mEq/L"), prob = c(0.5, 0.5)),
  CA       = list(units = c("mg/dL"),           prob = c(1.0)),
  CL       = list(units = c("mmol/L"),          prob = c(1.0)),
  CO2      = list(units = c("mmol/L"),          prob = c(1.0)),
  PHOS     = list(units = c("mmol/L"),          prob = c(1.0)),
  AST      = list(units = c("mmol/L"),          prob = c(1.0)),
  ALT      = list(units = c("mg/dL"),           prob = c(1.0)),
  GGT      = list(units = c("mg/dL"),           prob = c(1.0)),
  BILI     = list(units = c("mg/dL"),           prob = c(1.0)),
  BILDIR   = list(units = c("mg/dL"),           prob = c(1.0)),
  ALB      = list(units = c("mg/dL"),           prob = c(1.0)),
  ALP      = list(units = c("mg/dL"),           prob = c(1.0)),
  URATE    = list(units = c("mg/dL"),           prob = c(1.0))
)

# ── Formats from metadata ─────────────────────────────────────────────────────
lb_visit_fmt <- subset(data, name == "lb_visit", c(value, subvalue))
named_visit  <- setNames(lb_visit_fmt$subvalue, lb_visit_fmt$value)
lb2vis_fmt   <- paste0(lb_visit_fmt$value, " (", lb_visit_fmt$subvalue, ")")

# ── Helpers ───────────────────────────────────────────────────────────────────
gen_analyte <- function(row, n) {
  vals <- float_bell_gen(
    start  = row$start,
    end    = row$end,
    mean   = row$mean,
    sd     = row$sd,
    size   = n,
    digits = row$digits
  )
  pool <- unit_pools[[row$lbtestcd]]
  un   <- prob_gen(pool$units, n, prob = pool$prob)
  list(
    val  = vals,
    raw  = as.character(vals),
    un   = un,
    low  = lb_low(vals),
    high = lb_high(vals),
    flag = high_low(vals, lb_high(vals), lb_low(vals))
  )
}

flatten_analytes <- function(n) {
  out <- list()
  for (i in seq_len(nrow(chem_lut))) {
    row    <- chem_lut[i, ]
    prefix <- row$col
    a      <- gen_analyte(row, n)
    out[[prefix]]                  <- a$val
    out[[paste0(prefix, "_raw")]]  <- a$raw
    out[[paste0(prefix, "_un")]]   <- a$un
    out[[paste0(prefix, "_low")]]  <- a$low
    out[[paste0(prefix, "_high")]] <- a$high
    out[[paste0(prefix, "_flag")]] <- a$flag
  }
  as.data.frame(out)
}

# ── Subject roster ────────────────────────────────────────────────────────────
subj_list <- gen_site_subj(seed)
n_subj    <- nrow(subj_list)

# ── Visit dates ───────────────────────────────────────────────────────────────
vis_dates <- vis |>
  filter(subject %in% subj_list$subject) |>
  mutate(visdt = as.Date(visdt))

stopifnot(all(subj_list$subject %in% vis_dates$subject))

# ── Core LB2 generator ───────────────────────────────────────────────────────
gen_core_lb2 <- function(subj_expanded) {
  
  n <- nrow(subj_expanded)
  
  # Operational columns — mirrors raw_ae / raw_lb1 structure
  subject        <- subj_expanded$subject
  lbno           <- sequence(tabulate(as.factor(subject)))
  projectid      <- projectid
  project        <- project
  site_list      <- subj_expanded$site_list
  site_group     <- subj_expanded$site_group
  crfpageid      <- seq_len(n)
  crfpagename    <- "Chemistry (Lab Admin)"
  folder         <- subj_expanded$folder
  foldername     <- toupper(no_brac_txt(subj_expanded$instancename))
  recordid       <- seq_len(n)
  recordposition <- 1L
  
  # Lab collection date
  lb2dt      <- subj_expanded$visdt
  lb2dt_raw  <- format(lb2dt, "%d %b %Y")
  lb2dt_yyyy <- as.integer(format(lb2dt, "%Y"))
  lb2dt_mm   <- as.integer(format(lb2dt, "%m"))
  lb2dt_dd   <- as.integer(format(lb2dt, "%d"))
  
  # Analyte block
  analytes <- flatten_analytes(n)
  
  # Assemble
  df <- data.frame(
    subject,
    lbno,
    projectid,
    project,
    site_list,
    site_group,
    crfpageid,
    crfpagename,
    folder,
    foldername,
    recordid,
    recordposition,
    lb2dt,
    lb2dt_raw,
    lb2dt_yyyy,
    lb2dt_mm,
    lb2dt_dd,
    analytes
  )
  
  return(df)
}

# ── Process function ──────────────────────────────────────────────────────────
process_lb2 <- function() {
  
  # 1. Expand subjects × lb visits
  subj_expanded <- subj_list |>
    dplyr::slice(rep(seq_len(n_subj), each = length(lb2vis_fmt))) |>
    dplyr::mutate(
      instancename = rep(lb2vis_fmt, times = n_subj),
      folder       = named_visit[no_brac_txt(instancename)] |> unname(),
      folderseq    = factor(folder, levels = named_visit) |> as.integer()
    )
  
  # 2. Join visit dates
  subj_expanded <- subj_expanded |>
    dplyr::left_join(
      vis_dates |> dplyr::select(subject, folder, visdt),
      by = c("subject", "folder")
    )
  
  na_check <- sum(is.na(subj_expanded$visdt))
  if (na_check > 0)
    message(sprintf("[raw_lb2] %d rows missing visit date — will be dropped", na_check))
  
  # 3. Generate records
  df <- gen_core_lb2(subj_expanded)
  
  # 4. Filter: NA visit dates
  df <- df |> dplyr::filter(!is.na(lb2dt))
  
  # 5. Filter: screen failures
  # ie_scrnfail <- subset(raw_ie, IEORRES == "EXCLUDED")[, "SUBJECT", drop = FALSE]
  # names(ie_scrnfail) <- "SUBJECT"
  # df <- df |> dplyr::filter(!subject %in% ie_scrnfail$SUBJECT)
  
  date_to_char(df)
}

# ── Execute ───────────────────────────────────────────────────────────────────
raw_lb2        <- process_lb2()
names(raw_lb2) <- toupper(names(raw_lb2))

save(raw_lb2, file = "data/raw_lb2.rda")

message(sprintf(
  "[raw_lb2] saved  |  rows: %d  |  subjects: %d  |  visits: %d",
  nrow(raw_lb2),
  dplyr::n_distinct(raw_lb2$SUBJECT),
  dplyr::n_distinct(raw_lb2$FOLDER)
))