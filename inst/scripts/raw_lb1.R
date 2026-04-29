# ── LB1 (Hematology) generation ───────────────────────────────────────────────
rm(list = ls())
source("config.R")

# ── Hematology analyte lookup table ──────────────────────────────────────────
hema_lut <- tribble(
  ~lbtestcd,  ~lbtest,        ~lbcat,       ~col,    ~start,  ~end,    ~mean,   ~sd,    ~digits,
  "HGB",      "Hemoglobin",   "HEMATOLOGY", "hgb",    7.6,    14.6,    11.7,    1.4,    1,
  "HCT",      "Hematocrit",   "HEMATOLOGY", "hct",   24.5,    42.9,    35.4,    4.0,    1,
  "RBC",      "Erythrocytes", "HEMATOLOGY", "rbc",    4.0,     4.61,    3.76,   0.48,   1,
  "WBC",      "Leukocytes",   "HEMATOLOGY", "wbc",    4.0,    17.22,    6.77,   3.57,   1,
  "PLAT",     "Platelets",    "HEMATOLOGY", "plat",  73.0,   508.0,   188.0,   89.89,  1,
  "NEUT",     "Neutrophils",  "HEMATOLOGY", "neut",   4.0,    15.0,     4.85,   3.18,   1,
  "LYM",      "Lymphocytes",  "HEMATOLOGY", "lym",    1.0,     3.72,    1.26,   0.61,   1,
  "EOS",      "Eosinophils",  "HEMATOLOGY", "eos",    1.0,     2.7,     0.20,   0.18,   1,
  "MONO",     "Monocytes",    "HEMATOLOGY", "mono",   1.0,     2.24,    0.65,   0.48,   1,
  "BASO",     "Basophils",    "HEMATOLOGY", "baso",   1.0,     2.25,    0.27,   0.02,   2
)

unit_pools <- list(
  HGB  = list(units = c("g/dL"),                                   prob = c(1.0)),
  HCT  = list(units = c("%"),                                       prob = c(1.0)),
  RBC  = list(units = c("10^6/mm^3", "10^12/L", "mil/uL"),         prob = c(0.4, 0.3, 0.3)),
  WBC  = list(units = c("K/uL", "K/mm^3", "10^3/mm^3", "10^9/L"), prob = c(0.4, 0.3, 0.2, 0.1)),
  PLAT = list(units = c("K/uL", "10^3/uL", "10^9/L"),              prob = c(0.5, 0.3, 0.2)),
  NEUT = list(units = c("K/uL", "10^3/mm^3", "10^9/L"),            prob = c(0.4, 0.3, 0.3)),
  LYM  = list(units = c("K/uL", "10^3/mm^3", "10^9/L"),            prob = c(0.4, 0.3, 0.3)),
  EOS  = list(units = c("K/uL", "10^3/mm^3", "10^9/L"),            prob = c(0.4, 0.3, 0.3)),
  MONO = list(units = c("K/uL", "10^3/mm^3", "10^9/L"),            prob = c(0.4, 0.3, 0.3)),
  BASO = list(units = c("K/uL", "10^3/mm^3", "10^9/L"),            prob = c(0.4, 0.3, 0.3))
)

# ── Formats from metadata ─────────────────────────────────────────────────────
lb_visit_fmt <- subset(data, name == "lb_visit", c(value, subvalue))
named_visit  <- setNames(lb_visit_fmt$subvalue, lb_visit_fmt$value)
lb1vis_fmt   <- paste0(lb_visit_fmt$value, " (", lb_visit_fmt$subvalue, ")")

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
  for (i in seq_len(nrow(hema_lut))) {
    row    <- hema_lut[i, ]
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

# ── Visit dates — drop remove column if absent ────────────────────────────────
vis_dates <- vis |>
  filter(subject %in% subj_list$subject) |>
  mutate(visdt = as.Date(visdt))

stopifnot(all(subj_list$subject %in% vis_dates$subject))

load("data/raw_ie.rda")

# ── Core LB1 generator ───────────────────────────────────────────────────────
gen_core_lb1 <- function(subj_expanded) {
  
  n <- nrow(subj_expanded)
  
  # ── Operational columns — mirrors raw_ae structure ──────────────────────────
  subject        <- subj_expanded$subject
  lbno           <- sequence(tabulate(as.factor(subject)))   # per-subject sequence
  projectid      <- projectid
  project        <- project
  site_list      <- subj_expanded$site_list
  site_group     <- subj_expanded$site_group
  crfpageid      <- seq_len(n)
  crfpagename    <- "Hematology (Lab Admin)"
  folder         <- subj_expanded$folder
  foldername     <- toupper(no_brac_txt(subj_expanded$instancename))
  recordid       <- seq_len(n)
  recordposition <- 1L
  
  # ── Lab collection date ───────────────────────────────────────────────────────
  lb1dt      <- subj_expanded$visdt
  lb1dt_raw  <- format(lb1dt, "%d %b %Y")
  lb1dt_yyyy <- as.integer(format(lb1dt, "%Y"))
  lb1dt_mm   <- as.integer(format(lb1dt, "%m"))
  lb1dt_dd   <- as.integer(format(lb1dt, "%d"))
  
  # ── Analyte block ─────────────────────────────────────────────────────────────
  analytes <- flatten_analytes(n)
  
  # ── Assemble — only operational + analyte columns ────────────────────────────
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
    lb1dt,
    # lb1dt_raw,
    # lb1dt_yyyy,
    # lb1dt_mm,
    # lb1dt_dd,
    analytes
  )
  
  return(df)
}

# ── Process function ──────────────────────────────────────────────────────────
process_lb1 <- function() {
  
  # 1. Expand subjects × lb visits
  subj_expanded <- subj_list |>
    dplyr::slice(rep(seq_len(n_subj), each = length(lb1vis_fmt))) |>
    dplyr::mutate(
      instancename = rep(lb1vis_fmt, times = n_subj),
      folder       = named_visit[no_brac_txt(instancename)] |> unname(),
      folderseq    = factor(folder, levels = named_visit) |> as.integer()
    )
  
  # 2. Join visit dates — only visdt (remove column not required here)
  subj_expanded <- subj_expanded |>
    dplyr::left_join(
      vis_dates |> dplyr::select(subject, folder, visdt),
      by = c("subject", "folder")
    )
  
  na_check <- sum(is.na(subj_expanded$visdt))
  if (na_check > 0)
    message(sprintf("[raw_lb1] %d rows missing visit date — will be dropped", na_check))
  
  # 3. Generate records
  df <- gen_core_lb1(subj_expanded)
  
  # 4. Filter: NA visit dates
  df <- df |> dplyr::filter(!is.na(lb1dt))
  
  # 5. Filter: screen failures
  # ie_scrnfail <- subset(raw_ie, IEORRES == "EXCLUDED")[, "SUBJECT", drop = FALSE]
  # names(ie_scrnfail) <- "SUBJECT"
  # df <- df |> dplyr::filter(!subject %in% ie_scrnfail$SUBJECT)
  
  date_to_char(df)
}

# ── Execute ───────────────────────────────────────────────────────────────────
raw_lb1        <- process_lb1()
names(raw_lb1) <- toupper(names(raw_lb1))

save(raw_lb1, file = "data/raw_lb1.rda")

message(sprintf(
  "[raw_lb1] saved  |  rows: %d  |  subjects: %d  |  visits: %d",
  nrow(raw_lb1),
  dplyr::n_distinct(raw_lb1$SUBJECT),
  dplyr::n_distinct(raw_lb1$FOLDER)
))
