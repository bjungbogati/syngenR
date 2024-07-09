rm(list=ls())

source("inst/scripts/config.R")

subject <- mapply(rep, gen_site_subj(), each = nrow(visit_fmt)) |> data.frame()
size <- nrow(subject)

gen_core_visit <- function(folder) {

  visdt <- gen_visit_days(
    var = folder,
    sdate = trtstdt_range,
    scn_days = 12,
    size = size
  )

  visdt_raw <- visdt |> ymd_gen()
  visstat <- ifelse(is.na(visdt), 1, 0)
  visstat_raw <- visstat |> as.character()
  visreas <- ifelse(is.na(visdt), "", "")
  visreas_std <- ifelse(is.na(visdt), NA, "")

  df <- env_to_df(mget(ls()))
  df$folder <- NULL

  return(df)
}

process_visit <- function() {
  df1 <- gen_folder(
    crfpagename = "Visit",
    foldername = rep(visit_fmt$value, times = get_size()),
    folder = rep(visit_fmt$subvalue, times = get_size()),
    size = size
  )

  df2 <- gen_core_visit(folder = df1$folder)
  df3 <- cbind(subject, df1, df2)
  date_to_char(df3)
}

raw_visit <- process_visit()

save(raw_visit, file = "data/raw_visit.rda")

