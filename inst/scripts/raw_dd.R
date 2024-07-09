
rm(list = ls())

source("inst/scripts/config.R")

gen_core_dd <- function(size) {
  dddt <- date_gen(as.Date(max(trtstdt_range)) + 20, as.Date(max(trtstdt_range)) + 365, size)
  dddt_raw <- ymd_gen(dddt)
  ddterm <- std_gen(ddterm_fmt, size = size)
  df <- env_to_df(mget(ls()))
  return(df)
}

#  randomizationgen_core_dd
process_dd <- function() {

  subject <- sample(raw_dm$subject, size = length(raw_dm$subject) * 0.1, replace = F)
  size <- length(subject)
  df1 <- gen_folder(
    crfpagename = "Death Detail",
    foldername = "DEATH",
    folder = "DD",
    size = size
  )

  df2 <- gen_core_dd(size=size)
  df3 <- cbind(subject, df1, df2)
  date_to_char(df3)
}

raw_dd <- process_dd()
save(raw_dd, file = "data/raw_dd.rda")
