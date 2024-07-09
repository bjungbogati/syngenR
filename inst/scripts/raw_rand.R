rm(list = ls())

source("inst/scripts/config.R")

gen_core_rand <- function() {
  range_dt <- gen_date_range(start = trtstdt - 30, end = trtstdt - 5, by = "8 mins", size = get_size())
  rntm <- gen_date_time(date = range_dt, stime = 5, etime = 8, size = size, min = 8, replace = T) |> format("%H:%M")
  rndose <- std_gen(rndose_fmt, size = size)
  rnnum <- paste0("10", sub(".*-", "\\1", gen_site_subj()$subject))
  df <- env_to_df(mget(ls()))
  return(df)
}

#  randomization
process_rand <- function() {
  subject <- gen_site_subj()
  df1 <- gen_folder(
    crfpagename = "Randomization",
    foldername = "Treatment Period Day -1",
    folder = "TPDM",
    size = size
  )
  df2 <- gen_core_rand()
  df3 <- cbind(subject, df1, df2)
  date_to_char(df3)
}

raw_rand <- process_rand()
save(raw_rand, file = "data/raw_rand.rda")
