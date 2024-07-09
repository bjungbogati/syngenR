rm(list = ls())

source("inst/scripts/config.R")
subject <- gen_site_subj()

gen_core_ds <- function() {
  # subject <- gen_site_subj()
  dsendt <- data.frame(subject = subject$subject, folder = tail(visit_fmt$subvalue, 1)) |>
    merge(vis, all.x=T) |>
    merge(raw_dd[, c("subject", "dddt")], all.x=T)

  dsendt <- ifelse(is.na(dsendt$visdt), dsendt$dddt, dsendt$visdt)
  ds_endt_raw <- ymd_gen(dsendt)
  df <- env_to_df(mget(ls()))
  return(df)

}


process_ds <- function() {
  df1 <- gen_folder(crfpagename = "End of Study", foldername = "END OF STUDY", folder = "EOS", size = get_size())
  df2 <- gen_core_ds()
  df3 <- cbind(subject, df1, df2)

  date_to_char(df3)
}

raw_ds <- process_ds()
save(raw_visit, file = "data/raw_ds.rda")

