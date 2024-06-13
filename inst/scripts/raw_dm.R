rm(list=ls())

source("inst/scripts/config.R")

subject <- gen_site_subj()

gen_core_dm <- function() {
  recorddate <- date_gen(
    start = "2020/01/02",
    end = "2020/01/14",
    size = get_size()
  )
  sex <- std_gen(sex_fmt)
  race <- std_gen(race_fmt)
  ethnic <- factor(race$race, levels = ethnic_fmt$value, labels = ethnic_fmt$subvalue)
  ethnic_std <- factor(ethnic, levels = unique(ethnic_fmt$subvalue)) |> as.integer()

  age <- num_gen(start = 18, end = 60, size = get_size(), list = age)
  brth_dat <- birth_date(age$age, recorddate, size = get_size()) |> ymd_gen()

  # child bearing potential
  rpres <- ifelse(
    age$age < 60 & sex$sex == "female" &
      prob_gen(yn_fmt, size, prob = c(0.7, 0.3)) == "Yes",
    "Yes", NA
  )
  rpres_std <- factor(rpres, levels = yn_fmt) |> as.integer()
  df <- env_to_df(mget(ls()))

  return(df)
}

process_dm <- function() {
  df1 <- gen_folder(crfpagename = crf_dm$value, foldername = "SCREENING", folder = "SCN", size=get_size())
  df2 <- gen_core_dm()

  df3 <- cbind(subject, df1, df2)
  df3$race_std <- paste0("Race", df3$race_std)
  df3$racedummy <- ifelse(is.na(df3$race), 0, 1)
  df3 <- tidyr::pivot_wider(df3,
                            names_from = race_std,
                            values_from = racedummy,
                            values_fill = 0
  )
  df3$race <- NULL
  date_to_char(df3)
}

raw_dm <- process_dm()
save(raw_dm, file = "data/raw_dm.rda")
