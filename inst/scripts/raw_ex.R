rm(list = ls())

source("inst/scripts/config.R")

temp_subj <- gen_site_subj()
subject <- data.frame(subject = rep(temp_subj$subject, each = treatment_days)) %>%
  left_join(temp_subj)
size <- nrow(subject)


gen_core_ex <- function(df) {
  # was study drug administered
  exadmyn <- "Yes"
  exadmyn_std <- 1

  # if no reason not administered
  exrsnnd <- ""
  exrsnnd_std <- NA

  exdt <- cbind(subject, df) |>
    merge(vis, by = c("subject", "folder")) |>
    pull(visdt)

  exsttm <- gen_date_time(
    date = gen_dt(),
    stime = 9,
    etime = 15,
    size = size,
    min = 9,
    replace = T
  ) |> format("%H:%M")

  exstdtc <- as.POSIXct(paste(exdt, exsttm))

  exendtc <- gen_after_dt(
    date = exstdtc,
    range = 9:15,
    size = size,
    replace = T
  )

  exstdtc <- ymd_gen(exstdtc, time_off = "N")
  exendtc <- ymd_gen(exendtc, time_off = "N")

  # volume prepared
  exvolamt <- prob_gen(c("75", "74", "76"), get_size(), prob = c(0.7, 0.2, 0.1)) |>
    rep(each = treatment_days) |>
    as.integer()

  exvolamt_raw <- exvolamt |> as.character()

  df <- env_to_df(mget(ls()))
  return(df)
}

process_ds <- function() {

    df1 <- gen_folder(
      crfpagename = "Study Drug Administration",
      foldername = trt_fmt$value,
      folder = trt_fmt$subvalue, size = size
    )

  df2 <- gen_core_ex(df1)
  df3 <- cbind(subject, df1, df2)

  date_to_char(df3)
}

raw_ex <- process_ds()
save(raw_ex, file = "data/raw_ex.rda")


