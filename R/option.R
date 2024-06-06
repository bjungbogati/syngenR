library(readr)
meta <- read_csv("R/meta.csv")
# View(meta)
library(lubridate)
library(dplyr)

data <- meta

siteinit <- subset(data, name == "site_init", value) |> get_num()
site_fmt <- subset(data, name == "site", value) |> get_char()
get_size <- function(data = meta) {
  subset(data, name == "subject_size", value) |> get_num()
}

size <- get_size()
sex_fmt <- subset(data, name == "sex", c(value, factor))
race_fmt <- subset(data, name == "race", c(value, factor))
ethnic_fmt <- subset(data, name == "ethnic", c(value, factor))
yn_fmt <- subset(data, name == "yes_no", value) |> get_char()


crf_dm <- subset(data, name == "dm", value) |> get_char()

subject_gen <- function(data=meta, width=4) {
  subid <- siteinit * 5
  site_list <- list_gen(site_fmt, size)
  site_group <- siteinit + factor(site_list, levels = site_fmt) |> as.integer()
  group_seq <- ave(site_group, site_list, FUN = seq_along)

  # Generate unique subject IDs
  subjects <- paste(site_group, formatC(group_seq, width = width, flag = "0"), sep = "-") |> sort()

  return(subjects)
}

























