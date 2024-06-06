library(readr)
meta <- read_csv("R/meta.csv")
# View(meta)
library(lubridate)
library(dplyr)

#
# get_val(meta, name =="subject_size", value)
#
# get_val(meta, category == "site", value)
#
#
#
# subset(meta, name == "subject_size", value) |> unlist() |> as.character()
#
#
# subset(meta, category == "site", code)


# site_fmt <- subset(meta, name == "site", value) |> get_val()
# size <- subset(meta, name == "subject_size", value) |> get_val()
#
#
# site_list <- list_gen(site_fmt, size)
# site_group <- siteinit + factor(site_list, levels = site_fmt) |> as.integer()
# subject_origin <- subject_gen(site_list, site_group,
#                               size = size,
#                               init = siteinit, width = 4
# ) |> sort()
# site <- paste(site_group, site_list, sep = " - ")
# sitenumber <- paste(siteno, site_group, sep = "-")
# studyenvsitenumber <- site_group |> as.character()


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

subject_gen <- function(data=meta, width=4) {

#
#   data <- meta
#   width <- 4


  # site_init <- 1000
  # site_fmt <- c("University","Hospital","Clinic")
  # size <- 20

  # size <- size
  # siteinit <- 100
  subid <- siteinit * 5
  site_list <- list_gen(site_fmt, size)
  site_group <- siteinit + factor(site_list, levels = site_fmt) |> as.integer()
  group_seq <- ave(site_group, site_list, FUN = seq_along)
  # paste(site_group, formatC(group_seq, width = width, flag = "0"), sep = "-")

  # Generate unique subject IDs
  subjects <- paste(site_group, formatC(group_seq, width = width, flag = "0"), sep = "-") |> sort()

  return(subjects)
}

subject_gen()


























