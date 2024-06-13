library(syngenR)

set.seed(101)

data <- meta
project <- subset(data, name == "study_id", value)  |> get_char()
projectid <- 120
siteinit <- subset(data, name == "site_init", value) |> get_num()
site_fmt <- subset(data, name == "site", value) |> get_char()
get_size <- function(data = meta) {
  subset(data, name == "subject_size", value) |> get_num()
}

size <- get_size()
trtstdt <- subset(data, name == "treatment_start_date", value) |> get_char() |> as.Date(format("%m/%d/%Y"))
sex_fmt <- subset(data, name == "sex", c(value, subvalue))
race_fmt <- subset(data, name == "race", c(value, subvalue))
ethnic_fmt <- subset(data, name == "ethnic", c(value, subvalue))
yn_fmt <- subset(data, name == "yes_no", value) |> get_char()
crf_dm <- subset(data, name == "dm", c(value, subvalue))
dm_fmt <- subset(data, name == "dm_visit", c(value, subvalue))
cbp_fmt <- subset(data, name == "cbp", c(value, subvalue))
visit_fmt <-  subset(data, name == "visit", c(value, subvalue))
