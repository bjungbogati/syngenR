library(syngenR)
library(dplyr)

addTaskCallback(function(...) {set.seed(123);TRUE})

# set.seed(123)

data <- meta
project <- subset(data, name == "study_id", value)  |> get_char()
projectid <- 120
siteinit <- subset(data, name == "site_init", value) |> get_num()
site_fmt <- subset(data, name == "site", value) |> get_char()
get_size <- function(data = meta) {
  subset(data, name == "subject_size", value) |> get_num()
}

size <- get_size()
visit_fmt <- subset(data, name == "visit", c(value, subvalue))
rndose_fmt <- subset(data, name == "rndose", c(value, subvalue))
trtstdt <- subset(data, name == "treatment_start_date", value) |> get_char() |> as.Date(format("%m/%d/%Y"))

trtstdt_range <- gen_date_range(start = trtstdt, end = trtstdt + 90, by = "1 day") |>
  as.character() |>
  sample(size, replace = T)

sex_fmt <- subset(data, name == "sex", c(value, subvalue))
race_fmt <- subset(data, name == "race", c(value, subvalue))
ethnic_fmt <- subset(data, name == "ethnic", c(value, subvalue))
yn_fmt <- subset(data, name == "yes_no", value) |> get_char()
crf_dm <- subset(data, name == "dm", c(value, subvalue))
dm_fmt <- subset(data, name == "dm_visit", c(value, subvalue))
cbp_fmt <- subset(data, name == "cbp", c(value, subvalue))
visit_fmt <-  subset(data, name == "visit", c(value, subvalue))
ddterm_fmt <-  subset(data, name == "dd_cause", c(value, subvalue))

vis <- raw_visit[, c("subject", "folder", "visdt")]

trt_fmt <- subset(data, name== "ex_visit", c(value, subvalue))

treatment_days <- nrow(trt_fmt)

