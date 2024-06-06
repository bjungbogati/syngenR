library(tidyr)
library(dplyr)

subject <- subject_gen()

prefun <- function(subject){
  instancename <- "SCREENING"
  instancerepeatnumber <- 0L
  foldername <- instancename
  crfpagename <- crf_dm
  folder <- "SCN"
  folderseq <- 1L
  datapageid <- subject |> as.factor() |> as.numeric()
  recordid <- 1:size
  projectid <- 10
  environmentname <- "Prod"
  folderid <- datapageid + folderseq
  instanceid <- folderid + 5
  siteid <- datapageid + projectid
  recordposition <- prob_gen(c(1, 0), size = get_size(), prob = c(1, 0))

  data.frame(mget(ls()))
}


generate_dm <- function(){

  recorddate <- date_gen(
    start = "2020/01/02",
    end = "2020/01/14",
    size = get_size()
  )

  sex <- std_gen(sex_fmt)
  race <- std_gen(race_fmt)
  ethnic <- std_gen(ethnic_fmt)
  age <- num_gen(start = 18, end = 60, size = get_size(), list = age)
  brth_dat <- birth_date(age$age, recorddate, size = get_size()) |> ymd_gen()

  # child bearing potential
  rpres <- ifelse(
    age$age < 60 & sex$sex == "female" &
      prob_gen(yn_fmt, size, prob = c(0.7, 0.3)) == "Yes",
    "Yes", NA
  )
  rpres_std <- factor(rpres, levels = yn_fmt) |> as.integer()

  df <- data.frame(mget(ls()))

  return(df)
}

process_dm <- function(dm_demo){

  df1 <-  prefun(subject)
  df2 <- generate_dm()

  df3 <- cbind(subject, df1, df2)
  df3$race_std <- paste0("Race", dm_demo$race_std)
  df3$racedummy <- ifelse(is.na(dm_demo$race), 0, 1)
  df3 <- pivot_wider(dm_demo,
                       names_from = race_std,
                       values_from = racedummy,
                       values_fill = 0
  )
  df3$race <- NULL
  date_to_char(df3)
}



# prefun(subject)

generate_dm()

process_dm()



