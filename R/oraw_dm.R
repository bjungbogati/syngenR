
instancename <- "SCREENING"
instancerepeatnumber <- 0L
foldername <- instancename
datapagename <- "Demographics"
folder <- "SCN"
folderseq <- 1L

recorddate <- date_gen(
  start = "2020/01/02",
  end = "2020/01/14",
  size = get_size()
)

subject <- subject_gen()

recordposition <- prob_gen(c(1, 0), size = get_size(), prob = c(1, 0))

datapageid <- subject |> as.factor() |> as.numeric()
pagerepeatnumber <- 0L
recordid <- 1:size
sitegroup <- ""
targetdays <- ""
projectid <- 10
environmentname <- "Prod"
folderid <- datapageid + folderseq
instanceid <- folderid + 5
siteid <- datapageid + projectid
# sub1 <- data.frame(sub = subject |> as.factor() |> as.numeric() + max(subid))
#
# subjectid <- unique(sub1) |>  group_by(sub) |>
#   mutate(sample = sample(1:15, n(), replace = T)) |>
#   merge(sub1) |>
#   mutate(add = sub + sample) |>
#   pull(add)

sex <- prob_gen(sex_fmt$value, size, prob = sex_fmt$factor)
sex_std <- factor(sex, levels = names(sex_fmt)
) |> as.integer()


std_gen <- function(list, data=meta){

  dfname <- deparse(substitute(list))
  if(dfname != "ethnic_fmt"){
  code <-  prob_gen(list=list$value, size=get_size(), prob = list$factor)
  decode <- factor(code, levels = list$value) |> as.integer()
  } else if(dfname == "ethnic_fmt" & is.data.frame(get('race'))) {
    ethnic <- merge(race, ethnic_fmt, by.x=c("race"), by.y=c("value"))
    code <- ethnic$factor
    decode <- factor(code, levels = unique(list$factor)) |> as.integer()
  }
  name <- sub("_.*", "", dfname)
  std <- paste(name, "std", sep="_")
  df <- data.frame(code, decode)
  names(df) <- c(name, std)
  return(df)
}

sex <- std_gen(sex_fmt)
race <- std_gen(race_fmt)
ethnic <- std_gen(ethnic_fmt)
age <- num_gen(start = 18, end = 60, size = get_size(), list= age)

# density_plot(age)

# birth_date
brthdat <- birth_date(age$age, recorddate, size=get_size())
# brthyr <- birth_year(age$age, recorddate)
# brthyr_raw <- brthyr |> as.character()
# brthyr_mm <- format(brthdat, "%m")
# brthyr_dd <- format(brthdat, "%d")

# brthyr_mm <- NA
# brthyr_dd <- NA

# child bearing potential
rpres <- ifelse(
  age$age < 60 & sex$sex == "female" &
    prob_gen(yn_fmt, size, prob = c(0.7, 0.3)) == "Yes",
  "Yes", NA
)

rpres_std <- factor(rpres, levels = yn_fmt) |> as.integer()

# create dates
range_dt <- gen_date_range(
  start = "2020/01/01",
  end = "2020/01/08",
  by = "15 mins"
)
mincreated <- gen_date_time(
  date = range_dt,
  stime = 9,
  etime = 15,
  size = size,
  min = 9,
  replace = T
)
maxupdated <- gen_after_dt(
  date = mincreated,
  range = 10:15,
  size = size,
  replace = T
)

savets <- maxupdated <- mincreated

z_dmnow <- max(savets)
z_dmnow_raw <- format(z_dmnow, "%d %b %Y %H:%M")
z_dmnow_int <- z_dmnow
z_dmnow_yyyy <- format(z_dmnow, "%Y") |> as.numeric()
z_dmnow_mm <- format(z_dmnow, "%m") |> as.numeric()
z_dmnow_dd <- format(z_dmnow, "%d") |> as.numeric()

# create data frame
dm_demo <- data.frame(
  projectid,
  # project,
  # studyid,
  subject,
  # site,
  sitegroup,
  # subjectid,
  # sitenumber,
  siteid,
  environmentname,
  # studyenvsitenumber,
  instancename,
  instancerepeatnumber,
  instanceid,
  datapagename,
  datapageid,
  pagerepeatnumber,
  foldername,
  folder,
  folderid,
  folderseq,
  recorddate,
  recordposition,
  recordid,
  targetdays,
  sex,
  rpres,
  rpres_std,
  race,
  ethnic,
  age,
  brth_full,
  # brthyr_raw,
  # brthyr_mm,
  # brthyr_dd,
  mincreated,
  maxupdated,
  savets,
  z_dmnow,
  z_dmnow_raw,
  z_dmnow_int,
  z_dmnow_yyyy,
  z_dmnow_mm,
  z_dmnow_dd
)

# transpose race
# dm_demo$race_std <- paste0("Race", dm_demo$race_std)
# dm_demo$racedummy <- ifelse(is.na(dm_demo$race), 0, 1)
# dm_dd <- pivot_wider(dm_demo,
#                      names_from = race_std,
#                      values_from = racedummy,
#                      values_fill = 0
# )

# dm_dd$race <- NULL
#
# # dm_dd <- dm_dd |> rename(Raceoth=Race7)
#
# # dm_dd$RACEOTHSP <- ifelse(dm_dd$RACEOTH == 1)
# # dm_dd <- date_to_char(dm_dd)
#
#
# # gender data
# dm_sex <- dm_dd |> select(subject, sex, sex_std)
#
# # uppercase the columns and get labels from raw dm
# names(dm_dd) <- toupper(names(dm_dd))
# label_dm <- dm_var_labels[names(dm_dd)] |> head(0)
#
# # apply original labels to new data set
# dm_var_labels <- lapply(label_dm, attr, "label") |> unlist()
# attr(dm_dd, "variable.labels") <- dm_var_labels

# reorder

dm_dd <- dm_dd |> select(
  PROJECTID,
  PROJECT,
  STUDYID,
  SUBJECT,
  SITE,
  SITEGROUP,
  SUBJECTID,
  SITENUMBER,
  SITEID,
  ENVIRONMENTNAME,
  STUDYENVSITENUMBER,
  INSTANCENAME,
  INSTANCEREPEATNUMBER,
  INSTANCEID,
  DATAPAGENAME,
  DATAPAGEID,
  PAGEREPEATNUMBER,
  FOLDERNAME,
  FOLDER,
  FOLDERID,
  FOLDERSEQ,
  RECORDDATE,
  RECORDPOSITION,
  RECORDID,
  TARGETDAYS,
  SEX,
  SEX_STD,
  RPRES,
  RPRES_STD,
  ETHNIC,
  ETHNIC_STD,
  AGE,
  BRTHYR,
  BRTHYR_RAW,
  RACE1, RACE2, RACE3, RACE4, RACE5, RACE6, MINCREATED,
  MAXUPDATED,
  SAVETS,
  Z_DMNOW,
  Z_DMNOW_RAW,
  Z_DMNOW_YYYY,
  Z_DMNOW_MM,
  Z_DMNOW_DD
)

