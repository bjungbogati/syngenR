
subject <- rep_each(max_size = 4, subj = subject_origin, size = )

size <- length(subject)

crfpagename <- "Concomitant Medications"

sub1 <- data.frame(sub = subject |> as.factor() |> as.numeric() + max(subid))

subjectid <- unique(sub1) |>  group_by(sub) |>
  mutate(sample = sample(1:15, n(), replace = T)) |>
  merge(sub1) |>
  mutate(add = sub + sample) |>
  pull(add)

recorddate <- date_gen(
  start = "2020/01/08",
  end = "2020/02/08",
  size = size
)
recordposition <- 1L

yn_fmt <- function(){
  yn <- prob_gen(list = yn_fmt, size = size, prob = c(0.7, 0.3))
  yn_std <- factor(cmyn, levels = yn_fmt) |> as.integer()
  yn_list <- list(yn, yn_std)

  return(yn_list)
}


yn_fmt(list = yn_fmt, size = size, prob = c(0.7, 0.3))


cmyn <- prob_gen(list = yn_fmt, size = size, prob = c(0.7, 0.3))
cmyn_std <- factor(cmyn, levels = yn_fmt) |> as.integer()

medname <- prob_gen(named_cmtrt, size = size, prob = c(0.4, 0.2, 0.2, 0.2))
cmtrt <- medname |> names()
cmtrt_atc <- medname |> unname()
cmtrt_atc_code <- cmtrt_atc |> substr(1, 1)

# renaming
cmroute <- named_cmroute[cmtrt] |> unname()
cmroute_std <- factor(cmroute, levels = cmroute_fmt) |> as.integer()

# dose info

cmindic <- named_cmindic[cmtrt] |> unname()
cmdose <- named_cmdose[cmtrt] |> unname()

cmunit <- named_cmunit[cmtrt] |> unname()
cmunit_std <- substr(cmunit, 1, 3) |>
  trimws() |>
  toupper()

cmfreq <- named_cmfreq[cmtrt] |> unname()
cmfreq_std <- substr(cmfreq, 1, 3) |>
  trimws() |>
  toupper()

cmtrt_coderdictname <- "WHODrug-Global-B3"
cmtrt_coderdictversion <- "202003"

cmstdt <- date_gen(
  start = "2017/07/08",
  end = "2019/11/12", size = size
)
#
# cmstdt <- cmstdt + lubridate::year(1)

# cm start and end dates

cmstdt_raw <- format(cmstdt, "%d %b %Y")
cmstdt_int <- cmstdt
cmstdt_yyyy <- format(cmstdt, "%Y") |> as.numeric()
cmstdt_mm <- format(cmstdt, "%m") |> as.numeric()
cmstdt_dd <- format(cmstdt, "%d") |> as.numeric()

cmendt <- cmstdt + duration("P7D")

cmendt_raw <- format(cmendt, "%d %b %Y")
cmendt_int <- cmendt
cmendt_yyyy <- format(cmendt, "%Y") |> as.numeric()
cmendt_mm <- format(cmendt, "%m") |> as.numeric()
cmendt_dd <- format(cmendt, "%d") |> as.numeric()

# ongoing

# cmongo <- ifelse(is.na(cmendt), 0, 1)
# cmongo_raw <- cmongo |> as.character()

# create dates
range_dt <- gen_date_range(
  start = "2020/12/01",
  end = "2020/12/08",
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
savets <- maxupdated <- mincreated

z_cmnow <- max(savets)
z_cmnow_raw <- format(z_cmnow, "%d %b %Y %H:%M")
z_cmnow_int <- z_cmnow
z_cmnow_yyyy <- format(z_cmnow, "%Y") |> as.numeric()
z_cmnow_mm <- format(z_cmnow, "%m") |> as.numeric()
z_cmnow_dd <- format(z_cmnow, "%d") |> as.numeric()

# create data frame
cm_dd <- data.frame(
  projectid,
  project,
  studyid,
  subject,
  # site,
  # sitenumber,
  # studyenvsitenumber,
  siteid,
  subjectid,
  sitegroup,
  instancename,
  instancerepeatnumber,
  instanceid,
  datapagename,
  datapageid,
  pagerepeatnumber,
  environmentname,
  foldername,
  folder,
  folderseq,
  folderid,
  recorddate,
  recordposition,
  recordid,
  targetdays,
  cmyn,
  cmyn_std,
  cmtrt,
  cmtrt_atc,
  cmtrt_atc_code,
  cmroute,
  cmroute_std,
  cmindic,
  cmdose,
  cmunit,
  cmunit_std,
  cmfreq,
  cmfreq_std,
  cmtrt_coderdictname,
  cmtrt_coderdictversion,
  cmstdt,
  cmstdt_raw,
  cmstdt_int,
  cmstdt_yyyy,
  cmstdt_mm,
  cmstdt_dd,
  cmendt_raw,
  cmendt_int,
  cmendt_yyyy,
  cmendt_mm,
  cmendt_dd,
  # cmongo,
  # cmongo_raw,
  mincreated,
  maxupdated,
  savets,
  z_cmnow_raw,
  z_cmnow_int,
  z_cmnow_yyyy,
  z_cmnow_mm,
  z_cmnow_dd
)

# merge data
cm_dd <- left_join(cm_dd, op_site, by = "subject") |>
  select(projectid, project, subject, site, sitenumber, studyenvsitenumber, everything())
