rep_each <- function(max_size, subj, size = size) {
  rep_sum <- sample(0:max_size, size = size, replace = T) |> sum()

  sample(subj, size = rep_sum, replace = T) |> sort()
}

get_char <- function(data) {
  data |>
    unlist() |>
    as.character()
}

get_num <- function(data) {
  data |>
    unlist() |>
    as.numeric()
}

# get_val <- function(data, filter, value){
#   subset(data, eval(parse(text = filter)), value) |> as.character()
# }

date_origin <- function(x) as.Date(x, origin = "1970-01-01")

date_gen <- function(start, end, size) {
  sample(
    x = seq(
      from = as.Date(start),
      to = as.Date(end),
      by = "day"
    ),
    size = size,
    replace = T
  )
}


list_gen <- function(list, size, replace = T) {
  sample(list,
    size = size,
    replace = replace
  )
}

prob_gen <- function(list, size, prob, replace = T) {
  sample(list,
    size = size,
    replace = replace,
    prob = prob
  )
}

subject_gen <- function(list, group, size, init, width) {
  group_seq <- ave(1:size, list, FUN = seq_along)
  paste(group, formatC(group_seq, width = width, flag = "0"), sep = "-")
}


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


num_gen <- function(start, end, mean = 40, sd = 12, size, list, unique = T) {
  val <- rnorm(start:end, mean = mean, sd = sd) |>
    as.integer() |>
    sample(size = size, replace = unique)

  val_char <- as.character(val)

  dfname <- deparse(substitute(list))
  name <- sub("_.*", "", dfname)
  raw <- paste(name, "raw", sep="_")
  df <- data.frame(val, val_char)
  names(df) <- c(name, raw)
  return(df)
}

bell_gen <- function(start, end, mean, sd, size, unique = T) {
  rnorm(start:end, mean = mean, sd = sd) |>
    as.integer() |>
    sample(size = size, replace = unique) |>
    abs()
}

float_bell_gen <- function(start, end, mean = 40, sd = 10, size, unique = T, digits = 1) {
  rnorm(start:end, mean = mean, sd = sd) |>
    sample(size = size, replace = unique) |>
    formatC(digits = digits, format = "f") |>
    as.numeric() |>
    abs()
}

yyyy <- function(x) format(x, "%Y") |> as.numeric()
mm <- function(x) format(x, "%m") |> as.numeric()
dd <- function(x) format(x, "%d") |> as.numeric()

birth_year <- function(x, y) {
  y |>
    format(format = "%Y") |>
    as.integer() - x
}

birth_date <- function(x, y, size) {
  dat <- paste(birth_year(x, y),
    sample(1:12, size, T),
    sample(1:31, size, T),
    sep = "-"
  ) |> as.Date()

  return(dat)

}

ymd_gen <- function(dat){

  dfname <- deparse(substitute(brth))
  name <- sub("_.*", "", dfname)
  year <- yyyy(dat)
  month <- mm(dat)
  day <- dd(dat)

  year_raw <- as.character(year)
  month_raw <- as.character(month)
  day_raw <- as.character(day)

  df <- data.frame(year, month, day, year_raw, month_raw, day_raw)
  names(df) <- paste(name, names(df),  sep="_")
  return(df)

}

# ymd_gen(brth_full)

gen_date_range <- function(start, end, by) sample(seq(as.POSIXct(start), as.POSIXct(end), by = by), size = size, replace = T)

gen_date_time <- function(date, stime, etime, size, min, replace) {
  sample(
    date[hour(date) > stime & hour(date) < etime], size,
    replace = replace
  )
}

gen_dt <- function(date = Sys.Date()) {
  paste0(
    date, " ", sample(9:15, 1000, replace = T), ":",
    sample(seq(00, 59, by = 15), 1000, replace = T)
  ) |> as.POSIXct()
}

gen_after_dt <- function(date, range, size, replace) {
  date +
    minutes(sample(range, size, replace = T))
} #+
# sample(seq(0, 59), size, replace = T)


gen_day_after_dt <- function(date, days, range, size, replace) {
  date +
    days(sample(days, size, replace = T)) +
    minutes(sample(range, size, replace = T))
} #+

# converter
cel_to_fhr <- function(x, digits) formatC(x * 1.8 + 32, digits = digits, format = "f")

date_to_char <- function(df) {
  indf <- sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))
  df[indf] <- lapply(df[indf], as.character)
  df
}

# remove parenthesis

no_brac_txt <- function(x) gsub("\\s*\function([^\\)]+\\)", "", x)
get_alpha <- function(x) gsub("[[:digit:]]", "", x)
get_num <- function(x) gsub("[[:alpha:]]", "", x) |> as.numeric()

# visit days generator

gen_visit_days <- function(var, sdate, scn_days, size) {
  var <- as.character(var)
  sdate <- as.Date(trtstdt)
  sdate <- rep(as.Date(sdate), each = length(unique(var)))
  # scn_days <- scn_days |> as.numeric()

  # scn_d <- sample(1:length(var), size = length(var), replace = T)
  #
  period <- get_alpha(var)
  pnum <- get_num(var)
  is8601_dt <- paste0(
    "P", ifelse(is.na(pnum), scn_days, pnum),
    ifelse(period == "SCN", "D", period)
  )

  ifelse(
    var == "SCN",
    ((sdate - scn_days) |> origin_date()),
    ((sdate + duration(is8601_dt)) |> origin_date())
  ) |> origin_date()
}

get_fcode <- function(x) paste0(substr(x, 1, 1), get_num(x)) # folder code

get_stats <- function(data) {
  min <- data |> min(na.rm = T)
  max <- data |> max(na.rm = T)
  mean <- data |> mean(na.rm = T)
  sd <- data |> sd(na.rm = T)

  cat("start=", min, ",", "end=", max, ",", "mean=", mean, ",", "sd=", sd)
}

num_to_char <- function(x, digits = 1) round(x, digits = digits) %>% as.character()
num_to_round <- function(x, digits = 1) round(x, digits = digits)
lb_high <- function(x) (mean(x) + sd(x)) |> num_to_round()
lb_low <- function(x) (mean(x) - sd(x)) |> num_to_round()


high_low <- function(var, high, low) {
  ifelse(var < low, "-", ifelse(var > high, "+", "0"))
}

