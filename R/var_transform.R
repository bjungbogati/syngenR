
#' Generate Year, Month, and Day from Date
#'
#' Extracts the year, month, and day from a given date and returns a data frame.
#'
#' @param dat A vector of dates.
#'
#' @return A data frame with year, month, and day columns.
#' @export
#'
#' @examples
#' ymd_gen(Sys.Date())
ymd_gen <- function(dat, time_off= "Y") {
  dfname <- deparse(substitute(dat))
  name <- sub("_.*", "", dfname)

  year <- yyyy(dat)
  month <- mm(dat)
  day <- dd(dat)

  year_raw <- ifelse(is.na(year), NA_character_, as.character(year))
  month_raw <- ifelse(is.na(month), NA_character_, as.character(month))
  day_raw <- ifelse(is.na(day), NA_character_, as.character(day))

  df <- data.frame(year, month, day, year_raw, month_raw, day_raw)

  if(time_off == "N") {
    hour_min <-  hh_mm(dat)
    hour_min_raw <- ifelse(is.na(hour_min), NA_character_, as.character(hour_min))
    df <- data.frame(year, month, day, year_raw, month_raw, day_raw, hour_min_raw)

  }

  names(df) <- paste(name, names(df), sep = "_")
  return(df)
}

#' Categorize Values as High, Low, or Normal
#'
#' Categorizes values as high, low, or normal based on provided thresholds.
#'
#' @param var A numeric vector to categorize.
#' @param high The threshold for high values.
#' @param low The threshold for low values.
#'
#' @return A character vector with categories ("+", "-", "0").
#' @export
#'
#' @examples
#' high_low(c(1, 5, 10), 8, 3)
high_low <- function(var, high, low) {
  ifelse(var < low, "-", ifelse(var > high, "+", "0"))
}

#' Generate Folder Data
#'
#' Generates a data frame with folder information and identifiers.
#'
#' @param crfpagename The name of the CRF page.
#' @param foldername The name of the folder.
#' @param folder The folder identifier.
#'
#' @return A data frame with folder and identifier information.
#' @export
#'
#' @examples
#' gen_folder("CRFPage1", "Folder1", "F1")
gen_folder <- function(crfpagename, foldername, folder, size=size) {
  crfpagename <- crfpagename
  foldername <- foldername
  folder <- folder
  crfpageid <- 1:size
  recordid <- 1:size
  recordposition <- prob_gen(c(1, 0), size = size, prob = c(1, 0))

  df <- env_to_df(mget(ls()))
  return(df)
}

#' Generate Visit Days
#'
#' Generates visit days based on screening dates and specified day ranges.
#'
#' @param var A character vector indicating visit types.
#' @param sdate The start date for visits.
#' @param scn_days The number of days from the start date for screening visits.
#' @param size The number of visits to generate.
#'
#' @return A vector of visit dates.
#' @export
#'
#' @examples
#' gen_visit_days(c("SCN", "FUP"), Sys.Date(), 7, 10)
gen_visit_days <- function(var, sdate, scn_days, size) {
  var <- as.character(var)
  sdate <- as.Date(sdate)
  sdate <- rep(as.Date(sdate), each = length(unique(var)))
  period <- get_alpha(var)
  pnum <- get_num(var)
  is8601_dt <- paste0(
    "P", ifelse(is.na(pnum), scn_days, pnum),
    ifelse(period == "SCN", "D", period)
  )

  ifelse(
    var == "SCN",
    ((sdate - scn_days) |> date_origin()),
    ((sdate + lubridate::duration(is8601_dt)) |> date_origin())
  ) |> date_origin()
}

#' Generate First Character Code
#'
#' Generates a code from the first character and numeric part of a string.
#'
#' @param x A character vector to process.
#'
#' @return A character vector with the generated codes.
#' @export
#'
#' @examples
#' get_fcode(c("A123", "B456"))
get_fcode <- function(x) paste0(substr(x, 1, 1), get_num(x))

#' Remove Text Inside Parentheses
#'
#' Removes any text within parentheses in a string.
#'
#' @param x A character vector to process.
#'
#' @return A character vector with text inside parentheses removed.
#' @export
#'
#' @examples
#' no_brac_txt("Example (text) string")
no_brac_txt <- function(x) gsub("\\s*\\([^\\)]+\\)", "", x)
