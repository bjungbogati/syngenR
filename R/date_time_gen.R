#' Convert Numeric Date to Date Object
#'
#' Converts a numeric value representing the number of days since January 1, 1970, to a Date object.
#'
#' @param x A numeric value or vector representing the number of days since January 1, 1970.
#'
#' @return A Date object or vector of Date objects.
#' @export
#'
#' @examples
#' date_origin(0)  # Converts to "1970-01-01"
#' date_origin(18500)  # Converts to "2020-09-18"
date_origin <- function(x) {
  as.Date(x, origin = "1970-01-01")
}

#' Generate Random Dates
#'
#' Generates a vector of random dates within a specified range.
#'
#' @param start The start date as a character string in "YYYY-MM-DD" format.
#' @param end The end date as a character string in "YYYY-MM-DD" format.
#' @param size The number of random dates to generate.
#'
#' @return A vector of random Date objects.
#' @export
#'
#' @examples
#' date_gen("2022-01-01", "2022-12-31", 10)  # Generates 10 random dates in 2022
date_gen <- function(start, end, size) {
  sample(
    x = seq(
      from = as.Date(start),
      to = as.Date(end),
      by = "day"
    ),
    size = size,
    replace = TRUE
  )
}

#' Calculate Birth Year
#'
#' Calculates the birth year given an age and a reference year.
#'
#' @param x A numeric value or vector representing age(s).
#' @param y A Date object or vector representing the reference date(s).
#'
#' @return A numeric value or vector representing the birth year(s).
#' @export
#'
#' @examples
#' birth_year(25, as.Date("2023-01-01"))  # Returns 1998
#' birth_year(c(25, 30), as.Date("2023-01-01"))  # Returns c(1998, 1993)
birth_year <- function(x, y) {
  y |>
    format(format = "%Y") |>
    as.integer() - x
}

#' Generate Birth Dates
#'
#' Generates a vector of birth dates based on ages, reference date, and size.
#'
#' @param x A numeric value or vector representing age(s).
#' @param y A Date object representing the reference date.
#' @param size The number of birth dates to generate.
#'
#' @return A vector of Date objects representing the birth dates.
#' @export
#'
#' @examples
#' birth_date(25, as.Date("2023-01-01"), 5)  # Generates 5 birth dates for 25-year-olds
birth_date <- function(x, y, size) {
  dat <- paste(birth_year(x, y),
               sample(1:12, size, TRUE),
               sample(1:31, size, TRUE),
               sep = "-"
  ) |> as.Date()

  return(dat)
}

#' Extract Year from Date
#'
#' Extracts the year from a Date object.
#'
#' @param x A Date object or vector of Date objects.
#'
#' @return A numeric value or vector representing the year(s).
#' @export
#'
#' @examples
#' yyyy(as.Date("2023-01-01"))  # Returns 2023
yyyy <- function(x) {
  format(x, "%Y") |> as.numeric()
}

#' Extract Month from Date
#'
#' Extracts the month from a Date object.
#'
#' @param x A Date object or vector of Date objects.
#'
#' @return A numeric value or vector representing the month(s).
#' @export
#'
#' @examples
#' mm(as.Date("2023-01-01"))  # Returns 1
mm <- function(x) {
  format(x, "%m") |> as.numeric()
}

#' Extract Day from Date
#'
#' Extracts the day from a Date object.
#'
#' @param x A Date object or vector of Date objects.
#'
#' @return A numeric value or vector representing the day(s).
#' @export
#'
#' @examples
#' dd(as.Date("2023-01-01"))  # Returns 1
dd <- function(x) {
  format(x, "%d") |> as.numeric()
}

#' Generate Random Date-Time Range
#'
#' Generates a vector of random date-times within a specified range.
#'
#' @param start The start date-time as a character string in "YYYY-MM-DD HH:MM:SS" format.
#' @param end The end date-time as a character string in "YYYY-MM-DD HH:MM:SS" format.
#' @param by The increment of the sequence (e.g., "1 day", "1 hour").
#'
#' @return A vector of random POSIXct date-time objects.
#' @export
#'
#' @examples
#' gen_date_range("2023-01-01 00:00:00", "2023-01-02 00:00:00", "1 hour")
#' # Generates random date-times every hour from Jan 1 to Jan 2, 2023
gen_date_range <- function(start, end, by) {
  sample(seq(as.POSIXct(start), as.POSIXct(end), by = by), size = size, replace = TRUE)
}

#' Generate Random Date-Time Within Hours
#'
#' Generates random date-times within a specific time range on a given date.
#'
#' @param date A vector of POSIXct date-time objects.
#' @param stime The start time as an integer (hour in 24-hour format).
#' @param etime The end time as an integer (hour in 24-hour format).
#' @param size The number of date-times to generate.
#' @param min A vector of minute increments for the date-times.
#' @param replace A logical value indicating whether to allow replacement.
#'
#' @return A vector of POSIXct date-time objects.
#' @export
#'
#' @examples
#' gen_date_time(Sys.Date(), 9, 17, 10, c(0, 15, 30, 45), TRUE)
#' # Generates 10 random date-times between 9:00 and 17:00
gen_date_time <- function(date, stime, etime, size, min, replace) {
  sample(
    date[lubridate::hour(date) > stime & lubridate::hour(date) < etime], size,
    replace = replace
  )
}

#' Generate Random Date-Time
#'
#' Generates random date-times around the current date.
#'
#' @param date The base date as a Date object. Defaults to the current date.
#'
#' @return A vector of POSIXct date-time objects.
#' @export
#'
#' @examples
#' gen_dt()  # Generates 1000 random date-times around the current date
gen_dt <- function(date = Sys.Date()) {
  paste0(
    date, " ", sample(9:15, 1000, replace = TRUE), ":",
    sample(seq(0, 59, by = 15), 1000, replace = TRUE)
  ) |> as.POSIXct()
}

#' Generate Date-Time After Given Date
#'
#' Generates random date-times after a given date and time.
#'
#' @param date A POSIXct date-time object.
#' @param range A numeric vector representing minute ranges for generating times.
#' @param size The number of date-times to generate.
#' @param replace A logical value indicating whether to allow replacement.
#'
#' @return A vector of POSIXct date-time objects.
#' @export
#'
#' @examples
#' gen_after_dt(Sys.time(), c(10, 20, 30), 5, TRUE)
#' # Generates 5 date-times 10, 20, or 30 minutes after the current time
gen_after_dt <- function(date, range, size, replace) {
  date +
    lubridate::minutes(sample(range, size, replace = TRUE))
}

#' Generate Date-Time Days After Given Date
#'
#' Generates random date-times a specific number of days after a given date.
#'
#' @param date A POSIXct date-time object.
#' @param days A numeric vector representing the number of days after the base date.
#' @param range A numeric vector representing minute ranges for generating times.
#' @param size The number of date-times to generate.
#' @param replace A logical value indicating whether to allow replacement.
#'
#' @return A vector of POSIXct date-time objects.
#' @export
#'
#' @examples
#' gen_day_after_dt(Sys.time(), c(1, 2, 3), c(10, 20, 30), 5, TRUE)
#' # Generates 5 date-times 1, 2, or 3 days and 10, 20, or 30 minutes after the current time
gen_day_after_dt <- function(date, days, range, size, replace) {
  date +
    lubridate::days(sample(days, size, replace = TRUE)) +
    lubridate::minutes(sample(range, size, replace = TRUE))
}
