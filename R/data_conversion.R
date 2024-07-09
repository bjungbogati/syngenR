#' Convert Data to Character
#'
#' Converts any given data to a character vector.
#'
#' @param data The input data to be converted to a character vector.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' get_char(1)  # Converts the number 1 to the character "1"
#' get_char(c(1, 2, 3))  # Converts a numeric vector to a character vector
get_char <- function(data) {
  data |>
    unlist() |>
    as.character()
}

#' Convert Data to Numeric
#'
#' Converts any given data to a numeric vector.
#'
#' @param data The input data to be converted to a numeric vector.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' get_num("1")  # Converts the character "1" to the numeric 1
#' get_num(c("1", "2", "3"))  # Converts a character vector to a numeric vector
get_num <- function(data) {
  data |>
    unlist() |>
    as.numeric()
}

#' Convert Date Columns to Character
#'
#' Converts all Date and POSIXt columns in a dataframe to character columns.
#'
#' @param df A data frame containing Date or POSIXt columns.
#'
#' @return A data frame with Date and POSIXt columns converted to character columns.
#' @export
#'
#' @examples
#' df <- data.frame(date_col = as.Date("2021-01-01"), num_col = 1:3)
#' date_to_char(df)  # Converts the date column to a character column
date_to_char <- function(df) {
  indf <- sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))
  df[indf] <- lapply(df[indf], as.character)
  df
}

#' Convert Numeric to Character with Rounding
#'
#' Converts a numeric vector to a character vector, rounding each value to a specified number of digits.
#'
#' @param x A numeric vector.
#' @param digits The number of decimal places to round to (default is 1).
#'
#' @return A character vector with rounded numeric values.
#' @export
#'
#' @examples
#' num_to_char(3.14159)  # Converts to "3.1"
#' num_to_char(c(3.14159, 2.71828), digits = 2)  # Converts to "3.14" and "2.72"
num_to_char <- function(x, digits = 1) round(x, digits = digits) |> as.character()

#' Round Numeric Values
#'
#' Rounds numeric values to a specified number of decimal places.
#'
#' @param x A numeric vector.
#' @param digits The number of decimal places to round to (default is 1).
#'
#' @return A numeric vector with rounded values.
#' @export
#'
#' @examples
#' num_to_round(3.14159)  # Rounds to 3.1
#' num_to_round(c(3.14159, 2.71828), digits = 2)  # Rounds to 3.14 and 2.72
num_to_round <- function(x, digits = 1) round(x, digits = digits)

#' Convert Environment Variables to Data Frame
#'
#' Converts environment variables into a data frame.
#'
#' @param envs A named list of environment variables.
#'
#' @return A data frame containing the environment variables.
#' @export
#'
#' @examples
#' env_vars <- list(my.env.var1 = 10, my.env.var2 = 20)
#' env_to_df(env_vars)  # Converts the environment variables to a data frame
env_to_df <- function(envs) {
  df <- data.frame(envs)
  name <- sub(".*\\.", "", names(df))
  names(df) <- name
  df$size <- NULL
  return(df)
}

#' Remove Digits from String
#'
#' Removes all numeric characters from a string.
#'
#' @param x A character vector.
#'
#' @return A character vector with all numeric characters removed.
#' @export
#'
#' @examples
#' get_alpha("abc123")  # Returns "abc"
#' get_alpha("R2D2")  # Returns "RD"
get_alpha <- function(x) gsub("[[:digit:]]", "", x)

#' Extract Numbers from String
#'
#' Extracts all numeric characters from a string and converts them to a numeric value.
#'
#' @param x A character vector.
#'
#' @return A numeric vector with all numeric characters extracted and converted.
#' @export
#'
#' @examples
#' get_num("abc123")  # Returns 123
#' get_num("R2D2")  # Returns 22
get_num <- function(x) gsub("[[:alpha:]]", "", x) |> as.numeric()

#' Convert Celsius to Fahrenheit
#'
#' Converts a numeric vector representing temperatures in Celsius to Fahrenheit.
#'
#' @param x A numeric vector of temperatures in Celsius.
#' @param digits The number of decimal places to include in the output (default is 1).
#'
#' @return A character vector of temperatures in Fahrenheit, formatted to the specified number of decimal places.
#' @export
#'
#' @examples
#' cel_to_fhr(0, digits=1)  # Converts 0°C to "32.0"
#' cel_to_fhr(100, digits = 2)  # Converts 100°C to "212.00"
cel_to_fhr <- function(x, digits) formatC(x * 1.8 + 32, digits = digits, format = "f")
