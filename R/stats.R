
#' Get Summary Statistics
#'
#' Computes and prints summary statistics for a numeric vector.
#'
#' @param data A numeric vector to analyze.
#'
#' @return None. Prints the summary statistics.
#' @export
#'
#' @examples
#' get_stats(c(1, 2, 3, 4, 5))
get_stats <- function(data) {
  min <- data |> min(na.rm = TRUE)
  max <- data |> max(na.rm = TRUE)
  mean <- data |> mean(na.rm = TRUE)
  sd <- data |> sd(na.rm = TRUE)

  cat("start=", min, ",", "end=", max, ",", "mean=", mean, ",", "sd=", sd)
}

#' Compute Upper Bound
#'
#' Computes the upper bound as mean plus one standard deviation.
#'
#' @param x A numeric vector.
#'
#' @return The computed upper bound.
#' @export
#'
#' @examples
#' lb_high(c(1, 2, 3, 4, 5))
lb_high <- function(x) (mean(x) + sd(x)) |> num_to_round()

#' Compute Lower Bound
#'
#' Computes the lower bound as mean minus one standard deviation.
#'
#' @param x A numeric vector.
#'
#' @return The computed lower bound.
#' @export
#'
#' @examples
#' lb_low(c(1, 2, 3, 4, 5))
lb_low <- function(x) (mean(x) - sd(x)) |> num_to_round()
