#' Repeat Elements Randomly
#'
#' Generates a sorted vector by repeating elements from a given set a random number of times up to a specified maximum.
#'
#' @param max_size The maximum number of repetitions for each element.
#' @param subj A vector of subjects from which elements are selected.
#' @param size The number of times to sample.
#'
#' @return A sorted vector with repeated elements.
#' @export
#'
#' @examples
#' rep_each(5, c("A", "B", "C"), 10)
rep_each <- function(max_size, subj, size = size) {
  rep_sum <- sample(0:max_size, size = size, replace = TRUE) |> sum()
  sample(subj, size = rep_sum, replace = TRUE) |> sort()
}

#' Generate List Elements
#'
#' Generates a sample list of elements from a given list with or without replacement.
#'
#' @param list A list of elements to sample from.
#' @param size The number of elements to sample.
#' @param replace Logical; if TRUE, sampling is with replacement.
#'
#' @return A sampled list of elements.
#' @export
#'
#' @examples
#' list_gen(c("apple", "banana", "cherry"), 5)
list_gen <- function(list, size, replace = TRUE) {
  sample(list, size = size, replace = replace)
}

#' Generate List Elements with Probabilities
#'
#' Generates a sample list of elements from a given list with specified probabilities.
#'
#' @param list A list of elements to sample from.
#' @param size The number of elements to sample.
#' @param prob A vector of probabilities for each element.
#' @param replace Logical; if TRUE, sampling is with replacement.
#'
#' @return A sampled list of elements.
#' @export
#'
#' @examples
#' prob_gen(c("A", "B", "C"), 10, c(0.2, 0.5, 0.3))
prob_gen <- function(list, size, prob, replace = TRUE) {
  sample(list, size = size, replace = replace, prob = prob)
}

#' Generate Numeric Data
#'
#' Generates a data frame with numeric values following a normal distribution.
#'
#' @param start The start of the range for generating random numbers.
#' @param end The end of the range for generating random numbers.
#' @param mean The mean of the normal distribution.
#' @param sd The standard deviation of the normal distribution.
#' @param size The number of samples to generate.
#' @param list A list used to name the output columns.
#' @param unique Logical; if TRUE, ensures the samples are unique.
#'
#' @return A data frame with numeric values.
#' @export
#'
#' @examples
#' num_gen(1, 100, 50, 10, 10, "num_list")
num_gen <- function(start, end, mean = 40, sd = 12, size, list, unique = TRUE) {
  val <- rnorm(start:end, mean = mean, sd = sd) |>
    as.integer() |>
    sample(size = size, replace = unique)

  val_char <- as.character(val)

  dfname <- deparse(substitute(list))
  name <- sub("_.*", "", dfname)
  raw <- paste(name, "raw", sep = "_")
  df <- data.frame(val, val_char)
  names(df) <- c(name, raw)
  return(df)
}

#' Generate Bell-Shaped Numeric Data
#'
#' Generates numeric data following a bell curve (normal distribution).
#'
#' @param start The start of the range for generating random numbers.
#' @param end The end of the range for generating random numbers.
#' @param mean The mean of the normal distribution.
#' @param sd The standard deviation of the normal distribution.
#' @param size The number of samples to generate.
#' @param unique Logical; if TRUE, ensures the samples are unique.
#'
#' @return A vector of bell-shaped numeric data.
#' @export
#'
#' @examples
#' bell_gen(1, 100, 50, 10, 10)
bell_gen <- function(start, end, mean, sd, size, unique = TRUE) {
  rnorm(start:end, mean = mean, sd = sd) |>
    as.integer() |>
    sample(size = size, replace = unique) |>
    abs()
}

#' Generate Bell-Shaped Floating Point Data
#'
#' Generates floating-point data following a bell curve (normal distribution).
#'
#' @param start The start of the range for generating random numbers.
#' @param end The end of the range for generating random numbers.
#' @param mean The mean of the normal distribution.
#' @param sd The standard deviation of the normal distribution.
#' @param size The number of samples to generate.
#' @param unique Logical; if TRUE, ensures the samples are unique.
#' @param digits The number of decimal places to retain.
#'
#' @return A vector of bell-shaped floating-point data.
#' @export
#'
#' @examples
#' float_bell_gen(1, 100, 50, 10, 10, 2)
float_bell_gen <- function(start, end, mean = 40, sd = 10, size, unique = TRUE, digits = 1) {
  rnorm(start:end, mean = mean, sd = sd) |>
    sample(size = size, replace = unique) |>
    formatC(digits = digits, format = "f") |>
    as.numeric() |>
    abs()
}

#' Standardize Data Generation
#'
#' Generates standardized codes and decodes based on the given list.
#'
#' @param list A list containing 'value' and 'factor' elements for generating data.
#'
#' @return A data frame with standardized codes and decodes.
#' @export
#'
#' @examples
#' std_gen(list(value = c("A", "B", "C"), factor = c(0.3, 0.4, 0.3)))
std_gen <- function(list){
  dfname <- deparse(substitute(list))

  code <- prob_gen(list = list$value, size = get_size(), prob = list$factor)
  decode <- factor(code, levels = list$value) |> as.integer()

  name <- sub("_.*", "", dfname)
  std <- paste(name, "std", sep = "_")
  df <- data.frame(code, decode)
  names(df) <- c(name, std)
  return(df)
}

#' Generate Site Subject Identifiers
#'
#' Generates unique subject identifiers for each site.
#'
#' @param width The width of the numerical part of the identifier (default is 4).
#'
#' @return A data frame with site list, site group, and subject identifiers.
#' @export
#'
#' @examples
#' gen_site_subj()
gen_site_subj <- function(width = 4) {
  project <- project
  projectid <- projectid
  site_list <- list_gen(site_fmt, size) |> sort()
  site_group <- siteinit + factor(site_list, levels = site_fmt) |> as.integer() |> sort()
  group_seq <- stats::ave(site_group, site_list, FUN = seq_along)

  subject <- paste(site_group, formatC(group_seq, width = width, flag = "0"), sep = "-") |> sort()

  df <- data.frame(projectid, project, site_list, site_group, subject)
  return(df)
}
