% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_conversion.R
\name{get_num}
\alias{get_num}
\title{Convert Data to Numeric}
\usage{
get_num(x)

get_num(x)
}
\arguments{
\item{x}{A character vector.}

\item{data}{The input data to be converted to a numeric vector.}
}
\value{
A numeric vector.

A numeric vector with all numeric characters extracted and converted.
}
\description{
Converts any given data to a numeric vector.

Extracts all numeric characters from a string and converts them to a numeric value.
}
\examples{
get_num("1")  # Converts the character "1" to the numeric 1
get_num(c("1", "2", "3"))  # Converts a character vector to a numeric vector
get_num("abc123")  # Returns 123
get_num("R2D2")  # Returns 22
}
