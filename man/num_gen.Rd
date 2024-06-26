% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random_sampling.R
\name{num_gen}
\alias{num_gen}
\title{Generate Numeric Data}
\usage{
num_gen(start, end, mean = 40, sd = 12, size, list, unique = TRUE)
}
\arguments{
\item{start}{The start of the range for generating random numbers.}

\item{end}{The end of the range for generating random numbers.}

\item{mean}{The mean of the normal distribution.}

\item{sd}{The standard deviation of the normal distribution.}

\item{size}{The number of samples to generate.}

\item{list}{A list used to name the output columns.}

\item{unique}{Logical; if TRUE, ensures the samples are unique.}
}
\value{
A data frame with numeric values.
}
\description{
Generates a data frame with numeric values following a normal distribution.
}
\examples{
num_gen(1, 100, 50, 10, 10, "num_list")
}
