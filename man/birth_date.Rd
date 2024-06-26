% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_time_gen.R
\name{birth_date}
\alias{birth_date}
\title{Generate Birth Dates}
\usage{
birth_date(x, y, size)
}
\arguments{
\item{x}{A numeric value or vector representing age(s).}

\item{y}{A Date object representing the reference date.}

\item{size}{The number of birth dates to generate.}
}
\value{
A vector of Date objects representing the birth dates.
}
\description{
Generates a vector of birth dates based on ages, reference date, and size.
}
\examples{
birth_date(25, as.Date("2023-01-01"), 5)  # Generates 5 birth dates for 25-year-olds
}
