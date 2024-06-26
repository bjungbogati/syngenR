% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_conversion.R
\name{date_to_char}
\alias{date_to_char}
\title{Convert Date Columns to Character}
\usage{
date_to_char(df)
}
\arguments{
\item{df}{A data frame containing Date or POSIXt columns.}
}
\value{
A data frame with Date and POSIXt columns converted to character columns.
}
\description{
Converts all Date and POSIXt columns in a dataframe to character columns.
}
\examples{
df <- data.frame(date_col = as.Date("2021-01-01"), num_col = 1:3)
date_to_char(df)  # Converts the date column to a character column
}
