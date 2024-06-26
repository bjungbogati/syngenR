% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{lb_low}
\alias{lb_low}
\title{Compute Lower Bound}
\usage{
lb_low(x)
}
\arguments{
\item{x}{A numeric vector.}
}
\value{
The computed lower bound.
}
\description{
Computes the lower bound as mean minus one standard deviation.
}
\examples{
lb_low(c(1, 2, 3, 4, 5))
}
