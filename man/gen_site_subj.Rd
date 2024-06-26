% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random_sampling.R
\name{gen_site_subj}
\alias{gen_site_subj}
\title{Generate Site Subject Identifiers}
\usage{
gen_site_subj(width = 4)
}
\arguments{
\item{width}{The width of the numerical part of the identifier (default is 4).}
}
\value{
A data frame with site list, site group, and subject identifiers.
}
\description{
Generates unique subject identifiers for each site.
}
\examples{
gen_site_subj()
}
