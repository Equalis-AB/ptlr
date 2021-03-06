% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Algorithm_A.R
\name{Algorithm_A}
\alias{Algorithm_A}
\title{Robust Mean and Standard Deviation}
\usage{
Algorithm_A(x)
}
\arguments{
\item{x}{numerical vector}
}
\value{
A list with robust mean and sd.
\itemize{
\item robust_mean - the robust mean of x.
\item robust_sd - the robust standard deviation of x.
}
}
\description{
This function calculates robust mean and sd with 'Algorithm A' described in
ISO 13528-2015, section C.3.1
}
\examples{
Algorithm_A(c(0.2640, 0.2670, 0.2960 ,0.3110, 0.3310, 0.4246))
}
