% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/afterpeak_info.R
\name{afterpeak_info}
\alias{afterpeak_info}
\title{New S3 method for gathering information from stochastic SIR model after the peak}
\usage{
afterpeak_info(x)
}
\arguments{
\item{x}{the output from simulation}
}
\value{
A information list
}
\description{
\code{afterpeak_info} returns the peak information and time of infected reducing to certain level after the peak
}
\examples{
model1 <- rSIR (N0 = 1000, I0 = 0, S0 = 1000, days = 300, pars = c(1/10, 1, 1/5, 0.1, 0.1))
afterpeak_info(model1)

}
