% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.rSEIQR}
\alias{print.rSEIQR}
\title{S3 print method for stochastic SEIQR model}
\usage{
\method{print}{rSEIQR}(x)
}
\arguments{
\item{x}{the output from simulation}
}
\description{
\code{print} returns the basic information of simulated SEIQR model
}
\examples{
model1 <- rSEIQR(N0 = 100, S0 = 99, E0 = 1 ,I0 = 0,  Q0 = 0, R0 = 0,
                 days = 100,  pars = c(1/12, 1, 1/4, 0.15, 1/5, 0.3, 0.2, 0.7))
print(model1)

}
