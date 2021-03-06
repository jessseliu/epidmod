% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hist.R
\name{hist.rSEIQR}
\alias{hist.rSEIQR}
\title{S3 histogram for stochastic SEIQR model}
\usage{
\method{hist}{rSEIQR}(x, y, nsim = 100)
}
\arguments{
\item{x}{the output from simulation}

\item{y}{the input a certain time}

\item{nsim}{number of simulation to produce the histogram}
}
\description{
\code{hist} returns histogram for the number of each group given a certain time
}
\examples{
model1 <- rSEIQR(N0 = 100, S0 = 99, E0 = 1 ,I0 = 0, Q0 = 0, R0 = 0,
                 days = 100,  pars = c(1/12, 1, 0.5, 0.15, 0.04, 0, 0, 1))
hist( model1, 81, 100)

}
