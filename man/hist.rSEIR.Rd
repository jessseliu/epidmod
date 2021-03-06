% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hist.R
\name{hist.rSEIR}
\alias{hist.rSEIR}
\title{S3 histogram for stochastic SEIR model}
\usage{
\method{hist}{rSEIR}(x, y, nsim = 100)
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
model1 <- rSEIR(N0 = 1000, I0 = 0, S0 = 999, R0 = 0, E0 = 1,
                days = 300, pars = c(1/12, 1, 1/4, 1/5, 0.3, 0.2, 0.7))
hist( model1, 81, 100)

}
