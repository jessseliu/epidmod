

#'New S3 method for gathering information from stochastic SIR model at a given infection rate
#'
#' \code{IR_info} returns the simulated information at a given infection rate for SIR model
#' @param x the output from simulation
#' @param y the input infection rate
#' @examples
#' model1 <- rSIR (N0 = 1000, I0 = 0, S0 = 1000, days = 300, pars = c(1/10, 1, 1/5, 0.1, 0.1))
#' IR_info(model1, 0.7)
#'
#'@export
IR_info <- function(x,y,...) {
  UseMethod("IR_info")
}

#'@export
IR_info.rSIR <- function(x,y,...){

  # store the simulation value into vector
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people

  # calculate the infection rate
  rate <- i/n

  # determine the position of given rate
  l <- order(abs(rate- y))[1]
  if(max(rate) < y){ stop("Please enter an effective infection rate ") }
  cat(" Infection Information")
  cat("\n-----------------------\n")
  cat(" Time needed to reach the given infection rate = ", t[l])
  cat("\n Number of susceptible people at given infection rate = ", s[l])
  cat("\n Number of infected people at given infection rate = ", i[l])
  cat("\n Number of recovered people at given infection rate = ", r[l])
  cat("\n Total population at given infection rate = ", n[l])
  cat("\n")

}

#'New  S3 method for gathering information from stochastic SEIR model at a given infection rate
#'
#' \code{IR_info} returns the simulated information at a given infection rate for SEIR model
#' @param x the output from simulation
#' @param y the input infection rate
#' @examples
#' model1 <- rSEIR(N0 = 1000, I0 = 0, S0 = 999, R0 = 0, E0 = 1 , days = 300, pars = c(1/12, 1, 1/4, 1/5, 0.3, 0.2, 0.7))
#' IR_info(model1, 0.3)
#'
#'@export
IR_info.rSEIR <- function(x,y,...){

  # store the simulation value into vector
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people

  # calculate the infection rate
  rate <- i/n

  # determine the position of given rate
  l <- order(abs(rate- y))[1]
  if(max(rate) < y){ stop("Please enter an effective infection rate ") }
  cat(" Infection Information")
  cat("\n-----------------------\n")
  cat(" Time needed to reach the given infection rate = ", t[l])
  cat("\n Number of susceptible people at given infection rate = ", s[l])
  cat("\n Number of exposed people at given infection rate = ", e[l])
  cat("\n Number of infected people at given infection rate = ", i[l])
  cat("\n Number of recovered people at given infection rate = ", r[l])
  cat("\n Total population at given infection rate = ", n[l])
  cat("\n")

}


#'New  S3 method for gathering information from stochastic SEIQHRF model at a given infection rate
#'
#' \code{IR_info} returns the simulated information at a given infection rate for SEIQHRF model
#' @param x the output from simulation
#' @param y the input infection rate
#' @examples
#' para <- c(4,2,1,2,1,3,1,2,1,2,0.9, 0.3, 0.4,0.1, 0.1)
#' model1 <- rSEIQHRF(N0 = 1000, S0 = 999, E0 = 1, I0 = 0, Q0 = 0, H0 = 0, R0 = 0, F0 = 0, days = 300, pars = para )
#' IR_info(model1, 0.1)
#'
#'
IR_info.rSEIQHRF <- function(x,y,...){

  # store the simulation value into vector
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  q <- x$Quarantined_people
  h <- x$Hostipalization_people
  r <- x$Immune_people
  f <- x$Fatality_case
  n <- x$Total_people

  # calculate the infection rate
  rate <- i/n

  # determine the position of given rate
  l <- order(abs(rate- y))[1]
  if(max(rate) < y){ stop("Please enter an effective infection rate ") }
  cat(" Infection Information")
  cat("\n-----------------------\n")
  cat(" Time needed to reach the given infection rate = ", t[l])
  cat("\n Number of susceptible people at given infection rate = ", s[l])
  cat("\n Number of exposed people at given infection rate = ", e[l])
  cat("\n Number of infected people at given infection rate = ", i[l])
  cat("\n Number of self-quarantined people at given infection rate = ", q[l])
  cat("\n Number of people require hospitalization at given infection rate = ", h[l])
  cat("\n Number of recovered people at given infection rate = ", r[l])
  cat("\n Number of exposed people at given infection rate = ", f[l])
  cat("\n Total population at given infection rate = ", n[l])
  cat("\n")

}

