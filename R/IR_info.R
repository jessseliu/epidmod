

#'New S3 method for gathering information from stochastic SIR model at a given infected proportion
#'
#' \code{IR_info} returns the simulated information at a given infected proportionfor SIR model
#' @param x the output from simulation
#' @param y the input infected proportion
#' @examples
#' model1 <- rSIR (N0 = 1000, I0 = 0, S0 = 1000, days = 300, pars = c(1/10, 1, 1/5, 0.1, 0.1))
#' IR_info(model1, 0.7)
#'
#'@export
IR_info <- function(x,y) {
  UseMethod("IR_info")
}

#'@export
IR_info.rSIR <- function(x,y){

  # store the simulation value into vector
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people

  # calculate the infected proportion
  rate <- i/n

  # determine the position of given rate
  l <- order(abs(rate- y))[1]
  if(max(rate) < y){ stop("Please enter an effective infected proportion ") }
  cat(" Infection Information")
  cat("\n-----------------------\n")
  cat(" Time needed to reach the given infected proportion = ", t[l])
  cat("\n Number of susceptible people at given infected proportion = ", s[l])
  cat("\n Number of infected people at given infected proportion = ", i[l])
  cat("\n Number of recovered people at given infected proportion = ", r[l])
  cat("\n Total population at given infected proportion = ", n[l])
  cat("\n")

}

#'New  S3 method for gathering information from stochastic SEIR model at a given infected proportion
#'
#' \code{IR_info} returns the simulated information at a given infected proportion for SEIR model
#' @param x the output from simulation
#' @param y the input infected proportion
#' @examples
#' model1 <- rSEIR(N0 = 1000, I0 = 0, S0 = 999, R0 = 0, E0 = 1 , days = 300, pars = c(1/12, 1, 1/4, 1/5, 0.3, 0.2, 0.7))
#' IR_info(model1, 0.3)
#'
#'@export
IR_info.rSEIR <- function(x,y){

  # store the simulation value into vector
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people

  # calculate the infected proportion
  rate <- i/n

  # determine the position of given infected proportion
  l <- order(abs(rate- y))[1]
  if(max(rate) < y){ stop("Please enter an effective infected proportion ") }
  cat(" Infection Information")
  cat("\n-----------------------\n")
  cat(" Time needed to reach the given infected proportion = ", t[l])
  cat("\n Number of susceptible people at given infected proportion = ", s[l])
  cat("\n Number of exposed people at given infected proportion= ", e[l])
  cat("\n Number of infected people at given infected proportion = ", i[l])
  cat("\n Number of recovered people at given infected proportion = ", r[l])
  cat("\n Total population at given infected proportion = ", n[l])
  cat("\n")

}

#'New  S3 method for gathering information from stochastic SEIQR model at a given infected proportion
#'
#' \code{IR_info} returns the simulated information at a given infected proportion for SEIQR model
#' @param x the output from simulation
#' @param y the input infected proportion
#' @examples
#' model1 <- rSEIQR(N0 = 100, S0 = 99, E0 = 1 ,I0 = 0, Q0 = 0, R0 = 0,  days = 100,  pars = c(1/12, 1, 0.5, 0.15, 0.04, 0, 0, 1))
#' IR_info(model1, 0.3)
#'
#'@export
IR_info.rSEIR <- function(x,y){

  # store the simulation value into vector
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  q <- x$Quarantined_people
  r <- x$Immune_people
  n <- x$Total_people

  # calculate the infected proportion
  rate <- i/n

  # determine the position of given infected proportion
  l <- order(abs(rate- y))[1]
  if(max(rate) < y){ stop("Please enter an effective infected proportion ") }
  cat(" Infection Information")
  cat("\n-----------------------\n")
  cat(" Time needed to reach the given infected proportion = ", t[l])
  cat("\n Number of susceptible people at given infected proportion = ", s[l])
  cat("\n Number of exposed people at given infected proportion= ", e[l])
  cat("\n Number of infected people at given infected proportion = ", i[l])
  cat("\n Number of self-quarantined people at given infected proportion = ", q[l])
  cat("\n Number of recovered people at given infected proportion = ", r[l])
  cat("\n Total population at given infected proportion = ", n[l])
  cat("\n")

}


#' New S3 method for gathering information from stochastic SEIQHRF model at a given infected proportion
#'
#' \code{IR_info} returns the simulated information at a given infected proportion for SEIQHRF model
#' @param x the output from simulation
#' @param y the input infected proportion
#' @examples
#' para <- c( 1/10,  10, 0.01,  0.02, 0.2, 0.01, 1/20, 1/15, 1/30, 1/1000, 0.01, 0.2, 0.01, 0.02, 0.03)
#' model1 <-rSEIQHRF(N0 = 500, S0 = 497, E0 = 0, I0 = 3, Q0 = 0, H0 = 0,
#'                   R0 = 0, F0 = 0, days = 365, pars = para )
#' IR_info(model1, 0.001)
#'
#'
IR_info.rSEIQHRF <- function(x,y){

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

  # calculate the infected proportion
  rate <- i/n

  # determine the position of given rate
  l <- order(abs(rate- y))[1]
  if(max(rate) < y){ stop("Please enter an effective infected proportion ") }
  cat(" Infection Information")
  cat("\n-----------------------\n")
  cat(" Time needed to reach the given infected proportion = ", t[l])
  cat("\n Number of susceptible people at given infected proportion = ", s[l])
  cat("\n Number of exposed people at given infected proportion = ", e[l])
  cat("\n Number of infected people at given infected proportion = ", i[l])
  cat("\n Number of self-quarantined people at given infected proportion = ", q[l])
  cat("\n Number of people require hospitalization at given infected proportion = ", h[l])
  cat("\n Number of recovered people at given infected proportion = ", r[l])
  cat("\n Number of exposed people at given infected proportion = ", f[l])
  cat("\n Total population at given infected proportion = ", n[l])
  cat("\n")

}

