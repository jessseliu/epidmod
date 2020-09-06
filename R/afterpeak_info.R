
#' New S3 method for gathering information from stochastic SIR model after the peak
#'
#' \code{afterpeak_info} returns the peak information and time of infected reducing to certain level after the peak
#' @param x the output from simulation
#' @examples
#' model1 <- rSIR (N0 = 1000, I0 = 0, S0 = 1000, days = 300, pars = c(1/10, 1, 1/5, 0.1, 0.1))
#' afterpeak_info(model1)
#'
#'@export
afterpeak_info <- function(x, ...) {
  UseMethod("afterpeak_info")
}

#'@export
#'
afterpeak_info.rSIR <- function(x,...){

  # store the simulation value into vector
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  rate <- i/n
  cat("\n Peak Information")
  cat("\n-----------------------\n")
  cat("\n Peak infected proportion = ", max(rate))

  # determine the position of maximum infected proportion
  m <- which(rate == max(rate))[1]
  cat("\n Time to the infected proportion= ", t[m])
  cat("\n Number of infected people at the peak = ", i[m])


  cat("\n\n After peak Information")
  cat("\n-----------------------\n")

  # find the closest value of infected to the half of maximum infected
  i1 <- i[order(abs(i-i[m]*0.5))[1]]
  # determine the position of the closest value
  l1 <- which( i == i1 )
  # determine the time to get that closest value
  t1 <- t[l1[which(l1 > m)]][1]

   # find the closest value of infected to 0.3 of maximum infected
  i2 <- i[order(abs(i-i[m]*0.3))[1]]
  # determine the position of the closest value
  l2 <- which( i == i2 )
  # determine the time to get that closest value
  t2 <- t[l2[which(l2 > m)]][1]

  # find the closest value of infected to 0.1 of maximum infected
  i3 <- i[order(abs(i-i[m]*0.1))[1]]
  # determine the position of the closest value
  l3 <- which( i == i3 )
  # determine the time to get that closest value
  t3 <- t[l3[which(l3 > m)]][1]

  cat("\n Time until the number of infected people reduces to 50% of maximum = ", t1)

  cat("\n Time until the number of infected people reduces to 30% of maximum = ", t2)

  cat("\n Time until the number of infected people reduces to 10% of maximum = ", t3)

  cat("\n\n")
}


#' New S3 method for gathering information from stochastic SEIR model after the peak
#'
#' \code{afterpeak_info} returns the peak information and time of infected reducing to certain level after the peak
#' @param x the output from simulation
#' @examples
#' model1 <- rSEIR(N0 = 1000, I0 = 0, S0 = 999, R0 = 0, E0 = 1 , days = 300, pars = c(1/12, 1, 1/4, 1/5, 0.3, 0.2, 0.7))
#' afterpeak_info(model1)
#'@export
#'
afterpeak_info.rSEIR <- function(x,...){

  # store the simulation value into vector
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  rate <- i/n
  cat("\n Peak Information")
  cat("\n-----------------------\n")

  # determine the position of maximum infected proportion
  cat("\n Peak infected proportion = ", max(rate))
  m <- which(rate == max(rate))[1]
  cat("\n Time to the biggest infected proportion = ", t[m])
  cat("\n Number of infected people at the peak = ", i[m])


  cat("\n\n After peak Information")
  cat("\n-----------------------\n")

  # find the closest value of infected to the half of maximum infected
  i1 <- i[order(abs(i-i[m]*0.5))[1]]
  # determine the position of the closest value
  l1 <- which( i == i1 )
  # determine the time to get that closest value
  t1 <- t[l1[which(l1 > m)]][1]

  # find the closest value of infected to 0.3 of maximum infected
  i2 <- i[order(abs(i-i[m]*0.3))[1]]
  # determine the position of the closest value
  l2 <- which( i == i2 )
  # determine the time to get that closest value
  t2 <- t[l2[which(l2 > m)]][1]

  # find the closest value of infected to 0.1 of maximum infected
  i3 <- i[order(abs(i-i[m]*0.1))[1]]
  # determine the position of the closest value
  l3 <- which( i == i3 )
  # determine the time to get that closest value
  t3 <- t[l3[which(l3 > m)]][1]

  cat("\n Time until the number of infected people reduces to 50% of maximum = ", t1)

  cat("\n Time until the number of infected people reduces to 30% of maximum = ", t2)

  cat("\n Time until the number of infected people reduces to 10% of maximum = ", t3)

  cat("\n\n")
}

#' New S3 method for gathering information from stochastic SEIQHRF model after the peak
#'
#' \code{afterpeak_info} returns the peak information and time of infected reducing to certain level after the peak
#' @param x the output from simulation
#' @examples
#' para <- c(4,2,1,2,1,3,1,2,1,2,0.9, 0.3, 0.4,0.1, 0.1)
#' model1 <- rSEIQHRF(N0 = 1000, S0 = 999, E0 = 1, I0 = 0, Q0 = 0, H0 = 0, R0 = 0, F0 = 0, days = 300, pars = para )))
#' afterpeak_info(model1)
#'@export
#'
afterpeak_info.rSEIQHRF <- function(x,...){

  # store the simulation value into vector
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  rate <- i/n
  cat("\n Peak Information")
  cat("\n-----------------------\n")
  cat("\n Peak infected proportion = ", max(rate))

  # determine the position of infected proportion
  m <- which(rate == max(rate))[1]
  cat("\n Time to the biggest infected proportion= ", t[m])
  cat("\n Number of infected people at the peak = ", i[m])

  cat("\n\n After peak Information")
  cat("\n-----------------------\n")

  # find the closest value of infected to the half of maximum infected
  i1 <- i[order(abs(i-i[m]*0.5))[1]]
  # determine the position of the closest value
  l1 <- which( i == i1 )
  # determine the time to get that closest value
  t1 <- t[l1[which(l1 > m)]][1]

  # find the closest value of infected to 0.3 of maximum infected
  i2 <- i[order(abs(i-i[m]*0.3))[1]]
  # determine the position of the closest value
  l2 <- which( i == i2 )
  # determine the time to get that closest value
  t2 <- t[l2[which(l2 > m)]][1]

  # find the closest value of infected to 0.1 of maximum infected
  i3 <- i[order(abs(i-i[m]*0.1))[1]]
  # determine the position of the closest value
  l3 <- which( i == i3 )
  # determine the time to get that closest value
  t3 <- t[l3[which(l3 > m)]][1]

  cat("\n Time until the number of infected people reduces to 50% of maximum = ", t1)

  cat("\n Time until the number of infected people reduces to 30% of maximum = ", t2)

  cat("\n Time until the number of infected people reduces to 10% of maximum = ", t3)

  cat("\n\n")

}

