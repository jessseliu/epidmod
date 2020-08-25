
#'Output the peak information and time of infected reducing to certain level after the peak
#'@export
#'
afterpeak_info.rSIR <- function(x,...){

  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  rate <- i/n
  cat("\n\n Peak Information")
  cat("\n-----------------------\n")
  cat("\n Peak infection rate is ", max(rate))
  m <- which(rate == max(rate))[1]
  cat("\n Time to the biggest infection rate is ", m)
  cat("\n Number of infected people at the peak ", i[m])

  cat("\n\n After peak Information")
  cat("\n-----------------------\n")

  t1 <- which( i = i[m] * 0.5)
  t1 <- subset(t1, t1 > m)[1]
  t2 <- which( i = i[m] * 0.3)
  t2 <- subset(t2, t2 > m)[1]
  t3 <- which( i = i[m] * 0.1)
  t3 <- subset(t3, t3 > m)[1]
  cat("\n Time until the number of infected people reduces to 50% of maximum", t1)

  cat("\n Time until the number of infected people reduces to 30% of maximum", t2)

  cat("\n Time until the number of infected people reduces to 10% of maximum", t3)

}

#'Output the peak information and time of infected reducing to certain level after the peak
#'@export
#'
afterpeak_info.rSEIR <- function(x,...){

  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  rate <- i/n
  cat("\n\n Peak Information")
  cat("\n-----------------------\n")
  cat("\n Peak infection rate is ", max(rate))
  m <- which(rate == max(rate))[1]
  cat("\n Time to the biggest infection rate is ", m)
  cat("\n Number of infected people at the peak ", i[m])

  cat("\n\n After peak Information")
  cat("\n-----------------------\n")

  t1 <- which( i = i[m] * 0.5)
  t1 <- subset(t1, t1 > m)[1]
  t2 <- which( i = i[m] * 0.3)
  t2 <- subset(t2, t2 > m)[1]
  t3 <- which( i = i[m] * 0.1)
  t3 <- subset(t3, t3 > m)[1]
  cat("\n Time until the number of infected people reduces to 50% of maximum", t1)

  cat("\n Time until the number of infected people reduces to 30% of maximum", t2)

  cat("\n Time until the number of infected people reduces to 10% of maximum", t3)

}

#'Output the peak information and time of infected reducing to certain level after the peak
#'@export
#'
afterpeak_info.rSEIQHRF <- function(x,...){

  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  rate <- i/n
  cat("\n\n Peak Information")
  cat("\n-----------------------\n")
  cat("\n Peak infection rate is ", max(rate))
  m <- which(rate == max(rate))[1]
  cat("\n Time to the biggest infection rate is ", m)
  cat("\n Number of infected people at the peak ", i[m])

  cat("\n\n After peak Information")
  cat("\n-----------------------\n")

  t1 <- which( i = i[m] * 0.5)
  t1 <- subset(t1, t1 > m)[1]
  t2 <- which( i = i[m] * 0.3)
  t2 <- subset(t2, t2 > m)[1]
  t3 <- which( i = i[m] * 0.1)
  t3 <- subset(t3, t3 > m)[1]
  cat("\n Time until the number of infected people reduces to 50% of maximum", t1)

  cat("\n Time until the number of infected people reduces to 30% of maximum", t2)

  cat("\n Time until the number of infected people reduces to 10% of maximum", t3)

}

