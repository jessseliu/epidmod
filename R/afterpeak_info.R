
#'Output the peak information and time of infected reducing to certain level after the peak
#'@export
afterpeak_info <- function(x, ...) {
  UseMethod("afterpeak_info")
}


#'@export
#'
afterpeak_info.rSIR <- function(x,...){

  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  rate <- i/n
  cat("\n Peak Information")
  cat("\n-----------------------\n")
  cat("\n Peak infection rate = ", max(rate))
  m <- which(rate == max(rate))[1]
  cat("\n Time to the biggest infection rate = ", t[m])
  cat("\n Number of infected people at the peak = ", i[m])


  cat("\n\n After peak Information")
  cat("\n-----------------------\n")
  i1 <- i[order(abs(i-i[m]*0.5))[1]]
  l1 <- which( i == i1 )
  t1 <- t[l1[which(l1 > m)]][1]
  i2 <- i[order(abs(i-i[m]*0.3))[1]]
  l2 <- which( i == i2 )
  t2 <- t[l2[which(l2 > m)]][1]
  i3 <- i[order(abs(i-i[m]*0.1))[1]]
  l3 <- which( i == i3 )
  t3 <- t[l3[which(l3 > m)]][1]
  cat("\n Time until the number of infected people reduces to 50% of maximum = ", t1)

  cat("\n Time until the number of infected people reduces to 30% of maximum = ", t2)

  cat("\n Time until the number of infected people reduces to 10% of maximum = ", t3)

  cat("\n\n")
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
  cat("\n Peak Information")
  cat("\n-----------------------\n")
  cat("\n Peak infection rate = ", max(rate))
  m <- which(rate == max(rate))[1]
  cat("\n Time to the biggest infection rate = ", t[m])
  cat("\n Number of infected people at the peak = ", i[m])


  cat("\n\n After peak Information")
  cat("\n-----------------------\n")

  i1 <- i[order(abs(i-i[m]*0.5))[1]]
  l1 <- which( i == i1 )
  t1 <- t[l1[which(l1 > m)]][1]
  i2 <- i[order(abs(i-i[m]*0.3))[1]]
  l2 <- which( i == i2 )
  t2 <- t[l2[which(l2 > m)]][1]
  i3 <- i[order(abs(i-i[m]*0.1))[1]]
  l3 <- which( i == i3 )
  t3 <- t[l3[which(l3 > m)]][1]
  cat("\n Time until the number of infected people reduces to 50% of maximum = ", t1)

  cat("\n Time until the number of infected people reduces to 30% of maximum = ", t2)

  cat("\n Time until the number of infected people reduces to 10% of maximum = ", t3)

  cat("\n\n")
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
  cat("\n Peak Information")
  cat("\n-----------------------\n")
  cat("\n Peak infection rate = ", max(rate))
  m <- which(rate == max(rate))[1]
  cat("\n Time to the biggest infection rate = ", t[m])
  cat("\n Number of infected people at the peak = ", i[m])

  cat("\n\n After peak Information")
  cat("\n-----------------------\n")

  i1 <- i[order(abs(i-i[m]*0.5))[1]]
  l1 <- which( i == i1 )
  t1 <- t[l1[which(l1 > m)]][1]
  i2 <- i[order(abs(i-i[m]*0.3))[1]]
  l2 <- which( i == i2 )
  t2 <- t[l2[which(l2 > m)]][1]
  i3 <- i[order(abs(i-i[m]*0.1))[1]]
  l3 <- which( i == i3 )
  t3 <- t[l3[which(l3 > m)]][1]
  cat("\n Time until the number of infected people reduces to 50% of maximum = ", t1)

  cat("\n Time until the number of infected people reduces to 30% of maximum = ", t2)

  cat("\n Time until the number of infected people reduces to 10% of maximum = ", t3)

  cat("\n\n")

}

