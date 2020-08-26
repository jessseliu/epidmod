
#To print the simulated information at a given infection rate for SIR model
#'@export
IR_info <- function(x, ...) {
  UseMethod("IR_info")
}


#'@export
IR_info.rSIR <- function(x,y,...){

  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  rate <- i/n
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

#To print the simulated information at a given infection rate for SEIR model
#'@export
IR_info.rSEIR <- function(x,y,...){

  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  rate <- i/n
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



#To print the simulated information at a given infection rate for SEIQHRF model
#'@export
#'
IR_info.rSEIQHRF <- function(x,y,...){

  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  q <- x$Quarantined_people
  h <- x$Hostipalization_people
  r <- x$Immune_people
  f <- x$Fatality_case
  n <- x$Total_people
  rate <- i/n
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

