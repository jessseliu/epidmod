#'@export
#To find the close value in a vector
close <- function(x, value, tol=NULL){
  if(!is.null(tol)){
    x[abs(x-10) <= tol]
  } else {
    x[order(abs(x-10))]
  }
}

#To print the simulated information at a given infection rate for SIR model
#'@export
IR_info.rSIR <- function(x,y,...){

  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  rate <- i/n
  l <- which(rate == close(rate, y)[1])[1]
  if(max(rate) < y){ stop("Please enter an effective infection rate ") }

  cat("\n\n Infection Information")
  cat("\n-----------------------\n")
  cat("\n Time needed to reach the given infection rate is ", t[l])
  cat("\n Number of susceptible people at given infection rate is ", s[l])
  cat("\n Number of infected people at given infection rate is ", i[l])
  cat("\n Number of recovered people at given infection rate is ", r[l])
  cat("\n Total population at given infection rate is ", n[l])

}

#To print the simulated information at a given infection rate for SEIR model
#'@export
IR_info.rSEIR <- function(x,y,...){

  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  r <- x$Immune_people
  rate <- i/n
  l <- which(rate == close(rate, y)[1])[1]
  if(max(rate) < y){ stop("Please enter an effective infection rate ") }

  cat("\n\n Infection Information")
  cat("\n-----------------------\n")
  cat("\n Time needed to reach the given infection rate is ", t[l])
  cat("\n Number of susceptible people at given infection rate is ", s[l])
  cat("\n Number of exposed people at given infection rate is ", e[l])
  cat("\n Number of infected people at given infection rate is ", i[l])
  cat("\n Number of recovered people at given infection rate is ", r[l])
  cat("\n Total population at given infection rate is ", n[l])

}


#To print the simulated information at a given infection rate for SEIQHRF model
#'@export
IR_info.rSEIQHRF <- function(x,y,...){

  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  q <- x$Quarantined_people
  h <- x$Hostipalization_people
  r <- x$Immune_people
  f <- x$Fatality_case
  rate <- i/n
  l <- which(rate == close(rate, y)[1])[1]
  if(max(rate) < y){ stop("Please enter an effective infection rate ") }

  cat("\n\n Infection Information")
  cat("\n-----------------------\n")
  cat("\n Time needed to reach the given infection rate is ", t[l])
  cat("\n Number of susceptible people at given infection rate is ", s[l])
  cat("\n Number of exposed people at given infection rate is ", e[l])
  cat("\n Number of infected people at given infection rate is ", i[l])
  cat("\n Number of self-quarantined people at given infection rate is ", q[l])
  cat("\n Number of people require hospitalization at given infection rate is ", h[l])
  cat("\n Number of recovered people at given infection rate is ", r[l])
  cat("\n Number of exposed people at given infection rate is ", f[l])
  cat("\n Total population at given infection rate is ", n[l])

}
