
#'@export
print.rSIR <- function(x,...){


  cat("SIR EpiModel Simulation")
  cat("\n=======================")
  cat("\nModel type:", class(x))

  cat("\n\nSimulation Summary")
  cat("\n-----------------------")
  cat("\nNo. days :", x$Simulation_Time[nrow(x)])
  cat("\nNo. groups:", 3)

  i <- x$Infected_people
  n <- x$Total_people
  rate <- i/n

  cat("\n Biggest infection rate is ", max(rate))
  cat("\n Time to the biggest infection rate is ", which(rate == max(rate))[1])


  cat("\n\nModel Parameters")
  cat("\n-----------------------\n")

  cat(" Rate of arrival =", x$pars[1])
  cat(" Individual infection rate =", x$pars[2])
  cat(" Recovery rate =", x$pars[3])
  cat(" Probability of being infected for new arrival =", x$pars[4])
  cat(" Probability of being immune for recovery people =", x$pars[5])

  cat("\n\n Number of groups at the end of simulation")
  cat("\n-----------------------\n")

   for (i in 2:5) {
    cat(names(x)[i], "=", x[i][nrow(x)])
  }

  invisible(x)
}


#'@export
#To find the close value in a vector
close <- function(x, value, tol=NULL){
  if(!is.null(tol)){
    x[abs(x-10) <= tol]
  } else {
    x[order(abs(x-10))]
  }
}

#To print the information at a given infection rate
#'@export
IR_information.rSIR <- function(x,y,...){

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


#'@export
print.rSEIR <- function(x,...){


  cat("SEIR EpiModel Simulation")
  cat("\n=======================")
  cat("\nModel type:", class(x))

  cat("\n\nSimulation Summary")
  cat("\n-----------------------")
  cat("\nNo. days :", x$Simulation_Time[nrow(x)])
  cat("\nNo. groups:", 4)

  i <- x$Infected_people
  n <- x$Total_people
  rate <- i/n

  cat("\n Biggest infection rate is ", max(rate))
  cat("\n Time to the biggest infection rate is ", which(rate == max(rate))[1])

  cat("\n\nModel Parameters")
  cat("\n-----------------------\n")

  cat(" Rate of arrival =", x$pars[1])
  cat(" Individual being expose rate =", x$pars[2])
  cat(" Individual infection rate =", x$pars[3])
  cat(" Recovery rate =", x$pars[4])
  cat(" Probability of being exposed for new arrival =", x$pars[5])
  cat(" Probability of being immune for recovery people =", x$pars[6])

  cat("\n\n Number of groups at the end of simulation")
  cat("\n-----------------------\n")

  for (i in 2:7) {
    cat(names(x)[i], "=", x[i][nrow(x)])
  }

  invisible(x)
}

#'@export
print.rSEIQHRF <- function(x,...){


  cat("SEIQHRF EpiModel Simulation")
  cat("\n=======================")
  cat("\nModel type:", class(x))

  cat("\n\nSimulation Summary")
  cat("\n-----------------------")
  cat("\nNo. days :", x$Simulation_Time[nrow(x)])
  cat("\nNo. groups:", 7)

  i <- x$Infected_people
  n <- x$Total_people
  rate <- i/n

  cat("\n Biggest infection rate is ", max(rate))
  cat("\n Time to the biggest infection rate is ", which(rate == max(rate))[1])

  cat("\n\nModel Parameters")
  cat("\n-----------------------\n")

  cat(" Rate of arrival =", x$pars[1])
  cat(" Individual being expose rate =", x$pars[2])
  cat(" Individual infection rate =", x$pars[3])
  cat(" Infected individual requiring hospitalization rate =", x$pars[4])
  cat(" Self-isolated individual requiring hospitalization rate =", x$pars[5])
  cat(" Infected individual self-quarantining rate =", x$pars[6])
  cat(" Recovery rate for each infected individual", x$pars[7])
  cat(" Recovery rate for each individual who accepts hospitalization =", x$pars[8])
  cat(" Recovery rate for each individual who is self-quarantined =", x$pars[9])
  cat(" Individual who accepts hospitalization case fatality rate =", x$pars[10])
  cat(" Probability of being exposed for new arrival =", x$pars[11])
  cat(" Probability that a quarantined person recover is immune =", x$pars[12])
  cat(" Probability of being immune for recovery people =", x$pars[13])

  cat("\n\n Number of groups at the end of simulation")
  cat("\n-----------------------\n")

  for (i in 2:10) {
    cat(names(x)[i], "=", x[i][nrow(x)])
  }

  invisible(x)
}



