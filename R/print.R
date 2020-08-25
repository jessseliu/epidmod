#' S3 print method for stochastic SIR model
#'
#'@export
print.rSIR <- function(x,...){


  cat(" EpiModel Simulation")
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

  cat(" Rate of arrival =", x$Param[1])
  cat(" Individual infection rate =", x$Param[2])
  cat(" Recovery rate =", x$Param[3])
  cat(" Probability of being infected for new arrival =", x$Param[4])
  cat(" Probability of being immune for recovery people =", x$Param[5])

  cat("\n\n Number of groups at the end of simulation")
  cat("\n-----------------------\n")

   for (i in 2:5) {
    cat(names(x)[i], "=", x[i][nrow(x)])
  }

  invisible(x)
}

#' S3 print method for stochastic SEIR model
#'
#'@export
print.rSEIR <- function(x,...){


  cat(" EpiModel Simulation")
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

  cat(" Rate of arrival =", x$Param[1])
  cat(" Individual being expose rate =", x$Param[2])
  cat(" Individual infection rate =", x$Param[3])
  cat(" Recovery rate =", x$Param[4])
  cat(" Probability of being exposed for new arrival =", x$Param[5])
  cat(" Probability of being immune for recovery people =", x$Param[6])

  cat("\n\n Number of groups at the end of simulation")
  cat("\n-----------------------\n")

  for (i in 2:7) {
    cat(names(x)[i], "=", x[i][nrow(x)])
  }

  invisible(x)
}

#' S3 print method for stochastic SEIQHRF model
#'
#'@export
print.rSEIQHRF <- function(x,...){


  cat(" EpiModel Simulation")
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

  cat(" Rate of arrival =", x$Param[1])
  cat(" Individual being expose rate =", x$Param[2])
  cat(" Individual infection rate =", x$Param[3])
  cat(" Infected individual requiring hospitalization rate =", x$Param[4])
  cat(" Self-isolated individual requiring hospitalization rate =", x$Param[5])
  cat(" Infected individual self-quarantining rate =", x$Param[6])
  cat(" Recovery rate for each infected individual", x$Param[7])
  cat(" Recovery rate for each individual who accepts hospitalization =", x$Param[8])
  cat(" Recovery rate for each individual who is self-quarantined =", x$Param[9])
  cat(" Individual who accepts hospitalization case fatality rate =", x$Param[10])
  cat(" Probability of being exposed for new arrival =", x$Param[11])
  cat(" Probability that a quarantined person recover is immune =", x$Param[12])
  cat(" Probability of being immune for recovery people =", x$Param[13])

  cat("\n\n Number of groups at the end of simulation")
  cat("\n-----------------------\n")

  for (i in 2:10) {
    cat(names(x)[i], "=", x[i][nrow(x)])
  }

  invisible(x)
}



