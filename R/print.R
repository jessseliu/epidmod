#' S3 print method for stochastic SIR model
#'
#'@export
print.rSIR <- function(x,...){


  cat(" EpiModel Simulation")
  cat("\n=======================")
  cat("\nModel type:", class(x))

  cat("\n\n Simulation Summary")
  cat("\n-----------------------")
  fre <- length(x$Simulation_Time)
  cat("\nNo. days :", x$Simulation_Time[fre])
  cat("\nNo. groups:", 3)

  i <- x$Infected_people
  n <- x$Total_people
  rate <- i/n

  cat("\nBiggest infection rate = ", max(rate))
  cat("\nTime to the biggest infection rate = ", x$Simulation_Time[which(rate == max(rate))[1]])


  cat("\n\n Model Parameters")
  cat("\n-----------------------\n")

  cat("\nRate of arrival =", x$Param[1])
  cat("\nIndividual infection rate =", x$Param[2])
  cat("\nRecovery rate =", x$Param[3])
  cat("\nProbability of being infected for new arrival =", x$Param[4])
  cat("\nProbability of being immune for recovery people =", x$Param[5])

  cat("\n\n Number of groups at the end of simulation")
  cat("\n-----------------------\n")

   for (i in 2:5) {
    cat(names(x)[i], "=", x[[i]][fre],"\n")
  }
  cat("\n\n")

  invisible(x)
}

#' S3 print method for stochastic SEIR model
#'
#'@export
print.rSEIR <- function(x,...){


  cat(" EpiModel Simulation")
  cat("\n=======================")
  cat("\nModel type:", class(x))

  cat("\n\n Simulation Summary")
  cat("\n-----------------------")
  fre <- length(x$Simulation_Time)
  cat("\nNo. days :", x$Simulation_Time[fre])
  cat("\nNo. groups:", 4)

  i <- x$Infected_people
  n <- x$Total_people
  rate <- i/n

  cat("\nBiggest infection rate = ", max(rate))
  cat("\nTime to the biggest infection rate = ", x$Simulation_Time[which(rate == max(rate))[1]])

  cat("\n\n Model Parameters")
  cat("\n-----------------------\n")

  cat("\nRate of arrival =", x$Param[1])
  cat("\nIndividual being expose rate =", x$Param[2])
  cat("\nIndividual infection rate =", x$Param[3])
  cat("\nRecovery rate =", x$Param[4])
  cat("\nProbability of being exposed for new arrival =", x$Param[5])
  cat("\nProbability of being immune for recovery people =", x$Param[6])

  cat("\n\n Number of groups at the end of simulation")
  cat("\n-----------------------\n")

  for (i in 2:7) {
    cat(names(x)[i], "=", x[[i]][fre],"\n")
  }
  cat("\n\n")

  invisible(x)
}

#' S3 print method for stochastic SEIQHRF model
#'
#'@export
print.rSEIQHRF <- function(x,...){


  cat(" EpiModel Simulation")
  cat("\n=======================")
  cat("\nModel type:", class(x))

  cat("\n\n Simulation Summary")
  cat("\n-----------------------")
  fre <- length(x$Simulation_Time)
  cat("\nNo. days :", x$Simulation_Time[fre])
  cat("\nNo. groups:", 7)

  i <- x$Infected_people
  n <- x$Total_people
  rate <- i/n

  cat("\nBiggest infection rate = ", max(rate))
  cat("\nTime to the biggest infection rate = ", x$Simulation_Time[which(rate == max(rate))[1]])

  cat("\n\n Model Parameters")
  cat("\n-----------------------\n")

  cat("\nRate of arrival =", x$Param[1])
  cat("\nIndividual being expose rate =", x$Param[2])
  cat("\nIndividual infection rate =", x$Param[3])
  cat("\nInfected individual requiring hospitalization rate =", x$Param[4])
  cat("\nSelf-isolated individual requiring hospitalization rate =", x$Param[5])
  cat("\nInfected individual self-quarantining rate =", x$Param[6])
  cat("\nRecovery rate for each infected individual", x$Param[7])
  cat("\nRecovery rate for each individual who accepts hospitalization =", x$Param[8])
  cat("\nRecovery rate for each individual who is self-quarantined =", x$Param[9])
  cat("\nIndividual who accepts hospitalization case fatality rate =", x$Param[10])
  cat("\nProbability of being exposed for new arrival =", x$Param[11])
  cat("\nProbability that a quarantined person recover is immune =", x$Param[12])
  cat("\nProbability of being immune for recovery people =", x$Param[13])

  cat("\n\n Number of groups at the end of simulation")
  cat("\n-----------------------\n")

  for (i in 2:10) {
    cat(names(x)[i], "=", x[[i]][fre],"\n")
  }
  cat("\n\n")

  invisible(x)
}



