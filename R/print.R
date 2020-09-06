#' S3 print method for stochastic SIR model
#'
#' \code{print} returns the basic information of simulated SIR model
#' @param x the output from simulation
#' @examples
#' model1 <- rSIR (N0 = 1000, I0 = 0, S0 = 1000, days = 300, pars = c(1/10, 1, 1/5, 0.1, 0.1))
#' print(model1)
#'
#'@export
print.rSIR <- function(x,...){


  cat(" EpiModel Simulation")
  cat("\n=======================")
  cat("\nModel type:", class(x))

  cat("\n\n Simulation Summary")
  cat("\n-----------------------")

  # calculate the length of simulated value in each groups
  fre <- length(x$Simulation_Time)
  cat("\nNo. days :", x$Simulation_Time[fre])
  cat("\nNo. groups:", 3)


  # calculate the maximum infected proportion and the corresponding position
  i <- x$Infected_people
  n <- x$Total_people
  rate <- i/n

  cat("\nBiggest infected proportion = ", max(rate))
  cat("\nTime to the biggest infected proportion = ", x$Simulation_Time[which(rate == max(rate))[1]])


  cat("\n\n Model Parameters")
  cat("\n-----------------------\n")

  # extract information from the output
  cat("\nRate of arrival =", x$Param[1])
  cat("\nIndividual infection rate =", x$Param[2])
  cat("\nRecovery rate =", x$Param[3])
  cat("\nProbability of being infected for new arrival =", x$Param[4])
  cat("\nProbability of being immune for recovery people =", x$Param[5])

  cat("\n\n Number of groups at the end of simulation")
  cat("\n-----------------------\n")

  # extract information at the end of simulation
  for (i in 2:5) {
    cat(names(x)[i], "=", x[[i]][fre],"\n")
  }
  cat("\n\n")

  invisible(x)
}

#' S3 print method for stochastic SEIR model
#'
#' \code{print} returns the basic information of simulated SEIR model
#' @param x the output from simulation
#' @examples
#' model1 <- rSEIR(N0 = 1000, I0 = 0, S0 = 999, R0 = 0, E0 = 1 , days = 300, pars = c(1/12, 1, 1/4, 1/5, 0.3, 0.2, 0.7))
#' print(model1)
#'
#'@export
print.rSEIR <- function(x,...){


  cat(" EpiModel Simulation")
  cat("\n=======================")
  cat("\nModel type:", class(x))

  cat("\n\n Simulation Summary")
  cat("\n-----------------------")

  # calculate the length of simulated value in each groups
  fre <- length(x$Simulation_Time)
  cat("\nNo. days :", x$Simulation_Time[fre])
  cat("\nNo. groups:", 4)

  # calculate the maximum infected proportion and the corresponding position
  i <- x$Infected_people
  n <- x$Total_people
  rate <- i/n

  cat("\nBiggest infected proportion = ", max(rate))
  cat("\nTime to the biggest infected proportion = ", x$Simulation_Time[which(rate == max(rate))[1]])

  cat("\n\n Model Parameters")
  cat("\n-----------------------\n")

  # extract information from the output
  cat("\nRate of arrival =", x$Param[1])
  cat("\nIndividual being expose rate =", x$Param[2])
  cat("\nIndividual infection rate =", x$Param[3])
  cat("\nRecovery rate =", x$Param[4])
  cat("\nProbability of being exposed for new arrival =", x$Param[5])
  cat("\nProbability of being immune for recovery people =", x$Param[6])

  cat("\n\n Number of groups at the end of simulation")
  cat("\n-----------------------\n")

  # extract information at the end of simulation
  for (i in 2:7) {
    cat(names(x)[i], "=", x[[i]][fre],"\n")
  }
  cat("\n\n")

  invisible(x)
}

#' S3 print method for stochastic SEIQHRF model
#'
#' \code{print} returns the basic information of simulated SEIQHRF model
#' @param x the output from simulation
#' @examples
#' para <- c(4,2,1,2,1,3,1,2,1,2,0.9, 0.3, 0.4,0.1, 0.1)
#' model1 <- rSEIQHRF(N0 = 1000, S0 = 999, E0 = 1, I0 = 0, Q0 = 0, H0 = 0, R0 = 0, F0 = 0, days = 300, pars = para )
#' print(model1)
#'
#'@export
print.rSEIQHRF <- function(x,...){


  cat(" EpiModel Simulation")
  cat("\n=======================")
  cat("\nModel type:", class(x))

  cat("\n\n Simulation Summary")
  cat("\n-----------------------")

  # calculate the length of simulated value in each groups
  fre <- length(x$Simulation_Time)
  cat("\nNo. days :", x$Simulation_Time[fre])
  cat("\nNo. groups:", 7)

  # calculate the maximum infected proportion and the corresponding position
  i <- x$Infected_people
  n <- x$Total_people
  rate <- i/n

  cat("\nBiggest infected proportion= ", max(rate))
  cat("\nTime to the biggest infected proportion = ", x$Simulation_Time[which(rate == max(rate))[1]])

  cat("\n\n Model Parameters")
  cat("\n-----------------------\n")

  # extract information from the output
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

  # extract information at the end of simulation
  for (i in 2:10) {
    cat(names(x)[i], "=", x[[i]][fre],"\n")
  }
  cat("\n\n")

  invisible(x)
}



