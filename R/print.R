
#'export
print.rSIR <- function(x,...){


  cat("SIR EpiModel Simulation")
  cat("\n=======================")
  cat("\nModel type:", class(x))

  cat("\n\nSimulation Summary")
  cat("\n-----------------------")
  cat("\nNo. days :", x$Simulation_Time)
  cat("\nNo. groups:", 3)

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
    cat(names(x)[i], "=", x[i][nrow(res)])
  }

  invisible(x)
}

#'export
print.rSEIR <- function(x,...){


  cat("SEIR EpiModel Simulation")
  cat("\n=======================")
  cat("\nModel type:", class(x))

  cat("\n\nSimulation Summary")
  cat("\n-----------------------")
  cat("\nNo. days :", x$Simulation_Time)
  cat("\nNo. groups:", 4)

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
    cat(names(x)[i], "=", x[i][nrow(res)])
  }

  invisible(x)
}

#'export
print.rSEIQHRF <- function(x,...){


  cat("SEIQHRF EpiModel Simulation")
  cat("\n=======================")
  cat("\nModel type:", class(x))

  cat("\n\nSimulation Summary")
  cat("\n-----------------------")
  cat("\nNo. days :", x$Simulation_Time)
  cat("\nNo. groups:", 7)

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
    cat(names(x)[i], "=", x[i][nrow(res)])
  }

  invisible(x)
}
