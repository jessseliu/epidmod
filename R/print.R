print.rSIR <- function(x, ...) {

  cat("EpiModel Simulation")
  cat("\n=======================")
  cat("\nModel class:", class(x))

  cat("\n\nSimulation Summary")
  cat("\n-----------------------")
  cat("\nModel type:", x$control$type)
  cat("\nNo. simulations:", x$control$nsims)
  cat("\nNo. time steps:", x$control$nsteps)
  cat("\nNo. groups:", x$param$groups)

  cat("\n\nModel Parameters")
  cat("\n-----------------------\n")
  pToPrint <- which(!(names(x$param) %in% c("groups", "vital")))
  for (i in pToPrint) {
    cat(names(x$param)[i], "=", x$param[[i]], fill = 60)
  }

  cat("\nModel Output")
  cat("\n-----------------------")
  cat("\nVariables:", names(x$epi), fill = 60)

}
