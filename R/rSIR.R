

#' Simulates from a simple stochastic SIR epidemic model.
#'
#' \code{rSIR} returns a matrix contains information about time, number of susceptible people, infected people,
#' recover people, total number of people.
#'
#' @param N0 an integer. The population size at time 0.
#' @param I0 an integer. The initial number of infected people.
#' @param S0 an integer. The initial number of susceptible people.
#' @param R0 an integer. The initial number of recovered people.
#' @param days an integer. The number of days for which to simulate.
#' @param pars a numeric vector: (a.rate, i.rate, r.rate, pI, pIm). Description about pars
#' *\code{a.rate} represents rate of arrival of people into the population.
#' *\code{i.rate} represents individual infection rate at time t is beta / Nt .
#' *\code{r.rate} represents recovery rate for each infected individual.
#' *\code{pI}     represents probability that an arrival is infected.
#' *\code{pIm}    represents probability that an infected person is immune after recovery.

#' @return A numeric matrix with 5 columns.  Row i contains the values of (t, S_t, I_t, R_t, N_t) at time t.
#' @examples
#' rSIR <- function(N0 = 100, I0 = 0, S0 = N0 - I0, days = 100,pars = c(1, 4, 2, 0.3, 0.7))



rSIR <- function(N0 = 1000 , I0 = 0  , R0 = 0, S0 = N0 - I0 - R0,  days = 100 , pars = c(1, 2, 1, 0.1, 0.9)) {

  if (I0 + S0 > N0) {
    stop("There can be at most N people who are susceptible or infected")
  }
  # Infer the number of recovered people
  R0 <- N0 - I0 - S0
  # The initial state, at time = 0
  x <- c(0, S0, I0, R0, N0)
  # A list in which to store the results
  res <- list()
  i <- 1
  # Simulate the next change, or jump, in the process until days have passed
  while (x[1] < days) {
    res[[i]] <- x
    x <- SIRjump(x, pars)
    i <- i + 1
  }
  # Convert the list to a matrix
  res <- do.call(rbind, res)
  colnames(res) <- c("t", "St", "It", "Rt", "Nt")
  class(res) <- "SIR"
  return(res)
}





