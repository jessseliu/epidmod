

#' Simulates from a simple stochastic SEIQHRF epidemic model.
#'
#' \code{rSIR} returns a matrix contains information about time, number of susceptible people, exposed people, infected people,
#' recover people, total number of people.
#'  The meanings of parameters in pars are:
#'  a.rate    represents rate of arrival of people into the population.
#'  e.rate    represents individual being exposed rate at time t is e.rate / Nt .
#'  i.rate    represents individual infection rate at time t is i.rate / Nt .
#'  h1.rate   represents infected individual requiring hospitalization rate at time t is h1.rate / Nt
#'  h2.rate   represents self-isolated individual requiring hospitalization rate at time t is h2.rate / Nt
#'  q.rate    represents infected individual self-quarantining rate at time t is q.rate / Nt
#'  r1.rate   represents recovery rate for each infected individual.
#'  r2.rate   represents recovery rate for each individual who accepts hospitalization.
#'  r3.rate   represents recovery rate for each individual who is self-quarantined.
#'  f.rate    represents individual who accepts hospitalization case fatality rate at time t is f.rate / Nt
#'  pE   represents probability that an arrival is exposed.
#'  pQ   represents probability that a quarantined person recover is immune after recovery.
#'  pIm  represents probability that an infected person is immune after recovery.
#'
#' @param N0 an integer. The population size at time 0
#' @param S0 an integer. The initial number of susceptible people.
#' @param E0 an integer. The initial number of exposed people.
#' @param I0 an integer. The initial number of infected people.
#' @param Q0 an integer. The initial number of quarantined people.
#' @param H0 an integer. The initial number of people require hospitalization.
#' @param R0 an integer. The initial number of recovered people.
#' @param F0 an integer. The initial number of case fatality.
#' @param days an integer. The number of days for which to simulate.
#' @param pars a numeric vector: (a.rate, e.rate, i.rate, h1.rate, q.rate, h2.rate, r1.rate, r2.rate, r3.rate, f.rate, pE, pQ, pIm).
#'
#' @return A numeric matrix with 9 columns.  Row i contains the values of (t, S_t, E_t, I_t, Q_t, R_t, H_t, R_t, F_t, N_t) at time t.
#' @examples
#' rSEIR <- function(N0 = 100, I0 = 0, S0 = 99, R0 = 0, E0 = N0 - I0 - S0, days = 100,pars = c(1, 0.3, 4, 2, 2, 0.7))



rSEIR <- function(N0 = 1000 , S0 = 999, E0 = 1, I0 = 0, Q0 = 0, H0 = 0, R0 = 0 ,F0 = N0 - S0 - E0 - I0 - Q0 - H0 - R0,
                  days = 100 , pars = c(1, 0.1, 2, 2, 1, 0.9)) {

  if ( S0 + E0 + I0 + Q0 + H0 + R0 + F0 > N0) {
    stop("There can be at most N people among all states")
  }
  # Infer the number of dead people
  F0 <- N0 - S0 - E0 - I0 - Q0 - H0 - R0
  # The initial state, at time = 0
  x <- c(0, S0, E0, I0, Q0, H0, R0, F0, N0)
  # A list in which to store the results
  res <- list()
  i <- 1
  # Simulate the next change, or jump, in the process until days have passed
  while (x[1] < days) {
    res[[i]] <- x
    x <- SEIQHRFjump(x, pars)
    i <- i + 1
  }
  # Convert the list to a matrix
  res <- do.call(rbind, res)
  colnames(res) <- c("t", "St","Et", "It", "Qt", "Ht","Rt", "Ft", "Nt")
  return(res)
}





