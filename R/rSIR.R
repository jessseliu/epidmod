#
# Simulates from a simple stochastic SIR epidemic model
#
# Arguments:
#   N0     - an integer. The population size at time 0.
#   I0     - an integer. The initial number of infected people.
#   S0     - an integer. The initial number of susceptible people.
#   R0     - an integer. The initial number of recovered people.
#   days   - an integer. The number of days for which to simulate.
#   pars   - a numeric vector: (alpha, pI, beta, pR, gamma).
#     alpha : rate of arrival of people into the population
#     pI    : probability that an arrival is infected
#     beta  : individual infection rate at time t is beta / Nt
#     gamma : recovery rate for each infected individual
#     pR    : probability that an infected person is immune after recovery
#
# Returns:
#   A numeric matrix with 5 columns.  Row i contains the values of
#   (t, S_t, I_t, R_t, N_t) immediately after transition i - 1.
#
# Checks:


rSIR <- function(N0 = 1000, I0 = 0, S0 = N0 - I0, days = 100,
                 pars = c(1, 0.1, 2, 1, 0.9)) {

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
  return(res)
}




