
#' Simulates one jump from a simple stochastic SIS epidemic model.
#'
#' \code{SIRjump} returns states of next jump given the states right now.
#'
#' @param x a numeric vector: (time, S, I, R, N) at the previous jump.
#' @param pars a numeric vector: (alpha, pI, beta, gamma, pR).
#' @return a numeric vector: (time, S, I, R, N) immediately after the next jump.
#' @examples
#' A <- c(0,1000,0,0,1000)
#' B <- c(1,0.1,2,1,0.9)
#' SIRjump(A,B)
#' @importFrom ("stats", "rexp", "runif")


SIRjump <- function (x, pars) {
  # The numbers of susceptible and infected people and the population size
  St <- x[2]
  It <- x[3]
  Rt <- x[4]
  Nt <- x[5]
  # The parameter values
  alpha <- pars[1]
  pI <- pars[2]
  beta <- pars[3]
  gamma <- pars[4]
  pR <- pars[5]
  # Simulate the time at which the next transition occurs
  total_rate <- alpha + beta * St * It / Nt + gamma * It
  x[1] <- x[1] + rexp(1, total_rate)
  # Jump probabilities
  p1 <- alpha * pI / total_rate
  p2 <- alpha * (1 - pI) / total_rate
  p3 <- gamma * pR * It / total_rate
  p4 <- gamma * (1 - pR) * It / total_rate
  u <- runif(1)
  if (u < p1) {
    # Arrival of a new infected person It increases by 1, Nt increases by 1
    x[3] <- x[3] + 1
    x[5] <- x[5] + 1
  } else if (u < p1 + p2) {
    # Arrival of a new susceptible person St increases by 1, Nt increases by 1
    x[2] <- x[2] + 1
    x[5] <- x[5] + 1
  } else if (u < p1 + p2 + p3) {
    # Infected person becomes immune: It decreases by 1, Rt increases by 1
    x[3] <- x[3] - 1
    x[4] <- x[4] + 1
  } else if (u < p1 + p2 + p3 + p4) {
    # Infected person becomes susceptible: It decreases by 1, St increases by 1
    x[3] <- x[3] - 1
    x[2] <- x[2] + 1
  } else {
    # Infection of a susceptible: St decreases by 1, It increases by 1
    x[2] <- x[2] - 1
    x[3] <- x[3] + 1
  }
  return(x)
}
