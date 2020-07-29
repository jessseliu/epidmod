
#' Simulates one jump from a simple stochastic SEIR epidemic model.
#'
#' \code{SEIRjump} returns states of next jump given the states right now.
#'
#' @param x a numeric vector: (time, S, E, I, R, N) at the previous jump.
#' @param pars a numeric vector: (a.rate, e.rate, i.rate, r.rate, pE, pSR, pIm).
#' @return a numeric vector: (time, S, E, I, R, N) immediately after the next jump.
#' @examples
#' A <- c(0,1000,0,0,0,1000)
#' B <- c(5,2,4,1,0.9,0.1)
#' SEIRjump(A,B)
#' @export

SEIRjump <- function (x, pars) {
  # The numbers of susceptible and infected people and the population size
  St <- x[2]
  Et <- x[3]
  It <- x[4]
  Rt <- x[5]
  Nt <- x[6]
  # The parameter values
  a.rate <- pars[1]
  e.rate <- pars[2]
  i.rate <- pars[3]
  r.rate <- pars[4]
  pE <- pars[5]
  pSR <- pars[6]
  pIm <- pars[7]

  # Simulate the time at which the next transition occurs
  total_rate <- a.rate + e.rate * St * It / Nt + i.rate * Et  + r.rate * It
  x[1] <- x[1] + rexp(1, total_rate)
  # Jump probabilities
  p1 <- a.rate * pE / total_rate
  p2 <- a.rate * (1 - pE) / total_rate
  p3 <- r.rate * pIm * It / total_rate
  p4 <- r.rate * (1 - pIm) * It / total_rate
  p5 <- (i.rate * Et * pSR ) / total_rate
  p6 <- (i.rate * Et * (1 - pSR) ) / total_rate

  u <- runif(1)
  if (u < p1) {
    # Arrival of a new exposed person Et increases by 1, Nt increases by 1
    x[3] <- x[3] + 1
    x[6] <- x[6] + 1

  } else if (u < p1 + p2) {
    # Arrival of a new susceptible person St increases by 1, Nt increases by 1
    x[2] <- x[2] + 1
    x[6] <- x[6] + 1

  } else if (u < p1 + p2 + p3) {
    # Infected person becomes immune: It decreases by 1, Rt increases by 1
    x[4] <- x[4] - 1
    x[5] <- x[5] + 1

  } else if (u < p1 + p2 + p3 + p4) {
    # Infected person becomes susceptible: It decreases by 1, St increases by 1
    x[4] <- x[4] - 1
    x[2] <- x[2] + 1

  } else if (u < p1 + p2 + p3 + p4 + p5){
    # Recovery of a exposed: Et decreases by 1, St increases by 1
    x[3] <- x[3] - 1
    x[2] <- x[2] + 1

  } else if (u < p1 + p2 + p3 + p4 + p5 + p6) {
    # Infection of a exposed: Et decreases by 1, It increases by 1
    x[3] <- x[3] - 1
    x[4] <- x[4] + 1

  }  else {
    # Latency of a susceptible: St decreases by 1, Et increases by 1
    x[2] <- x[2] - 1
    x[3] <- x[3] + 1
  }
  return(x)
}
