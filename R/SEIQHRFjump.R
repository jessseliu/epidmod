
#' Simulates one jump from a simple stochastic SEIQHRF epidemic model.
#'
#' \code{SEIQHRFjump} returns states of next jump given the states right now.
#'
#' @param x a numeric vector: (time, S, E, I, Q, H, R, F, N) at the previous jump.
#' @param pars a numeric vector: (a.rate, e.rate, i.rate, h1.rate, q.rate, h2.rate, r1.rate, r2.rate, r3.rate, f.rate, pE, pQ, pSR, pIm).
#'
#' @return a numeric vector: (time, S, E, I, Q, H, R, F, N)  immediately after the next jump.
#' @examples
#' A <- c(0,1000,0,0,0,0,0,0,1000)
#' B <- c(4,2,1,2,1,3,1,2,1,2,0.9, 0.3, 0.4,0.1)
#' SEIQHRFjump(A,B)
#' @export

SEIQHRFjump <- function (x, pars) {
  # The numbers of susceptible and infected people and the population size
  St <- x[2]
  Et <- x[3]
  It <- x[4]
  Qt <- x[5]
  Ht <- x[6]
  Rt <- x[7]
  Ft <- x[8]
  Nt <- x[9]
  # The parameter values
  a.rate <- pars[1]
  e.rate <- pars[2]
  i.rate <- pars[3]
  h1.rate <- pars[4]
  q.rate <-  pars[5]
  h2.rate <- pars[6]
  r1.rate <- pars[7]
  r2.rate <- pars[8]
  r3.rate <- pars[9]
  f.rate <- pars[10]
  pE <- pars[11]
  pQ <- pars[12]
  pSR <- pars[13]
  pIm <- pars[14]
  # Simulate the time at which the next transition occurs
  total_rate <- a.rate + e.rate * St * Et / Nt + i.rate * Et  + (q.rate + h1.rate + r1.rate) * It +
    (h2.rate + r3.rate) * Qt + (r2.rate + f.rate) * Ht
  x[1] <- x[1] + rexp(1, total_rate)
  # Jump probabilities
  p1 <- a.rate * pE / total_rate
  p2 <- a.rate * (1 - pE) / total_rate
  p3 <- i.rate * pSR * Et
  p4 <- i.rate * (1 - pSR) * Et
  p5 <- q.rate * It
  p6 <- h1.rate * It
  p7 <- r1.rate * It * pIm
  p8 <- r1.rate * It * (1-pIm)
  p9 <- h2.rate * Qt
  p10 <- r3.rate * Qt * pQ
  p11 <- r3.rate * Qt * (1 - pQ)
  p12 <- r2.rate * Ht
  p13 <- f.rate * Ht

  u <- runif(1)
  if (u < p1) {
    # Arrival of a new exposed person Et increases by 1, Nt increases by 1
    x[3] <- x[3] + 1
    x[9] <- x[9] + 1

  } else if (u < p1 + p2) {
    # Arrival of a new susceptible person St increases by 1, Nt increases by 1
    x[2] <- x[2] + 1
    x[9] <- x[9] + 1

  } else if (u < p1 + p2 + p3) {
    # Recovery of a exposed: Et decreases by 1, St increases by 1
    x[3] <- x[3] - 1
    x[2] <- x[2] + 1

  } else if (u < p1 + p2 + p3 + p4) {
    # Exposed person becomes infected: Et decreases by 1, It increases by 1
    x[3] <- x[3] - 1
    x[4] <- x[4] + 1

  } else if (u < p1 + p2 + p3 + p4 +p5) {
    # Infected person becomes quarantined: It decreases by 1, Qt increases by 1
    x[4] <- x[4] - 1
    x[5] <- x[5] + 1

  } else if (u < p1 + p2 + p3 + p4 + p5 + p6) {
    # Infected person becomes immune: It decreases by 1, Rt increases by 1
    x[4] <- x[4] - 1
    x[7] <- x[7] + 1
  } else if  (u < p1 + p2 + p3 + p4 + p5 + p6 + p7) {
    # Infected person requires hospitalization: It decreases by 1, Ht increases by 1
    x[4] <- x[4] - 1
    x[6] <- x[6] + 1

  } else if  (u < p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8) {
    # Quarantined person requires hospitalization: Qt decreases by 1, Ht increases by 1
    x[5] <- x[5] - 1
    x[6] <- x[6] + 1
  } else if  (u < p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9) {
    # Infected person becomes susceptible: It decreases by 1, St increases by 1
    x[4] <- x[4] - 1
    x[2] <- x[2] + 1

  } else if  (u < p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10) {
    # Quarantined person becomes immune: Qt decreases by 1, Rt increases by 1
    x[5] <- x[5] - 1
    x[7] <- x[7] + 1

  } else if  (u < p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11) {
    # Quarantined person becomes susceptible: Qt decreases by 1, St increases by 1
    x[5] <- x[5] - 1
    x[2] <- x[2] + 1

  } else if  (u < p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12) {
    # Person with hospitalization becomes immune: Ht decreases by 1, Rt increases by 1
    x[6] <- x[6] - 1
    x[7] <- x[7] + 1

  } else if (u < p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13){
    # Person with hospitalization becomes case fatality: Ht decreases by 1, Ft increases by 1
    x[6] <- x[6] - 1
    x[8] <- x[8] + 1

  } else {
    # Latency of a susceptible: St decreases by 1, Et increases by 1
    x[2] <- x[2] - 1
    x[3] <- x[3] + 1
  }
  return(x)
}
