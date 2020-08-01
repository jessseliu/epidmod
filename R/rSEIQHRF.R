

#' Simulates from a simple stochastic SEIQHRF epidemic model.
#'
#' \code{rSEIQHRF} returns a matrix contains information about time, number of susceptible people, exposed people, infected people,
#' recover people, total number of people.
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
#' @param pars a numeric vector: (a.rate, e.rate, i.rate, h1.rate, q.rate, h2.rate, r1.rate, r2.rate, r3.rate, f.rate, pE, pSR, pIim, pQim, pHim).
#' #' Description about pars
#' *\code{a.rate} represents rate of arrival of people into the population.
#' *\code{e.rate} represents individual being exposed rate at time t is e.rate / Nt.
#' *\code{i.rate} represents the incubation period is 1/i.rate days.
#' *\code{h1.rate}   represents infected individual requiring hospitalization rate at time t is h1.rate / Nt
#' *\code{q.rate }   represents infected individual self-quarantining rate at time t is q.rate / Nt
#' *\code{h2.rate}   represents self-isolated individual requiring hospitalization rate at time t is h2.rate / Nt
#' *\code{r1.rate}   represents recovery time for each infected individual is 1/ r1.rate.
#' *\code{r2.rate}   represents recovery time for each individual who accepts hospitalization is 1/ r2.rate..
#' *\code{r3.rate}   represents recovery time for each individual who is self-quarantined is is 1/ r3.rate..
#' *\code{f.rate }   represents individual who accepts hospitalization case fatality rate at time t is f.rate / Nt
#' *\code{pE}   represents probability that an arrival is exposed.
#' *\code{pSR}  represents probability that an exposed individual self-recover and turns susceptible.
#' *\code{pIim}  represents probability that an infected person is immune after recovery.
#' *\code{pQim}   represents probability that a quarantined person  is immune after recovery.
#' *\code{pHim}  represents probability that an individual who requires hospitalization recovers and  turns susceptible.


#'
#' @return A numeric matrix with 10 columns.  Row i contains the values of (t, S_t, E_t, I_t, Q_t, H_t, R_t, F_t, N_t) at time t.
#' @examples
#' para <- c(4,2,1,2,1,3,1,2,1,2,0.9, 0.3, 0.4,0.1, 0.1)
#' rSEIQHRF(N0 = 100, S0 = 99, E0 = 1, I0 = 0, Q0 = 0, H0 = 0, R0 = 0, F0 = 0, days = 100,pars = para )
#'
#' @export

rSEIQHRF <- function(N0 = 1000 , S0 = 999, E0 = 1, I0 = 0, Q0 = 0, H0 = 0, R0 = 0 ,F0 = N0 - S0 - E0 - I0 - Q0 - H0 - R0,
                  days = 100 , pars = c(10, 2, 1, 2, 1, 3, 1,3, 1, 1, 0.9, 0.3, 0.4 , 0.1, 0.1)) {

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
  output <- list(  Param = pars, Simulation_Time = res[,1], Susceptible_people = res[,2], Exposed_people = res[,3],
                   Infected_people = res[,4], Quarantined_people = res[,4], Hostipalization_ppl = res[,5],
                   Immune_people = res[,6], Fatality_case = res[,7], Total_people = res[,8])
  class(output) <- "rSEIQHRF"
  return(output)

}


#' Simulates one jump from a simple stochastic SEIQHRF epidemic model.
#'
#' \code{SEIQHRFjump} returns states of next jump given the states right now.
#'
#' @param x a numeric vector: (time, S, E, I, Q, H, R, F, N) at the previous jump.
#' @param pars a numeric vector: (a.rate, e.rate, i.rate, h1.rate, q.rate, h2.rate, r1.rate, r2.rate, r3.rate, f.rate, pE, pSR, pIim, pQim, pHim).
#'
#' @return a numeric vector: (time, S, E, I, Q, H, R, F, N)  immediately after the next jump.
#' @examples
#' A <- c(0,1000,0,0,0,0,0,0,1000)
#' B <- c(4,2,1,2,1,3,1,2,1,2,0.9, 0.3, 0.4,0.1,0.1)
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
  pSR <- pars[12]
  pIim <- pars[13]
  pQim <- pars[14]
  pHim <- pars[15]
  # Simulate the time at which the next transition occurs
  total_rate <- a.rate + e.rate * St * It / Nt + i.rate * Et  + (q.rate + h1.rate + r1.rate) * It +
    (h2.rate + r3.rate) * Qt + (r2.rate + f.rate) * Ht
  x[1] <- x[1] + rexp(1, total_rate)
  # Jump probabilities
  p1 <- a.rate * pE / total_rate
  p2 <- a.rate * (1 - pE) / total_rate
  p3 <- i.rate * pSR * Et
  p4 <- i.rate * (1 - pSR) * Et
  p5 <- q.rate * It
  p6 <- h1.rate * It
  p7 <- r1.rate * It * pIim
  p8 <- r1.rate * It * (1-pIim)
  p9 <- h2.rate * Qt
  p10 <- r3.rate * Qt * pQim
  p11 <- r3.rate * Qt * (1 - pQim)
  p12 <- r2.rate * Ht * pHim
  p13 <- r2.rate * Ht * (1 - pHim)
  p14 <- f.rate * Ht

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

  } else if  (u < p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13){
    # Person with hospitalization becomes susceptible: Ht decreases by 1, St increases by 1
    x[6] <- x[6] - 1
    x[2] <- x[2] + 1

  } else if (u < p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13 + p14){
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



