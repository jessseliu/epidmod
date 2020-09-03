

#' Simulates from a simple stochastic SEIR epidemic model.
#'
#' \code{rSEIR} returns a matrix contains information about time, number of susceptible people, exposed people, infected people,
#' recover people, total number of people.
#'
#' @param N0 An integer. The population size at time 0.
#' @param I0 An integer. The initial number of infected people.
#' @param S0 An integer. The initial number of susceptible people.
#' @param R0 An integer. The initial number of recovered people.
#' @param E0 An integer. The initial number of exposed people.
#' @param days An integer. The number of days for which to simulate.
#' @param pars A numeric vector: (a.rate, e.rate, i.rate, r.rate, pE, pSR, pIm).
#' Description about pars
#' *\code{a.rate} represents rate of arrival of people into the population.
#' *\code{e.rate} represents individual effective contact rate.
#' *\code{i.rate} represents the incubation period is 1/i.rate days.
#' *\code{r.rate} represents recovery time for each infected individual is 1/ r.rate.
#' *\code{pE}     represents probability that an arrival is exposed.
#' *\code{pSR}    represents probability that an exposed individual self-recover and turns susceptible.
#' *\code{pIm}    represents probability that an infected person is immune after recovery.
#'
#' @return A numeric matrix with 6 columns.  Row i contains the values of (t, S_t, E_t, I_t, R_t, N_t) at time t.
#' @examples
#' model1 <- rSEIR(N0 = 100, I0 = 0, S0 = 99, R0 = 0, E0 = 1 , days = 100,  pars = c(1/12, 1, 1/4, 1/5, 0.3, 0.2, 0.7))
#' model1
#' print(model1)
#'
#' @export



rSEIR <- function(N0 = 1000 , I0 = 0, S0 = 999, R0 = 0, E0 = N0 - I0 - S0, days = 100 , pars = c(3, 2, 2, 1, 0.9, 0.3, 0.1)) {

  if (I0 + S0 + E0 > N0) {
    stop("There can be at most N people who are susceptible or infected or exposed")
  }
  # Infer the number of recovered people
  R0 <- N0 - I0 - S0 - E0
  # The initial state, at time = 0
  x <- c(0, S0, E0, I0, R0, N0)
  # A list in which to store the results
  res <- list()
  i <- 1
  # Simulate the next change, or jump, in the process until days have passed
  while (x[1] < days) {
    res[[i]] <- x
    x <- SEIRjump(x, pars)
    i <- i + 1
  }
  # Convert the list to a matrix
  res <- do.call(rbind, res)
  colnames(res) <- c("t", "St","Et", "It", "Rt", "Nt")
  output <- list(  Param = pars, Simulation_Time = res[,1], Susceptible_people = res[,2], Exposed_people = res[,3],
                   Infected_people = res[,4], Immune_people = res[,5], Total_people = res[,6])
  class(output) <- "rSEIR"
  return(output)
}


#' Simulates one jump from a simple stochastic SEIR epidemic model.
#'
#' \code{SEIRjump} returns states of next jump given the states right now.
#'
#' @param x A numeric vector: (time, S, E, I, R, N) at the previous jump.
#' @param pars A numeric vector: (a.rate, e.rate, i.rate, r.rate, pE, pSR, pIm).
#' @return A numeric vector: (time, S, E, I, R, N) immediately after the next jump.
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
  total_rate <- a.rate + e.rate * St * Et / Nt + i.rate * Et  + r.rate * It
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



