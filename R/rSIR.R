

#' Simulates from a simple stochastic SIR epidemic model.
#'
#' \code{rSIR} returns a matrix contains information about time, number of susceptible people, infected people,
#' recover people, total number of people.
#'
#' @param N0 An integer. The population size at time 0.
#' @param S0 An integer. The initial number of susceptible people.
#' @param I0 An integer. The initial number of infected people.
#' @param R0 An integer. The initial number of recovered people.
#' @param days An integer. The number of days for which to simulate.
#' @param pars A numeric vector: (a.rate, i.rate, r.rate, pI, pIm). Description about pars
#' *\code{a.rate} represents rate of arrival of people into the population.
#' *\code{i.rate} represents individual effective contact rate.
#' *\code{r.rate} represents recovery time for each infected individual is 1/ r.rate.
#' *\code{pI}     represents probability that an arrival is infected.
#' *\code{pIm}    represents probability that an infected person is immune after recovery.

#' @return A numeric matrix with 5 columns.  Row i contains the values of (t, S_t, I_t, R_t, N_t) at time t.
#' @examples
#' model1 <- rSIR (N0 = 100, S0 = 100, I0 = 0, R0 = 0,  days = 100, pars = c(1, 4, 2, 0.3, 0.7))
#' model1
#' print(model1)
#' plot(model1)
#'
#' @export

rSIR <- function(N0 = 1000 , S0 = 1000, I0 = 0  , R0 = N0 - I0 - S0,   days = 100 , pars = c(1, 2, 1, 0.1, 0.9)) {

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
  output <- list(  Set = c(N0, S0, I0, R0, days), Param = pars, Simulation_Time = res[,1], Susceptible_people = res[,2], Infected_people = res[,3],
                  Immune_people = res[,4], Total_people = res[,5])
  class(output) <- "rSIR"
  return(output)
}


#' Simulates one jump from a simple stochastic SIS epidemic model.
#'
#' \code{SIRjump} returns states of next jump given the states right now in SIR model.
#'
#' @param x A numeric vector: (time, S, I, R, N) at the previous jump.
#' @param pars A numeric vector: (alpha, pI, beta, gamma, pR).
#' @return A numeric vector: (time, S, I, R, N) immediately after the next jump.
#' @examples
#' A <- c(0,1000,0,0,1000)
#' B <- c(1,0.1,2,1,0.9)
#' SIRjump(A,B)
#' @export

SIRjump <- function (x, pars) {
  # The numbers of susceptible and infected people and the population size
  St <- x[2]
  It <- x[3]
  Rt <- x[4]
  Nt <- x[5]
  # The parameter values
  a.rate <- pars[1]
  i.rate <- pars[2]
  r.rate <- pars[3]
  pI <- pars[4]
  pIm <- pars[5]
  # Simulate the time at which the next transition occurs
  total_rate <- a.rate + i.rate * St * It / Nt + r.rate * It
  x[1] <- x[1] + rexp(1, total_rate)
  # Jump probabilities
  p1 <- a.rate * pI / total_rate
  p2 <- a.rate * (1 - pI) / total_rate
  p3 <- r.rate * pIm * It / total_rate
  p4 <- r.rate * (1 - pIm) * It / total_rate
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




