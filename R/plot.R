#' S3 plot method for stochastic SIR model
#'
#' \code{plot} returns the time series plot of simulated SIR model
#' @param x the output from simulation
#' @examples
#' model1 <- rSIR (N0 = 1000, I0 = 0, S0 = 1000, days = 300, pars = c(1/10, 1, 1/5, 0.1, 0.1))
#' plot(model1)
#'
#'@export

plot.rSIR <- function(x,...){

  lPoints <- function(..., log, axes, frame.plot,
                      panel.first, panel.last) points(...)
  lText <- function(..., log, axes, frame.plot,
                    panel.first, panel.last) text(...)
  lLines <- function(..., log, axes, frame.plot,
                     panel.first, panel.last) lines(...)

  # store the simulation value into vector
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people

  # store the simulation value into matrix
  m <- matrix( c(t, s, i, r, n), nrow = length(t), ncol = 5)

  # adjust the size of plot
  par(mar = c(4, 4, 1, 1))
  leg <- c("susceptible", "infected", "immune", "population")

  # plot for established matrix
  matplot(m[, 1], m[, -1], type = "l", lty = 1, lwd = 2, col = c("burlywood",  "firebrick1", "seagreen1", "cornflowerblue"), main = " Time series plot of simulated model ",
          ylab = "Number of people", xlab = "time ( in days )")
  legend("right", legend = leg, lty = 1, lwd = 2, col = c("burlywood",  "firebrick1", "seagreen1", "cornflowerblue"), bg = "transparent")

  invisible(x)
}


#' S3 plot method for stochastic SEIR model
#'
#' \code{plot} returns the time series plot of simulated SEIR model
#' @param x the output from simulation
#' @examples
#' model1 <- rSEIR(N0 = 1000, I0 = 0, S0 = 999, R0 = 0, E0 = 1 , days = 300, pars = c(1/12, 1, 1/4, 1/5, 0.3, 0.2, 0.7))
#' plot(model1)
#'
#'@export
plot.rSEIR <- function(x,...){

  lPoints <- function(..., log, axes, frame.plot,
                      panel.first, panel.last) points(...)
  lText <- function(..., log, axes, frame.plot,
                    panel.first, panel.last) text(...)
  lLines <- function(..., log, axes, frame.plot,
                     panel.first, panel.last) lines(...)

  # store the simulation value into vector
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people

  # store the simulation value into matrix
  m <- matrix( c(t, s, e, i, r, n), nrow = length(t), ncol = 6)

  # adjust the size of plot
  par(mar = c(4, 4, 1, 1))
  leg <- c("susceptible", "exposed", "infected", "immune", "population")

  # plot for established matrix
  matplot(m[, 1], m[, -1], type = "l", lty = 1, lwd = 2, col = c("burlywood", "darkgoldenrod1", "firebrick1", "seagreen1", "cornflowerblue"), main = "Time series plot of simulated model",
          ylab = "Number of people", xlab = "time ( in days )")
  legend("right", legend = leg, title="Group Type", lty = 1, lwd = 2, col = c("burlywood", "darkgoldenrod1", "firebrick1", "seagreen1", "cornflowerblue"), bg = "transparent")

  invisible(x)
}

#' S3 plot method for stochastic SEIQHRF model
#'
#' \code{plot} returns the time series plot of simulated SEIQHRF model
#' @param x the output from simulation
#' @examples
#' para <- c(4,2,1,2,1,3,1,2,1,2,0.9, 0.3, 0.4,0.1, 0.1)
#' model1 <- rSEIQHRF(N0 = 1000, S0 = 999, E0 = 1, I0 = 0, Q0 = 0, H0 = 0, R0 = 0, F0 = 0, days = 300, pars = para )
#' plot(model1)
#'
#'@export
plot.rSEIQHRF <- function(x,...){

  lPoints <- function(..., log, axes, frame.plot,
                      panel.first, panel.last) points(...)
  lText <- function(..., log, axes, frame.plot,
                    panel.first, panel.last) text(...)
  lLines <- function(..., log, axes, frame.plot,
                     panel.first, panel.last) lines(...)

  # store the simulation value into vector
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  q <- x$Quarantined_people
  h <- x$Hostipalization_people
  r <- x$Immune_people
  f <- x$Fatality_case
  n <- x$Total_people

  # store the simulation value into matrix
  m <- matrix(c(t, s, e, i, q, h, r, f, n), nrow = length(t), ncol = 9)

  # adjust the size of plot
  par(mar = c(4, 4, 1, 1))
  leg <- c("susceptible", "exposed", "infected", "quarantined", "need hospitalization", "immune", "fatality", "population")

  # plot for established matrix
  matplot(m[, 1], m[, -1], type = "l", lty = 1, lwd = 2, col=c("burlywood", "darkgoldenrod1", "lightpink","black","chocolate1",  "seagreen1","brown1", "cornflowerblue"),  main = "Time series plot of simulated model",
          ylab = "Number of people", xlab = "time ( in days )")
  legend("right", legend = leg, title="Group Type", lty = 1, lwd = 2, col=c("burlywood", "darkgoldenrod1", "lightpink","black","chocolate1",  "seagreen1","brown1", "cornflowerblue" ), bg = "transparent")


  invisible(x)
}
