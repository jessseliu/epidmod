#' S3 plot method for stochastic SIR model
#'
#'@export

plot.rSIR <- function(x,...){

  lPoints <- function(..., log, axes, frame.plot,
                      panel.first, panel.last) points(...)
  lText <- function(..., log, axes, frame.plot,
                    panel.first, panel.last) text(...)
  lLines <- function(..., log, axes, frame.plot,
                     panel.first, panel.last) lines(...)

  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  m <- matrix( c(t, s, i, r, n), nrow = length(t), ncol = 5)
  par(mar = c(4, 4, 1, 1))
  leg <- c("susceptible", "infected", "immune", "population")
  matplot(m[, 1], m[, -1], type = "l", lty = 1, lwd = 2, col = c("burlywood",  "firebrick1", "seagreen1", "cornflowerblue"),
          ylab = "Number of people", xlab = "time ( in days )")
  legend("right", legend = leg, lty = 1, lwd = 2, col = c("burlywood",  "firebrick1", "seagreen1", "cornflowerblue"), bg = "transparent")

  invisible(x)
}


#' S3 plot method for stochastic SEIR model
#'
#'@export
plot.rSEIR <- function(x,...){

  lPoints <- function(..., log, axes, frame.plot,
                      panel.first, panel.last) points(...)
  lText <- function(..., log, axes, frame.plot,
                    panel.first, panel.last) text(...)
  lLines <- function(..., log, axes, frame.plot,
                     panel.first, panel.last) lines(...)

  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  m <- matrix( c(t, s, e, i, r, n), nrow = length(t), ncol = 6)
  par(mar = c(4, 4, 1, 1))
  leg <- c("susceptible", "exposed", "infected", "immune", "population")
  matplot(m[, 1], m[, -1], type = "l", lty = 1, lwd = 2, col = c("burlywood", "darkgoldenrod1", "firebrick1", "seagreen1", "cornflowerblue"),
          ylab = "Number of people", xlab = "time ( in days )")
  legend("right", legend = leg, title="Group Type", lty = 1, lwd = 2, col = c("burlywood", "darkgoldenrod1", "firebrick1", "seagreen1", "cornflowerblue"), bg = "transparent")

  invisible(x)
}

#' S3 plot method for stochastic SEIQHRF model
#'
#'@export
plot.rSEIQHRF <- function(x,...){

  lPoints <- function(..., log, axes, frame.plot,
                      panel.first, panel.last) points(...)
  lText <- function(..., log, axes, frame.plot,
                    panel.first, panel.last) text(...)
  lLines <- function(..., log, axes, frame.plot,
                     panel.first, panel.last) lines(...)

  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  q <- x$Quarantined_people
  h <- x$Hostipalization_people
  r <- x$Immune_people
  f <- x$Fatality_case
  n <- x$Total_people
  m <- matrix(c(t, s, e, i, q, h, r, f, n), nrow = length(t), ncol = 9)
  par(mar = c(4, 4, 1, 1))
  leg <- c("susceptible", "exposed", "infected", "quarantined", "need hospitalizationo", "immune", "fatality", "population")
  matplot(m[, 1], m[, -1], type = "l", lty = 1, lwd = 2, col=c("burlywood", "darkgoldenrod1", "firebrick1","black","chocolate1",  "seagreen1","brown1", "cornflowerblue"),
          ylab = "Number of people", xlab = "time ( in days )")
  legend("right", legend = leg, title="Group Type", lty = 1, lwd = 2, col=c("burlywood", "darkgoldenrod1", "firebrick1","black","chocolate1",  "seagreen1","brown1", "cornflowerblue" ), bg = "transparent")


  invisible(x)
}
