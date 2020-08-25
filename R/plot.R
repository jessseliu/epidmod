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
  plot(t, s, lty = 1, pch = 15, col = "burlywood1" ,xlab = " Time (days) " , ylab = "Number of people" , main = " Plot for the number of people in each group changing over time")
  lLines(i, lty = 2, pch = 16, col = "firebrick1")
  lLines(r, lty = 3, pch = 17, col = "seagreen1" )

  legend("topright", inset=.05, title="Group Type", c("Susceptible","Infected", "Recovered"),
          lty=c(1, 2,3 ), pch=c(15, 16, 17), col=c("burlywood", "firebrick1", "seagreen1"))

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
  plot(t, s, lty = 1, pch = 15, col = "burlywood1" ,xlab = " Time (days) " , ylab = "Number of people" , main = " Plot for the number of people in each group changing over time")
  lLines(e, lty = 2, pch = 16, col = "darkgoldenrod1")
  lLines(i, lty = 3, pch = 17, col = "firebrick1")
  lLines(r, lty = 4, pch = 18, col = "seagreen1" )

  legend("topright", inset=.05, title="Group Type", c("Susceptible","Exposed", "Infected", "Recovered"),
         lty=c(1, 2, 3, 4 ), pch=c(15, 16, 17, 18), col=c("burlywood", "darkgoldenrod1", "firebrick1", "seagreen1"))

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
  plot(t, s, lty = 1, col = "burlywood1" ,xlab = " Time (days) " , ylab = "Number of people" , main = " Plot for the number of people in each group changing over time")
  lLines(e, lty = 2,  col = "darkgoldenrod1")
  lLines(i, lty = 3,  col = "firebrick1")
  lLines(q, lty = 4,  col = "black")
  lLines(h, lty = 5,  col = "chocolate1")
  lLines(r, lty = 6,  col = "seagreen1" )
  lLines(f, lty = 7,  col = "brown1")

  legend("topright", inset=.05, title="Group Type", c("Susceptible","Exposed", "Infected", "Recovered"),
         lty=c(1, 2, 3, 4 ),  col=c("burlywood", "darkgoldenrod1", "firebrick1","black","chocolate1",  "seagreen1","brown1" ))

  invisible(x)
}
