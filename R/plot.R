
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



#'@export
#To find the close value in a vector
close <- function(x, value, tol=NULL){
  if(!is.null(tol)){
    x[abs(x-10) <= tol]
  } else {
    x[order(abs(x-10))]
  }
}

#'@export

#plot the histogram for each group given a certain a time or an infected rate
hist.rSIR <- function(x, y, type = c(" time ", " rate "), ...){

  type <- match.arg(type)
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  rate <- i/n

  par(mfrow = c(1,3))

  if(type == "time") {
  s <- s[1:y]
  i <- i[1:y]
  r <- r[1:y]
  hist(s, col = "burlywood1" , main = "The histogram of Susceptible at given time")
  hist(i, col = "firebrick1", main = "The histogram of Infected at given time")
  hist(r, col = "seagreen1", main = "The histogram of Recovered at given time")

  } else {
    if(max(rate) < y){
      cat("\n The largest infection rate is :", max(rate))
      stop("Please enter an effective infection rate ")
    }else{
      l <- which(rate == close(rate, y)[1])[1]
      s <- s[1:l]
      i <- i[1:l]
      r <- r[1:l]
      hist(s, col = "burlywood1" , main = "The histogram of Susceptible at given infection rate ")
      hist(i, col = "firebrick1", main = "The histogram of Infected at given infection rate")
      hist(r, col = "seagreen1", main = "The histogram of Recovered at given infection rate")

      }

  }

}
