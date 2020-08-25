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
#plot the histogram for each group given a certain a time or an infected rate for SIR model
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
    if(time > max(t)){
      stop("Please enter an effective time ")
    }else{
      s <- s[1:y]
      i <- i[1:y]
      r <- r[1:y]
      hist(s, col = "burlywood1" , main = "The histogram of Susceptible at given time")
      hist(i, col = "firebrick1", main = "The histogram of Infected at given time")
      hist(r, col = "seagreen1", main = "The histogram of Recovered at given time")
    }
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

#'@export
#plot the histogram for each group given a certain a time or an infected rate for SEIR model
hist.rSIR <- function(x, y, type = c(" time ", " rate "), ...){

  type <- match.arg(type)
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  r <- x$Immune_people
  rate <- i/n

  par(mfrow = c(1,4))

  if(type == "time") {
    if(time > max(t)){
      stop("Please enter an effective time ")
    }else{
      s <- s[1:y]
      e <- e[1:y]
      i <- i[1:y]
      r <- r[1:y]
      hist(s, col = "burlywood1" , main = "The histogram of Susceptible at given time")
      hist(e, col = "darkgoldenrod1" , main = "The histogram of Exposed at given time")
      hist(i, col = "firebrick1", main = "The histogram of Infected at given time")
      hist(q, col = "black", main = "The histogram of self-quarantined at given time")
      hist(h, col = "chocolate1", main = "The histogram of those require hospitalization at given time")
      hist(r, col = "seagreen1", main = "The histogram of Recovered at given time")
      hist(f, col = "brown1", main = "The histogram of case fatality at given time")

    }
  } else {
    if(max(rate) < y){
      cat("\n The largest infection rate is :", max(rate))
      stop("Please enter an effective infection rate ")
    }else{
      l <- which(rate == close(rate, y)[1])[1]
      s <- s[1:l]
      e <- e[1:e]
      i <- i[1:l]
      r <- r[1:l]
      hist(s, col = "burlywood1" , main = "The histogram of Susceptible at given time")
      hist(e, col = "darkgoldenrod1" , main = "The histogram of Exposed at given time")
      hist(i, col = "firebrick1", main = "The histogram of Infected at given time")
      hist(q, col = "black", main = "The histogram of self-quarantined at given time")
      hist(h, col = "chocolate1", main = "The histogram of those require hospitalization at given time")
      hist(r, col = "seagreen1", main = "The histogram of Recovered at given time")
      hist(f, col = "brown1", main = "The histogram of case fatality at given time")

    }

  }

}

#'@export
#plot the histogram for each group given a certain a time or an infected rate for SEIQHRF model
hist.rSIR <- function(x, y, type = c(" time ", " rate "), ...){

  type <- match.arg(type)
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  q <- x$Quarantined_people
  h <- x$Hostipalization_people
  r <- x$Immune_people
  f <- x$Fatality_case
  rate <- i/n

  par(mfrow = c(2,4))

  if(type == "time") {
    if(time > max(t)){
      stop("Please enter an effective time ")
    }else{
      s <- s[1:y]
      e <- e[1:y]
      i <- i[1:y]
      q <- q[1:y]
      h <- h[1:y]
      r <- r[1:y]
      f <- f[1:y]
      hist(s, col = "burlywood1" , main = "The histogram of Susceptible at given time")
      hist(e, col = "darkgoldenrod1" , main = "The histogram of Exposed at given time")
      hist(i, col = "firebrick1", main = "The histogram of Infected at given time")
      hist(r, col = "seagreen1", main = "The histogram of Recovered at given time")
    }
  } else {
    if(max(rate) < y){
      cat("\n The largest infection rate is :", max(rate))
      stop("Please enter an effective infection rate ")
    }else{
      l <- which(rate == close(rate, y)[1])[1]
      s <- s[1:l]
      e <- e[1:l]
      i <- i[1:l]
      q <- q[1:l]
      h <- h[1:l]
      r <- r[1:l]
      f <- f[1:l]
      hist(s, col = "burlywood1" , main = "The histogram of Susceptible at given infection rate ")
      hist(e, col = "darkgoldenrod1" , main = "The histogram of Exposed at given time")
      hist(i, col = "firebrick1", main = "The histogram of Infected at given infection rate")
      hist(r, col = "seagreen1", main = "The histogram of Recovered at given infection rate")

    }

  }

}

