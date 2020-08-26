#' S3 histogram for stochastic SIR model
#'
#' \code{hist} returns histogram for each group given a certain a time or an infected rate for SIR model
#' @param x the output from simulation
#' @param y the input infection rate or a certain time
#' @param type the type of the second argument
#' @examples
#' model1 <- rSIR (N0 = 1000, I0 = 0, S0 = 1000, days = 300, pars = c(1/10, 1, 1/5, 0.1, 0.1))
#'hist( model1, 81, type = "time" )
#'hist( model1, 0.7, type = "rate" )
#'@export
#'
hist.rSIR <- function(x, y, type = c("time", "rate"), ...){

  # store the simulation value into vector and extract the type
  type <- match.arg(type)
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  rate <- i/n

  # adjust the size for plotting
  par(mfrow = c(1,3))

  # if the second input is time
  if(type == "time") {
    if( y > max(t) ){
      stop("Please enter an effective time ")
    }else{

      # select values until the given time
      s <- s[1:y]
      i <- i[1:y]
      r <- r[1:y]
      hist(s, col = "burlywood1" , main = "The histogram of Susceptible at given time")
      hist(i, col = "firebrick1", main = "The histogram of Infected at given time")
      hist(r, col = "seagreen1", main = "The histogram of Recovered at given time")
    }
  } else {

    # if the second input is rate
    if(max(rate) < y){
      cat("\n The largest infection rate is :", max(rate))
      stop("Please enter an effective infection rate ")
    }else{

      # determine the position of given rate
      l <- order(abs(rate- y))[1]

      # select values until the given rate
      s <- s[1:l]
      i <- i[1:l]
      r <- r[1:l]
      hist(s, col = "burlywood1" , main = "The histogram of Susceptible at given infection rate ")
      hist(i, col = "firebrick1", main = "The histogram of Infected at given infection rate")
      hist(r, col = "seagreen1", main = "The histogram of Recovered at given infection rate")

    }

  }

}


#' S3 histogram for stochastic SEIR model
#'
#' \code{hist} returns histogram for each group given a certain a time or an infected rate for SEIR model
#' @param x the output from simulation
#' @param y the input infection rate or a certain time
#' @param type the type of the second argument
#' @examples
#' model1 <- rSEIR(N0 = 1000, I0 = 0, S0 = 999, R0 = 0, E0 = 1 , days = 300, pars = c(1/12, 1, 1/4, 1/5, 0.3, 0.2, 0.7))
#'hist( model1, 81, type = "time" )
#'hist( model1, 0.3, type = "rate" )
#'@export
#'

hist.rSEIR <- function(x, y, type = c("time", "rate"), ...){


  # store the simulation value into vector and extract the type
  type <- match.arg(type)
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  i <- x$Infected_people
  q <- x$Quarantined_people
  h <- x$Hostipalization_people
  r <- x$Immune_people
  f <- x$Fatality_case
  n <- x$Total_people
  rate <- i/n


  # adjust the size for plotting
  par(mfrow = c(1,4))

  # if the second input is time
  if(type == "time") {
    if( y > max(t) ){
      stop("Please enter an effective time ")
    }else{

      # select values until the given time
      s <- s[1:y]
      e <- e[1:y]
      i <- i[1:y]
      r <- r[1:y]
      hist(s, col = "burlywood1" , main = "The histogram of Susceptible at given time")
      hist(e, col = "darkgoldenrod1" , main = "The histogram of Exposed at given time")
      hist(i, col = "firebrick1", main = "The histogram of Infected at given time")
      hist(r, col = "seagreen1", main = "The histogram of Recovered at given time")
    }
  } else {

    # if the second input is rate
    if(max(rate) < y){
      cat("\n The largest infection rate is :", max(rate))
      stop("Please enter an effective infection rate ")
    }else{

      # determine the position of given rate
      l <- order(abs(rate- y))[1]

      # select values until the given rate
      s <- s[1:l]
      e <- e[1:l]
      i <- i[1:l]
      r <- r[1:l]
      hist(s, col = "burlywood1" , main = "The histogram of Susceptible at given infection rate ")
      hist(e, col = "darkgoldenrod1" , main = "The histogram of Exposed at given time")
      hist(i, col = "firebrick1", main = "The histogram of Infected at given infection rate")
      hist(r, col = "seagreen1", main = "The histogram of Recovered at given infection rate")

    }

  }

}


#' S3 histogram for stochastic SEIQHRF model
#'
#' \code{hist} returns histogram for each group given a certain a time or an infected rate for SEIQHRF model
#' @param x the output from simulation
#' @param y the input infection rate or a certain time
#' @param type the type of the second argument
#' @examples
#' para <- c(4,2,1,2,1,3,1,2,1,2,0.9, 0.3, 0.4,0.1, 0.1)
#' model1 <- rSEIQHRF(N0 = 1000, S0 = 999, E0 = 1, I0 = 0, Q0 = 0, H0 = 0, R0 = 0, F0 = 0, days = 300, pars = para )
#' hist( model1, 81, type = "time" )
#' hist( model1, 0.1, type = "rate" )
#'@export
#'
hist.rSEIQHRF <- function(x, y, type = c("time", "rate"), ...){


  # store the simulation value into vector and extract the type
  type <- match.arg(type)
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  e <- x$Exposed_people
  q <- x$Quarantined_people
  h <- x$Hostipalization_people
  i <- x$Infected_people
  r <- x$Immune_people
  f <- x$Fatality_case
  n <- x$Total_people
  rate <- i/n


  # adjust the size for plotting
  par(mfrow = c(2,4))


  # if the second input is time
  if(type == "time") {
    if( y > max(t) ){
      stop("Please enter an effective time ")
    }else{

      # select values until the given time
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
      hist(q, col = "black", main = "The histogram of self-quarantined at given time")
      hist(h, col = "chocolate1", main = "The histogram of those require hospitalization at given time")
      hist(r, col = "seagreen1", main = "The histogram of Recovered at given time")
      hist(f, col = "brown1", main = "The histogram of case fatality at given time")
    }
  } else {

    # if the second input is rate
    if(max(rate) < y){
      cat("\n The largest infection rate is :", max(rate))
      stop("Please enter an effective infection rate ")
    }else{

      # determine the position of given rate
      l <- order(abs(rate- y))[1]

      # select values until the given rate
      s <- s[1:l]
      e <- e[1:l]
      i <- i[1:l]
      q <- q[1:l]
      h <- h[1:l]
      r <- r[1:l]
      f <- f[1:l]

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



