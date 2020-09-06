#' S3 histogram for stochastic SIR model
#'
#' \code{hist} returns histogram for each group given a certain a time or an infected rate for SIR model
#' @param x the output from simulation
#' @param y the input infected proportion or a certain time
#' @param type the type of the second argument
#' @examples
#' model1 <- rSIR (N0 = 1000, I0 = 0, S0 = 1000, days = 300, pars = c(1/10, 1, 1/5, 0.1, 0.1))
#'hist( model1, 81, type = "time" )
#'hist( model1, 0.7, type = "proportion" )
#'@export
#'
hist.rSIR <- function(x, y, type = c("time", "proportion"), ...){

  # store the simulation value into vector and extract the type
  type <- match.arg(type)
  t <- x$Simulation_Time
  s <- x$Susceptible_people
  i <- x$Infected_people
  r <- x$Immune_people
  n <- x$Total_people
  rate <- i/n

  # adjust the size for plotting
  oldpar <- graphics::par(mfrow = c(1, 3))
  on.exit(graphics::par(oldpar))

  # if the second input is time
  if(type == "time") {
    if( y > max(t) ){
      stop("Please enter an effective time ")
    }else{

      # select values until the given time
      s <- s[1:y]
      i <- i[1:y]
      r <- r[1:y]
      hist(s,  prob = TRUE, col = "burlywood1" , main = "Susceptible")
      hist(i,  prob = TRUE,  col = "firebrick1", main = "Infected”")
      hist(r,  prob = TRUE,  col = "seagreen1", main = "Recovered")
    }
  } else {

    # if the second input is proportion
    if(max(rate) < y){
      cat("\n The largest infected proportion is :", max(rate))
      stop("Please enter an effective infected proportion ")
    }else{

      # determine the position of given proportion
      l <- order(abs(rate- y))[1]

      # select values until the givenproportion
      s <- s[1:l]
      i <- i[1:l]
      r <- r[1:l]
      hist(s,  prob = TRUE, col = "burlywood1" , main = "Susceptible")
      hist(i,  prob = TRUE,  col = "firebrick1", main = "Infected”")
      hist(r,  prob = TRUE,  col = "seagreen1", main = "Recovered")

    }

  }

}


#' S3 histogram for stochastic SEIR model
#'
#' \code{hist} returns histogram for each group given a certain a time or an infected rate for SEIR model
#' @param x the output from simulation
#' @param y the input infection proportion or a certain time
#' @param type the type of the second argument
#' @examples
#' model1 <- rSEIR(N0 = 1000, I0 = 0, S0 = 999, R0 = 0, E0 = 1 , days = 300, pars = c(1/12, 1, 1/4, 1/5, 0.3, 0.2, 0.7))
#'hist( model1, 81, type = "time" )
#'hist( model1, 0.3, type = "proportion" )
#'@export
#'

hist.rSEIR <- function(x, y, type = c("time", "proportion"), ...){


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
  oldpar <- graphics::par(mfrow = c(1, 3))
  on.exit(graphics::par(oldpar))

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

      hist(s,  prob = TRUE, col = "burlywood1" , main = "Susceptible")
      hist(e,  prob = TRUE,  col = "darkgoldenrod1" , main = "Exposed")
      hist(i,  prob = TRUE,  col = "firebrick1", main = "Infected”")
      hist(r,  prob = TRUE,  col = "seagreen1", main = "Recovered")
    }
  } else {

    # if the second input is infected proportion
    if(max(rate) < y){
      cat("\n The largest infected proportion is :", max(rate))
      stop("Please enter an effective infected proportion ")
    }else{

      # determine the position of given infected proportion
      l <- order(abs(rate- y))[1]

      # select values until the infected proportion
      s <- s[1:l]
      e <- e[1:l]
      i <- i[1:l]
      r <- r[1:l]
      hist(s,  prob = TRUE, col = "burlywood1" , main = "Susceptible")
      hist(e,  prob = TRUE,  col = "darkgoldenrod1" , main = "Exposed")
      hist(i,  prob = TRUE,  col = "firebrick1", main = "Infected”")
      hist(r,  prob = TRUE,  col = "seagreen1", main = "Recovered")

    }

  }

}


#' S3 histogram for stochastic SEIQHRF model
#'
#' \code{hist} returns histogram for each group given a certain a time or an infected rate for SEIQHRF model
#' @param x the output from simulation
#' @param y the input infection proportion or a certain time
#' @param type the type of the second argument
#' @examples
#' para <- c(4,2,1,2,1,3,1,2,1,2,0.9, 0.3, 0.4,0.1, 0.1)
#' model1 <- rSEIQHRF(N0 = 1000, S0 = 999, E0 = 1, I0 = 0, Q0 = 0, H0 = 0, R0 = 0, F0 = 0, days = 300, pars = para )
#' hist( model1, 81, type = "time" )
#' hist( model1, 0.1, type = "proportion" )
#'@export
#'
hist.rSEIQHRF <- function(x, y, type = c("time", "proportion"), ...){


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
  oldpar <- graphics::par(mfrow = c(1, 3))
  on.exit(graphics::par(oldpar))


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
      hist(s,  prob = TRUE, col = "burlywood1" , main = "Susceptible")
      hist(e,  prob = TRUE,  col = "darkgoldenrod1" , main = "Exposed")
      hist(i,  prob = TRUE,  col = "firebrick1", main = "Infected”")
      hist(q,  prob = TRUE,  col = "black", main = "Quarantined")
      hist(h,  prob = TRUE,  col = "chocolate1", main = "Requiring Hospitalization")
      hist(r,  prob = TRUE,  col = "seagreen1", main = "Recovered")
      hist(f,  prob = TRUE,  col = "brown1", main = "Fatality")
    }
  } else {

    # if the second input is infected proportion
    if(max(rate) < y){
      cat("\n The largest infected proportion is :", max(rate))
      stop("Please enter an effective infected proportion")
    }else{

      # determine the position of given infected proportion
      l <- order(abs(rate- y))[1]

      # select values until the given infected proportion
      s <- s[1:l]
      e <- e[1:l]
      i <- i[1:l]
      q <- q[1:l]
      h <- h[1:l]
      r <- r[1:l]
      f <- f[1:l]

      hist(s,  prob = TRUE, col = "burlywood1" , main = "Susceptible")
      hist(e,  prob = TRUE,  col = "darkgoldenrod1" , main = "Exposed")
      hist(i,  prob = TRUE,  col = "firebrick1", main = "Infected”")
      hist(q,  prob = TRUE,  col = "black", main = "Quarantined")
      hist(h,  prob = TRUE,  col = "chocolate1", main = "Requiring Hospitalization")
      hist(r,  prob = TRUE,  col = "seagreen1", main = "Recovered")
      hist(f,  prob = TRUE,  col = "brown1", main = "Fatality")


    }

  }

}



