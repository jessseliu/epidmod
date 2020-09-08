#' S3 histogram for stochastic SIR model
#'
#' \code{hist} returns histogram for the number of each group given a certain time
#' @param x    the output from simulation
#' @param y    the input a certain time
#' @param nsim number of simulation to produce the histogram
#' @examples
#' model1 <- rSIR (N0 = 1000, I0 = 0, S0 = 1000, days = 300, pars = c(1/10, 1, 1/5, 0.1, 0.1))
#'hist( model1, 81, 100)
#'
#'@export
#'
hist.rSIR <- function(x, y, nsim = 100){


  # adjust the size for plotting
  oldpar <- graphics::par(mfrow = c(1, 3))
  on.exit(graphics::par(oldpar))

  # store the simulation information into vector and extract the type
  N0 <- x$Set[1]
  S0 <- x$Set[2]
  I0 <- x$Set[3]
  R0 <- x$Set[4]
  par <- x$Param
  t <- x$Simulation_Time
  s <- numeric(nsim)
  i <- numeric(nsim)
  r <- numeric(nsim)
  # To judge whether the entered time is valid
  if( y > max(t) ){
    stop("Please enter a valid time ")
  }else{
    # store the value of first simulation
    s[1] <- x$Susceptible_people[y]
    i[1] <- x$Infected_people[y]
    r[1] <- x$Immune_people[y]

    # repeat the simulation to get the histogram
    for (i in 1:nsim-1){
      model <- rSIR(N0, S0, I0, R0, pars = par )
      s[i+1] <- model$Susceptible_people[y]
      i[i+1] <- model$Infected_people[y]
      r[i+1] <- model$Immune_people[y]
    }
      hist(s,  prob = TRUE, col = "burlywood1" , main = "Susceptible")
      hist(i,  prob = TRUE,  col = "firebrick1", main = "Infected”")
      hist(r,  prob = TRUE,  col = "seagreen1", main = "Recovered")}

}



#' S3 histogram for stochastic SEIR model
#'
#' \code{hist} returns histogram for the number of each group given a certain time
#' @param x    the output from simulation
#' @param y    the input a certain time
#' @param nsim number of simulation to produce the histogram
#' @examples
#' model1 <- rSEIR(N0 = 1000, I0 = 0, S0 = 999, R0 = 0, E0 = 1,
#'                 days = 300, pars = c(1/12, 1, 1/4, 1/5, 0.3, 0.2, 0.7))
#'hist( model1, 81, 100)
#'
#'@export
#'
hist.rSEIR <- function(x, y, nsim = 100){


  # adjust the size for plotting
  oldpar <- graphics::par(mfrow = c(1, 4))
  on.exit(graphics::par(oldpar))

  # store the simulation information into vector and extract the type
  N0 <- x$Set[1]
  S0 <- x$Set[2]
  E0 <- x$Set[3]
  I0 <- x$Set[4]
  R0 <- x$Set[5]
  par <- x$Param
  t <- x$Simulation_Time
  s <- numeric(nsim)
  e <- numeric(nsim)
  i <- numeric(nsim)
  r <- numeric(nsim)
  # To judge whether the entered time is valid
  if( y > max(t) ){
    stop("Please enter a valid time ")
  }else{
    # store the value of first simulation
    s[1] <- x$Susceptible_people[y]
    e[1] <- x$Exposed_people[y]
    i[1] <- x$Infected_people[y]
    r[1] <- x$Immune_people[y]

    # repeat the simulation to get the histogram
    for (i in 1:nsim-1){
      model <- rSEIR(N0, S0, E0, I0, R0, pars = par )
      s[i+1] <- model$Susceptible_people[y]
      e[i+1] <- model$Exposed_people[y]
      i[i+1] <- model$Infected_people[y]
      r[i+1] <- model$Immune_people[y]
    }
    hist(s,  prob = TRUE, col = "burlywood1" , main = "Susceptible")
    hist(e,  prob = TRUE,  col = "darkgoldenrod1" , main = "Exposed")
    hist(i,  prob = TRUE,  col = "firebrick1", main = "Infected”")
    hist(r,  prob = TRUE,  col = "seagreen1", main = "Recovered")}

}

#' S3 histogram for stochastic SEIQR model
#'
#' \code{hist} returns histogram for the number of each group given a certain time
#' @param x    the output from simulation
#' @param y    the input a certain time
#' @param nsim number of simulation to produce the histogram
#' @examples
#' model1 <- rSEIQR(N0 = 100, S0 = 99, E0 = 1 ,I0 = 0, Q0 = 0, R0 = 0,
#'                  days = 100,  pars = c(1/12, 1, 0.5, 0.15, 0.04, 0, 0, 1))
#' hist( model1, 81, 100)
#'
#'@export
#'
hist.rSEIQR <- function(x, y, nsim = 100){


  # adjust the size for plotting
  oldpar <- graphics::par(mfrow = c(2, 3))
  on.exit(graphics::par(oldpar))

  # store the simulation information into vector and extract the type
  N0 <- x$Set[1]
  S0 <- x$Set[2]
  E0 <- x$Set[3]
  I0 <- x$Set[4]
  Q0 <- x$Set[5]
  R0 <- x$Set[6]
  par <- x$Param
  t <- x$Simulation_Time
  s <- numeric(nsim)
  e <- numeric(nsim)
  i <- numeric(nsim)
  q <- numeric(nsim)
  r <- numeric(nsim)
  # To judge whether the entered time is valid
  if( y > max(t) ){
    stop("Please enter a valid time ")
  }else{
    # store the value of first simulation
    s[1] <- x$Susceptible_people[y]
    e[1] <- x$Exposed_people[y]
    i[1] <- x$Infected_people[y]
    q[1] <- x$Quarantined_people[y]
    r[1] <- x$Immune_people[y]

    # repeat the simulation to get the histogram
    for (i in 1:nsim-1){
      model <- rSEIQR(N0, S0, E0, I0, Q0, R0, pars = par )
      s[i+1] <- model$Susceptible_people[y]
      e[i+1] <- model$Exposed_people[y]
      i[i+1] <- model$Infected_people[y]
      q[i+1] <- model$Quarantined_people[y]
      r[i+1] <- model$Immune_people[y]
    }
    hist(s,  prob = TRUE, col = "burlywood1" , main = "Susceptible")
    hist(e,  prob = TRUE,  col = "darkgoldenrod1" , main = "Exposed")
    hist(i,  prob = TRUE,  col = "firebrick1", main = "Infected”")
    hist(q,  prob = TRUE,  col = "black", main = "Quarantined")
    hist(r,  prob = TRUE,  col = "seagreen1", main = "Recovered")}

}


#' S3 histogram for stochastic SEIQHRF model
#'
#' \code{hist} returns histogram for the number of each group given a certain time
#' @param x    the output from simulation
#' @param y    the input a certain time
#' @param nsim number of simulation to produce the histogram
#' @examples
#' para <- c(4,2,1,2,1,3,1,2,1,2,0.9, 0.3, 0.4,0.1, 0.1)
#' model1 <- rSEIQHRF(N0 = 1000, S0 = 999, E0 = 1, I0 = 0, Q0 = 0, H0 = 0,
#'                    R0 = 0, F0 = 0, days = 300, pars = para )
#'
#' hist( model1, 81, 50)
#'
#'@export
#'
hist.rSEIQHRF <- function(x, y, nsim = 100){


  # adjust the size for plotting
  oldpar <- graphics::par(mfrow = c(2, 4))
  on.exit(graphics::par(oldpar))

  # store the simulation information into vector and extract the type
  N0 <- x$Set[1]
  S0 <- x$Set[2]
  E0 <- x$Set[3]
  I0 <- x$Set[4]
  Q0 <- x$Set[5]
  H0 <- x$Set[6]
  R0 <- x$Set[7]
  F0 <- x$Set[8]
  par <- x$Param
  t <- x$Simulation_Time
  s <- numeric(nsim)
  e <- numeric(nsim)
  i <- numeric(nsim)
  q <- numeric(nsim)
  h <- numeric(nsim)
  r <- numeric(nsim)
  f <- numeric(nsim)
  # To judge whether the entered time is valid
  if( y > max(t) ){
    stop("Please enter a valid time ")
  }else{
    # store the value of first simulation
    s[1] <- x$Susceptible_people[y]
    e[1] <- x$Exposed_people[y]
    i[1] <- x$Infected_people[y]
    q[1] <- x$Quarantined_people[y]
    h[1] <- x$Hostipalization_people
    r[1] <- x$Immune_people[y]
    f[1] <- x$Fatality_case[y]

    # repeat the simulation to get the histogram
    for (i in 1:nsim-1){
      model <- rSEIQHRF(N0, S0, E0, I0, Q0, H0, R0, F0, pars = par )
      s[i+1] <- model$Susceptible_people[y]
      e[i+1] <- model$Exposed_people[y]
      i[i+1] <- model$Infected_people[y]
      q[i+1] <- model$Quarantined_people[y]
      h[i+1] <- model$Hostipalization_people
      r[i+1] <- model$Immune_people[y]
      f[i+1] <- model$Fatality_case[y]
    }
    hist(s,  prob = TRUE, col = "burlywood1" , main = "Susceptible")
    hist(e,  prob = TRUE,  col = "darkgoldenrod1" , main = "Exposed")
    hist(i,  prob = TRUE,  col = "firebrick1", main = "Infected”")
    hist(q,  prob = TRUE,  col = "black", main = "Quarantined")
    hist(h,  prob = TRUE,  col = "chocolate1", main = "Requiring Hospitalization")
    hist(r,  prob = TRUE,  col = "seagreen1", main = "Recovered")
    hist(f,  prob = TRUE,  col = "brown1", main = "Fatality")}

}


