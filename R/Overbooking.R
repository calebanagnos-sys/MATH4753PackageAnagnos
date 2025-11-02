#' Title
#'
#' @param N Number of seats on plane
#' @param gamma How much "pain" airline is willing to take/tolerated amount of overbooking
#' @param p Probability that a person who bought a ticket shows up
#'
#' @return Helps find the optimal amount of tickets to sell on a plane given the number of seats, the probability of a ticket holder not showing up and the airlines tolerance for pain. Creates two plots, one for the discrete distribution and one for the continuous distribution.
#' @export
#'
#' @examples
#' # ntickets(N = 200, gamma = .02, p =.95)
ntickets <- function(N, gamma, p) {
  possibletickets <- N:(1.1 * N) # Create a large range of possible ticket counts (up to 1.1 times the seats should suffice, I tried a few values, starting with N*5 but that made the x axis far too long)
  probs <- pbinom(N, size = possibletickets, prob = p)  # Probability that N or fewer people show up.
  nd <- possibletickets[which.min(abs(1 - gamma - probs))] #first number of tickets where the % of people who show up is limit is within gamma. The arguments on this one took a long time to figure out.
  z <- qnorm(1 - gamma) # Z-score that leaves a 'gamma' chance to be on the right of the normal curve
  sddisc <- sqrt(N * p * (1 - p)) # Standard deviation of people who show up (discrete)
  sdcont <- sqrt(possibletickets * p * (1 - p)) # Standard deviation of people who show up (continuous)
  nc <- (N + 0.5 + z * sddisc) / p # About how many tickets to sell using the normal distribution, divide by p to approximate tickets to sell, adding .5 for endpoint correction.
  obj_discrete <- 1 - gamma - pbinom(N, possibletickets, p) # Discrete objective function
  obj_continuous <- 1 - gamma - pnorm((N + 0.5 - possibletickets*p)/sdcont) # Discrete continuous function
  par(mfrow = c(2, 1))
  plot(y = obj_discrete, x = possibletickets, type="l",
       main = paste("Objective vs n","N =", N, ",gamma =", gamma, ", nd =", round(nd, 2),
                    "(Discrete)"),
       cex.main = .5,
       xlab="Tickets Sold", ylab="Discrete Function")
  text(x = nd,
       y = .1,
       labels = paste('ND = ',round(nd, digits = 3)),
       pos = 4,
       col = "palevioletred")
  abline(v=nd, col="palevioletred", lty=2,lwd = 2)
  plot( y =obj_continuous, x = possibletickets, type="l",main = paste("Objective vs n ","N =", N, ",gamma =", gamma, ", nc =", round(nc, 2), "(Continuous)"),
        cex.main = .5,
       xlab="Tickets Sold", ylab="Continuous Function")
  text(x = nd,
       y = .1,
       labels = paste('NC = ', (round(nc, digits = 3))),
       pos = 4,
       col = "darkslategray")
  abline(v=nc, col="darkslategray", lty=2,lwd = 2)
  return(list(
    nd = nd,
    nc = nc,
    N = N,
    p = p,
    gamma = gamma
  ))   # Return results
}


