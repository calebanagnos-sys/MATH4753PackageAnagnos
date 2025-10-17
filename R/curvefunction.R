#' Plot Normal Distribution and Shade Cumulative Probability
#'
#' @param mu  Numeric. Mean of the normal distribution.
#' @param sigma Numeric. Standard deviation of the normal distribution.
#' @param a Numeric. The x-value up to which the area under the curve is shaded.
#'
#' @return
#' @export
#'
#' @examples# Plot a normal curve with mean 100, sd 15, and shade up to 120
#' myncurve(mu = 100, sigma = 15, a = 120)
myncurve = function(mu, sigma, a) {
  # Plot the normal curve
  curve(dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 3*sigma, mu + 3*sigma),
        main = paste("Normal Distribution (u =", mu, ", sigma =", sigma, ")"),
        ylab = "Density",
        xlab = "x")}

myncurve = function(mu, sigma, a) {
  # Plot the normal curve
  curve(dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 3*sigma, mu + 3*sigma),
        main = paste("Normal Distribution (u =", mu, ", sigma =", sigma, ")"),
        ylab = "Density",
        xlab = "x")

  # Shade the area from -∞ to a
  x_fill = seq(mu - 3*sigma, a, length = 1000)
  y_fill = dnorm(x_fill, mean = mu, sd = sigma)
  polygon(c(x_fill, a, mu - 3*sigma), c(y_fill, 0, 0), col = "skyblue")

  # Compute P(X <= a)
  prob = pnorm(a, mean = mu, sd = sigma)

  # Display the probability on the plot
  text(a, dnorm(a, mu, sigma)/2, paste("P(X <=", a, ") =", round(prob, 4)))

  # Return results
  return(list(mu = mu, sigma = sigma, a = a, prob = prob))
}
