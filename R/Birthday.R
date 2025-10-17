#' Title
#'
#' @param x the size of the class in students
#'
#' @return the probability of two people having the same birthday in a classs of n students
#' @export
#'
#' @examples
#' birthday(x = 20:25)
birthday <- function(x)
  {
  1 - exp(lchoose(365,x) + lfactorial(x) - x*log(365))
  }
