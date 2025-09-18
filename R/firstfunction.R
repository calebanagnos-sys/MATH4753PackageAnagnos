#' Myfirst
#'
#' @param x A numeric vector
#'
#' @return A list with components 'x' and 'y', where y is the square of x
#' @export
#'
#' @examples
#' myfirst(1:10)
myfirst <- function(x) {

  y <- x^2
  plot(y~x)
  list(x=x, y=y)

}
