#' @title Calculate Total Sum of Squares
#' @description
#' This function calculates the Total Sum of Squares (TSS) for a given numeric vector
#'
#' @param x A numeric vector
#'
#' @return A value representing the Total Sum of Squares
#' @export
#'
#' @examples
#' @examples
#' data_vector <- c(10, 15, 20, 25, 30)
#' # Calculate the TSS for the vector
#' tss(data_vector)
tss <- function(x) { # Calculate the mean of the vector
mean_x <- mean(x, na.rm = TRUE)

# Calculate the sum of the squared differences from the mean

tss <- sum((x - mean_x) ^ 2, na.rm = TRUE)

return(tss)

}
