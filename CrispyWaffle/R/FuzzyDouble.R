#' FuzzyDouble
#'
#' Fuzzy double of a numeric object.
#'
#' Doubles an object with a random noise: a Gaussian error drawn
#' by \code{\link{rnorm}}.
#'
#' @param x A numeric object
#' @param mean The mean noise. Default is 0.
#' @param sd The standard deviation of the noise. Default is 1.
#'
#' @return a \code{FuzzyDouble} object which is a data.frame with
#' columns \code{x} for the input and \code{y} for the output.
#'
#' @seealso \code{\link{plot.FuzzyDouble}}, \code{\link{autoplot.FuzzyDouble}}
#' @export
FuzzyDouble <- function(x, mean = 0, sd=1) {
  #input check
  if(!is.numeric(number))
    stop("You need to put a number, you cow")
  if(!is.numeric(mean))
    stop("The mean must be numeric!!")
  #Double x and add normal error
  y <- 2 * x + stats::rnorm(n=length(x), mean = mean, sd = sd)
  # Make a data.frame
  fuzzydouble <- data.frame(x = x, y = y)
  # Make it a FuzzyMultiple object
  class(fuzzydouble) <- c("FuzzyDouble", class(fuzzydouble))
  return(fuzzydouble)
}