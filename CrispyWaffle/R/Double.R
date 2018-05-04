#' Double
#' 
#' compute double of vector
#' 
#' the double is calculated by multiplying each value of vector by 2
#'
#' @param number A numerical vector
#'
#' @return A vector containing the doubles of the vector
#' @export
#'
#' @examples
#' Double(runinf(3))
Double <- function(number) {
  #input check
  if(!is.numeric(number))
    stop("You need to put a number, you cow")
  return(2L * number)
}



