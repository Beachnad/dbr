#' Finds the mode of an array
#'
#' Given an array, will find the most common element, or mode.
#'
#' @param x List of elements
#' @param tie String of how to handle ties
#' @param na.rm Boolean, remove NA elements before evaluating
#' @return mode of \code(x)
#' @export
#' @examples
#'
#' vals <- c(2, 2, 1, 1, 3, 3)
#'
#' mode(vals, tie='first')
#' mode(vals, tie='last')
#' for (i in 1:10){
#'   print(mode(x0, tie='random'))
#' }
mode <- function(x, tie=c('first', 'last', 'random'), na.rm=FALSE) {
  tie <- tie[1]
  if (na.rm){
    x <- x[!is.na(x)]
  }

  setx <- unique(x)
  if (tie=='random') {
    tab <- tabulate(match(x, setx))
    setx[sample(which(tab==max(tab)), 1)]
  } else if (tie=='first'){
    setx[which.max(tabulate(match(x, setx)))]
  } else if (tie=='last'){
    rev(setx)[which.max(tabulate(match(x, rev(setx))))]
  }
}
