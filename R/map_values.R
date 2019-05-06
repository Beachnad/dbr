#' Maps elements using a names list
#'
#' Given an array and a named list, maps values from the array
#' to the right hand side of the named list.
#'
#' @param x Array
#' @param map Named list
#' @param if_null Value to use if not present in \code(map), if
#'                if_null == NULL, then the missing values will be
#'                omitted.
#' @return array of mapped values
#' @export
#' @examples
#'
#' x <- c(2, 2, 1, 2, 1, 3, 4)
#' map <- list('1' = 'one', '2' = 'two', '2' = 'three')
#'
#' new_values <- map_values(x, map)
#'
map_values <- function(x, map, if_null=NULL){
  x1 <- map[x]
  x1[is.na(names(x1))] <- if_null
  array(unlist(x1))
}

#' Get values from named list
#'
#' Given a names list, returns an array of just the values.
#' Analagous to the names function.
#'
#' @param x Named list
#' @return Values of x as an array
#' @export
#' @examples
#'
#' x <- list('apples'=2, 'oranges'=4)
#' values(x)
#'
values <- function(x){array(unlist(x))}
