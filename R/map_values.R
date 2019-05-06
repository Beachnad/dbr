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
#' @return mode of x
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
