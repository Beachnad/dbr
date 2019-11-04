#' Not in
#'
#' not version of the %in% infix
#'@export

'%not_in%' <- function(x,y){!('%in%'(x,y))}


#' Return default value if expression is null
#'
#' Mimics the us of ISNULL in SQL.
#'
#' @export
ifnull <- function(expr, default){
  ifelse(is.null(expr), default, expr)
}
