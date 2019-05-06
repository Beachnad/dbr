#' Not in
#'
#' not version of the %in% infix
#'@export

'%!in%' <- function(x,y){!('%in%'(x,y))}
