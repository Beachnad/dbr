#' Not in
#'
#' not version of the %in% infix
#'@export

'%not_in%' <- function(x,y){!('%in%'(x,y))}
