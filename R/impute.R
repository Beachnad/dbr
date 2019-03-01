#' Replaces NA values with imputed values
#'
#' Given a data.frame and expressions, replaces
#' NaN values. The function is made in the style
#' as dplyr and as such, can use dplyr style
#' operations.
#'
#'@examples
#'
#' df <- data.frame(
#'   a = c(1, 2, NA, 20, 30),
#'   b = seq(1, 5)
#' )
#'
#' df %>% impute(a = mean(a, na.rm=TRUE))
#' df %>% impute(a = mean(b))
#' df %>% impute(a = sum(b[a > 3], na.rm=T))
#'
impute <- function(.data, ...){

  stopifnot(is.data.frame(.data) || is.list(.data) || is.environment(.data))
  cols <- as.list(substitute(list(...))[-1])
  cols <- cols[names(cols) != ""]
  for (col in names(cols)) {
    replacement <- eval(cols[[col]], .data, parent.frame())
    len = length(replacement)
    if (len == 1) {
      .data[[col]][is.na(.data[[col]])] <- replacement
    } else if (len == sum(is.na(.data[[col]]))) {
      ind <- which(is.na(.data[[col]]))
      .data[[col]][ind] <- replacement
    } else if (len == nrow(.data)) {
      .data[[col]] <- ifelse(is.na(.data[[col]]), replacement, .data[[col]])
    } else (
      warning(paste('\nThe replacement length must be either be a length of 1, the length',
                    'of the missing values, or the same length as there are observations.',
                    sep='/n'))
    )
  }
  .data
}

#' Imputes Missing Values and Replaces with Column Mean
#'
#' Function is created in the style of dplyr! Huray!
impute_mean <-function(.data, ...){
  cols <- as.character(substitute(alist(...)))[-1]
  for (c in cols){
    m <- mean(.data[[c]], na.rm = T)
    .data[[c]][is.na(.data[[c]])] <- m
  }
  .data
}

#' Imputes Missing Values and Replaces with Column Median
#'
#' Function is created in the style of dplyr! Huray!
impute_median <-function(.data, ...){
  cols <- as.character(substitute(alist(...)))[-1]
  for (c in cols){
    m <- median(.data[[c]], na.rm = T)
    .data[[c]][is.na(.data[[c]])] <- m
  }
  .data
}
