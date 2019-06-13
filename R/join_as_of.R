#' Wrapper for data.table to perform a join as of
#'
#' Given I find the syntax used by DT to be quite tedious to learn,
#' this function abstracts the operation of performing a join
#' as of leveraging the performance of DT.
#'
#' @param left_df The left side of the join. Should be a data frame.
#' @param right_df The right side of the join. Should be a data frame.
#' @param on What column to join on.
#' @param left_time The name of the column within left_dt to join as of.
#' @param right_time The name of the column within right_df to join as of.
#' @param direction Which direction to join as of. Should be 'forward', 'backward', or 'nearest'
#'
#' @example
#' A <- data.frame(
#'   id = c(1, 2, 3),
#'   a_time = as.Date(c("2017-02-02", "2017-04-28", "2017-05-05"))
#' )
#'
#' B <- data.frame(
#'   id = c(1, 1, 2, 2, 2),
#'   units = c(100, 204, 222, 1054, 12),
#'   b_time = as.Date(c("2017-02-02", "2017-02-28", "2017-3-05", "2017-07-01", "2017-10-01"))
#' )
#'
#' C <- join_as_of(A, B, 'id', 'a_time', 'b_time')
#'
#' @importFrom data.table as.data.table
#' @export
join_as_of <- function(left_df, right_df, on, left_time, right_time, direction='forward'){
  roll_dir <-
    if(direction == 'forward')-Inf
    else if(direction=='backward')Inf
    else if(direction=='nearest')'nearest'
    else stop("Argument direction must be either 'forward', 'backward', or 'nearest'!")

  left_df['JOINING_ID'] <- left_df[on]
  right_df['JOINING_ID'] <- right_df[on]
  right_df[on] <- NULL

  left_df['JOINING_TIME'] <- left_df[left_time]
  right_df['JOINING_TIME'] <- right_df[right_time]

  l_dt <- data.table::as.data.table(left_df)
  r_dt <- data.table::as.data.table(right_df)

  data.table::setkey(l_dt, JOINING_ID, JOINING_TIME)
  data.table::setkey(r_dt, JOINING_ID, JOINING_TIME)

  AOJ <- r_dt[l_dt, roll=roll_dir]
  AOJ[,JOINING_ID:=NULL]
  AOJ[,JOINING_TIME:=NULL]

  l_names <- names(l_dt)[-match(c('JOINING_ID', 'JOINING_TIME'), names(l_dt))]
  r_names <- names(r_dt)[-match(c('JOINING_ID', 'JOINING_TIME'), names(r_dt))]
  setcolorder(AOJ, c(l_names, r_names))

  AOJ
}
