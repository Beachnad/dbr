#' Vectors of colors for figures
#'
#' Creates different vecotrs of related colors than may be useful for figures
#'
#' @param set Character string indicating a set of colors
#' @return Vector of character strings representing the chosen set of colors, in Hex.
#' @export
#' @examples
#' plot(0, 0, type="n", xlab="", ylab="", xlim=c(0, 6), ylim=c(7, 0), yaxs="i",
#'      xaxt="n", yaxt="n", xaxs="i")
#' sex <- db_colors("sex")
#' points(seq(along=sex), rep(6,length(sex)), pch=21, bg=sex, cex=4)
#' text(seq(along=sex), rep(5, length(sex)), names(sex))
#'
#' basic <- db_colors('basic')
#' points(seq(along=basic), rep(4,length(basic)), pch=21, bg=basic, cex=4)
#' text(seq(along=basic), rep(3, length(basic)), names(basic))
#'
#' basic <- db_colors('procare')
#' points(seq(along=basic), rep(2,length(basic)), pch=21, bg=basic, cex=4)
#' text(seq(along=basic), rep(1, length(basic)), names(basic))
db_colors <- function(set=c('basic', 'procare','CCalt', 'sex')){
  basic = c('red'  = '#E01A2E',
            'blue' = '#3788BC')

  procare <- c('blue'= '#74D0D3',
               'orange' = '#F6983E')

  CCalt <- c("AJ"  = "#FFDC00",
             "B6"  = "#888888",
             "129" = "#F08080",
             "NOD" = "#0064C9",
             "NZO" = "#7FDBFF",
             "CAST"= "#2ECC40",
             "PWK" = "#FF4136",
             "WSB" = "#B10DC9")

  sex <- c(female='#FF8C8C', male='#0E9DD6')

  switch(match.arg(set),
         basic=basic,
         procare=procare,
         CCalt=CCalt,
         sex=sex)
}

neon <- function(x=c(
  'red', 'orange', 'yellow', 'green', 'blue', 'purple', 'pink'
)){
  switch(match.arg(x),
         red='#ff0000',
         orange='#ffbb00',
         yellow='#fff200',
         green='#00ff08',
         blue='#00f7ff',
         purple='#e854ff',
         pink='#ffa6f2')
}
