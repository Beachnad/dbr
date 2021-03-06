% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_colors.R
\name{db_colors}
\alias{db_colors}
\title{Vectors of colors for figures}
\usage{
db_colors(set = c("basic", "procare", "CCalt", "sex"))
}
\arguments{
\item{set}{Character string indicating a set of colors}
}
\value{
Vector of character strings representing the chosen set of colors, in Hex.
}
\description{
Creates different vecotrs of related colors than may be useful for figures
}
\examples{
plot(0, 0, type="n", xlab="", ylab="", xlim=c(0, 6), ylim=c(7, 0), yaxs="i",
     xaxt="n", yaxt="n", xaxs="i")
sex <- db_colors("sex")
points(seq(along=sex), rep(6,length(sex)), pch=21, bg=sex, cex=4)
text(seq(along=sex), rep(5, length(sex)), names(sex))

basic <- db_colors('basic')
points(seq(along=basic), rep(4,length(basic)), pch=21, bg=basic, cex=4)
text(seq(along=basic), rep(3, length(basic)), names(basic))

basic <- db_colors('procare')
points(seq(along=basic), rep(2,length(basic)), pch=21, bg=basic, cex=4)
text(seq(along=basic), rep(1, length(basic)), names(basic))
}
