#' Round up to nearest number
#'
#' @param x A vector.
#' @param to The nearest digit to round up to.
#'
#' @author Nathan Eastwood
roundUp <- function(x, to = 10) {
  to * (x %/% to + as.logical(x %% to))
}

#' Round down to nearest 10
#'
#' @param x A vector
#'
#' @author Nathan Eastwood
roundDown <- function(x) {
  floor(x / 10) * 10
}

#' Column Means
#'
#' Return the column means of a dataframe or matrix as a dataframe.
#'
#' @param data A matrix or dataframe.
#' @param na.rm Logical. Should missing values (including NaN) be omitted from
#'   the calculations?
#' @param ... Additional arguments to be passed to \code{\link[base]{colMeans}}.
#'
#' @author Nathan Eastwood
#'
#' @export
columnMeans <- function(data, na.rm = TRUE, ...) {
  means <- round(colMeans(data, na.rm = na.rm, ...), 2)
  data.frame(measure = names(means), Mean = means)
}
