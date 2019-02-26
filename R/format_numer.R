#' Number formatting
#'
#' Format a number to contain exactly a number of decimals.
#' @param x Numeric vector that needs to be formatted.
#' @param digits Number of decimals required. Can be a vector the same length as x, or length 1.
#' @return A character vector.
#' @keywords numeric format
#' @export
#' @examples
#' format_number(pi, 2)

format_number <- Vectorize(function(x, digits = 2) {
	trimws(format(round(x, digits), nsmall = digits))
})
