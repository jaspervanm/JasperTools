#' Format a P value
#'
#' Formats a p value to a number of digits, or <X
#'
#' @param x Numerical vector
#' @param digits Decimals to show
#' @param small Threshold to show "smaller than X"
#' @return Character vector
#' @export
#' @examples
#' format_pval(0.0001)

format_pval <- function(x, digits = 3, small = 10^(-1 * digits)) {
	ifelse(x < small,
		   paste0("<", small),
		   round(x, digits)
	)
}
