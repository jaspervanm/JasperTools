#' Format a P value
#'
#' Formats a p value to a number of digits, or <X
#'
#' @param x Numerical vector
#' @param digits Digits to show
#' @param small Threshold to show "smaller than X"
#' @return Character vector
#' @export
#' @examples
#' format_pval(0.0001)

format_pval <- function(x, digits = 2, small = 10^(-1 * (digits + 1))) {
	ifelse(x < small,
		   paste0("<", small),
		   ifelse( x < 10^(-1 * digits),
		   		JasperTools::format_number(x, digits + 1),
		   		JasperTools::format_number(x, digits)
		   )
	)
}
