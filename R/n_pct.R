#' Number and percentages

#' @param x Vector with logicals
#' @param digits Number of decimals in percentage, defaults to 0
#' @return Number of TRUEs, and percentage of occurrences
#' @export
#' @examples
#' x <- c(TRUE, TRUE, FALSE)
#' n_pct(x)

n_pct <- function(x, digits = 0) {
	paste0( sum(x), " (", format_number(mean(x) * 100, digits), "%)" )
}
