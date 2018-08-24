#' OR_to_RR
#'
#' Convert odds ratio to a relative risk.
#' @param OR Odds ratio to convert, as a decimal number.
#' @param p0 Baseline risk, between 0 and 1
#' @return Relative risk, as a decimal number
#' @keywords numeric format
#' @export
#' @examples
#' OR_to_RR(10, 0.1)

OR_to_RR <- function(OR, p0) {
	OR / (1 - p0 + (p0 * OR))
}
