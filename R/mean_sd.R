#' Summarise a numerical vector to mean (SD).
#'
#' Summarise a numerical vector to a mean and a SD.
#'
#' @import dplyr
#' @param x Numerical vector
#' @param digits Number of decimals to use
#' @return Character vector
#' @export
#' @examples
#' mean_sd(1:10)

mean_sd <- function(...) UseMethod("mean_sd")

#' @export
mean_sd.numeric <- function(x, ...) {
	cbind(M = mean(x), Sd = sd(x)) %>%
		mean_sd(...)
}

#' @export
mean_sd.matrix <- function(x, digits = 1) {
	x[] <- format_number(x, digits)
	paste0(x[, 1], " (", x[, 2], ")")
}
