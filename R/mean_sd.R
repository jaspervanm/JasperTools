#' Summarise a numerical vector to mean (SD).
#'
#' Summarise a numerical vector to a mean and a SD,
#' or a median and an interquartile range (p25 and p75).
#'
#' @import dplyr
#' @param x Numerical vector
#' @param digits Number of decimals to use
#' @return Character vector
#' @importFrom stats sd quantile median
#' @export
#' @examples
#' mean_sd(1:10)
#' median_iqr(1:10)

mean_sd <- function(...) UseMethod("mean_sd")

#' @export
median_iqr <- function(...) UseMethod("median_iqr")

#' @export
#' @importFrom stats sd
mean_sd.numeric <- function(x, ...) {
	cbind(M = mean(x), Sd = sd(x)) %>%
		mean_sd(...)
}

#' @export
mean_sd.matrix <- function(x, digits = 1, ...) {
	x[] <- format_number(x, digits)
	paste0(x[, 1], " (", x[, 2], ")")
}

#' @export
#' @importFrom stats median quantile
median_iqr.numeric <- function(x, ...) {
	cbind(M = median(x), p25 = quantile(x, 0.25), p75 = quantile(x, 0.75)) %>%
		median_iqr(...)
}

#' @export
median_iqr.matrix <- function(x, digits = 1, ...) {
	x[] <- format_number(x, digits)
	paste0(x[, 1], " (IQR ", x[, 2], " -- ", x[, 3], ")")
}
