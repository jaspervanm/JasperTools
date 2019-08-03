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
mean_sd.numeric <- function(x, na.rm = FALSE, ...) {
	cbind(M = mean(x, na.rm = na.rm), Sd = sd(x, na.rm = na.rm)) %>%
		mean_sd(...)
}

#' @export
mean_sd.matrix <- function(x, digits = 1, ...) {
	x[] <- format_number(x, digits)
	paste0(x[, 1], " &plusmn; ", x[, 2])
}

#' @export
#' @importFrom stats median quantile
median_iqr.numeric <- function(x, na.rm = FALSE, ...) {
	cbind( M   = median(x, na.rm = na.rm),
		   p25 = quantile(x, 0.25, na.rm = na.rm),
		   p75 = quantile(x, 0.75, na.rm = na.rm)
	) %>%
		median_iqr(...)
}

#' @export
median_iqr.matrix <- function(x, digits = 1, ...) {
	x[] <- format_number(x, digits)
	paste0(x[, 1], " [", x[, 2], " -- ", x[, 3], "]")
}
