#' Combine an estimate and a confidence interval
#'
#' Summarise a glm, lm or a broom::tidy() object of aforementioned types to
#' an estimate with 95 percent confidence interval for every term in the object.
#'
#' @import broom
#' @import tibble
#' @importFrom stats confint glm
#' @param x Either an object from lm or glm, or a broom::tidy()-object of those types.
#' @param digits Decimals to use in estimate, defaults to 2
#' @param dash Separator between the min and max of the confidence interval. Sent to dash()
#' @param CI_text Text to preceed the confidence interval, e.g. "95pct CI ".
#' @return Named character vector.
#' @export
#' @examples
#' x <- lm(log(mpg) ~ log(disp), data = mtcars)
#' y <- glm( mpg < 20 ~ log(disp), binomial(), data = mtcars)
#' z <- broom::tidy(y, exponentiate = TRUE, conf.int = TRUE)
#' est_ci(x)
#' est_ci(y)
#' est_ci(z)

est_ci <- function(...) UseMethod("est_ci")

#' @export
est_ci.glm <- function(x, ...) {
	est_ci.tbl_df( broom::tidy(x, exponentiate = TRUE, conf.int = TRUE), ... )
}

#' @export
est_ci.lm <- function(x, ...) {
	est_ci.tbl_df( broom::tidy(x, conf.int = TRUE), ... )
}

#' @export
est_ci.data.frame <- function(x, ... ) {
	i <- colnames(x)
	j <- c("term", "estimate", "std.error", "conf.low", "conf.high")
	if(all(j %in% i)) {
		est_ci.tbl_df(x, ...)
	} else {
		warning("est_ci doesn't know how to handle this data.frame, returning NA")
		return(NA)
	}
}

#' @export
est_ci.tbl_df <- function(x, digits = 2, dash = "auto", CI_text = "", ...) {
	y <- paste0( format_number(x$estimate, digits),
				 " (", CI_text,
				 format_number(x$conf.low, digits), " ", dash(x, dash), " ",
				 format_number(x$conf.high, digits), ")" )
	names(y) <- x$term
	y
}
