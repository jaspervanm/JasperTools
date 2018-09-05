#' Transform to normal
#'
#' Force a normal distribution on data.
#' Uses an empirical cumulative density function for the input data,
#' and places it on a normal distribution according to the p value.
#' The mean and sd of the standard distribution are based on the original data.
#' @param x Vector of inputs
#' @return X transformed to a normal distribution.
#' @export
#' @examples
#' transform_norm(runif(100))

transform_norm <- function(x) {
	probs <- ecdf(x)(x)
	# probabilities of 0 and 1 are problematic, result in -Inf and Inf
	probs[probs == 1] <- 0.999
	probs[probs == 0] <- 0.001
	# we need to have at least a small sd
	qnorm( probs, mean(x), max(sd(x), 0.01) )
}
