#' Time within therapeutic range
#'
#' Calculate the TTR according to Rosendaal.
#' @param INR_meas Data.frame with (at least) the following columns:
#' INR_date (a date object with the date of the INR measurement),
#' INR (the international normalised ratio).
#' @param range.lower,range.upper The INR target range.
#' @param to,from Period over which the TTR is calculated.
#' Interpolation from earlier INRs will take place, but not from INR's after to (since we cannot see in the future).
#'
#' @export

calc_ttr <- function(INR_meas, range.lower, range.upper, to = max(INR_meas$INR_date), from = min(INR_meas$INR_date)) {
	from_tt     <- as.integer( difftime( from, min(INR_meas$INR_date), units = "days"))
	# from_tt is >0 when there are INR measurements before the start date, <0 if the start date is earlier than the INR
	INR_meas$tt <- as.integer( difftime( INR_meas$INR_date, min(INR_meas$INR_date), units = "days" ) )
	INR_meas    <- as.data.table(INR_meas[INR_meas$INR_date <= to, ])[ , list(INR = mean(INR)), by = tt]

	dates <- Vectorize(rep_len)( diff(INR_meas$tt) < 56, diff(INR_meas$tt) ) %>% unlist()
	dates[INR_meas$tt] <- TRUE

	if(from_tt > 1) {
		dates[seq_len(from_tt - 1)] <- FALSE
	}

	interpolated_INR <- with(INR_meas, {
		approx( x = tt, y = INR, xout = seq_len(max(tt))[dates] )
	})$y

	tibble(
		below_range = mean(interpolated_INR < range.lower),
		in_range    = mean(interpolated_INR <= range.upper) - below_range,
		above_range = mean(interpolated_INR > range.upper),
		mean_inr    = mean(interpolated_INR),
		min_date    = from + dplyr::first(which(dates == TRUE), default = NA) - from_tt,
		# from_tt might not necessarily be able to be interpolated, pick the later date
		max_date    = from + max(INR_meas$tt) - from_tt
	)
}
