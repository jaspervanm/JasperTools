#' Time within therapeutic range
#'
#' Calculate the TTR according to Rosendaal.
#' @param INR_meas Data.frame with (at least) the following columns:
#' INR_date (a date object with the date of the INR measurement),  
#' INR (the international normalised ratio).
#' @param range.lower,range.upper The INR target range.
#' Optionally it contains a column tt, with the time between measurements in days.
#'
#' @export

calc_ttr <- function(INR_meas, range.lower, range.upper) {
	if(!"tt" %in% colnames(INR_meas)) {
		INR_meas$tt <- as.numeric( difftime( INR_meas$INR_date, min(INR_meas$INR_date), units = "days" ) )
	}

	INR_meas <- as.data.table(INR_meas)[ , list(INR = mean(INR)), by = tt]
	
	dates <- Vectorize(rep_len)( diff(INR_meas$tt) < 56, diff(INR_meas$tt) ) %>% unlist()
	dates[INR_meas$tt] <- TRUE
	
	interpolated_INR <- with(INR_meas, {
		approx( x = tt, y = INR, xout = seq_len(max(tt))[dates] )
	})$y
	
	tibble(
		below_range = mean(interpolated_INR < range.lower),
		in_range    = mean(interpolated_INR <= range.upper) - below_range,
		above_range = mean(interpolated_INR > range.upper),
		mean_inr    = mean(interpolated_INR),
		min_date    = min( INR_meas$INR_date ),
		max_date    = max( INR_meas$INR_date )
	)
}
