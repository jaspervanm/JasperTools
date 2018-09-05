#' Variance growth rate
#'
#' Calculate Variance Growth Rate according to Cannegieter (2008)
#' @param INR_meas Data.frame with (at least) the following columns:
#' INR_date (a date object with the date of the INR measurement),
#' INR (the international normalised ratio).
#' Optionally it contains a column tt, with the time between measurements in days.
#' @import data.table
#' @import dplyr
#' @export

calc_vgr <- function(INR_meas) {
	if(!"tt" %in% colnames(INR_meas)) {
		INR_meas$tt <- as.numeric( difftime( INR_meas$INR_date, min(INR_meas$INR_date), units = "days" ) )
	}
	setDT(INR_meas)

	INR_meas[ , list(INR = mean(INR)), by = tt] %>% # this removes the tt == 0
	with({
		mean( diff(INR)^2 / diff(tt / 7) )
	})
}
