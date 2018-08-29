#' Standard deviation of transformed INR's
#'
#' Calculate sdtINR according to Lind.
#' @param INR_meas Data.frame with (at least) the following columns:
#' INR_date (a date object with the date of the INR measurement),  
#' INR (the international normalised ratio).
#' Optionally it contains a column tt, with the time between measurements in days.
#'
#' @export

calc_sdtINR <- function(INR_meas) {
	if(!"tt" %in% colnames(INR_meas)) {
		INR_meas$tt <- as.numeric( difftime( INR_meas$INR_date, min(INR_meas$INR_date), units = "days" ) )
	}

	INR_meas <- as.data.table(INR_meas)[ , list(INR = mean(INR)), by = tt]
	INR_meas$t_inr <- transform_norm(INR_meas$INR)
	
	reg_line <- try( lm( t_inr ~ tt, INR_meas ), silent = TRUE)
	
	ifelse( class(reg_line) == "lm",
			sqrt( deviance(reg_line) / df.residual(reg_line)),
			NA )
}
