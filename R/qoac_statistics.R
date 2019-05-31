#' Quality of Anticoagulation
#'
#' Calculate summary measures for quality of anticoagulation (QOAC) with VKA on a certain time period.
#' Calculates the time within the therapeutic range (TTR) based on the Rosendaal method,
#' the variance growth rate according to Cannegieter,
#' and the sdtINR according to Lind (2012), using calc_ttr(), calc_vgr() and calc_sdtINR() respectively.
#' INR measurements more than 56 days apart will not be interpolated.
#' @import data.table
#' @import dplyr
#' @import purrr
#' @import tibble
#' @param INR_meas Data.frame with (at least) the following columns:
#'
#' * INR_date, a date object with the date of the INR measurement
#'
#' * INR, the international normalised ratio.
#'
#' @param to,from Define the period over which the calculcation will take place; default to the latest measurement and 1 year before. Can be a vector, together with `period_id`
#' @param range.lower,range.upper Define the target range for the time within target range
#' @param period_id Names for the periods defined by _to_, _from_
#' @param interpolate_end If there are measurements after *to*, should they be used to interpolate? Defaults to FALSE.
#' @return Tibble with time below/in/above range (from 0 to 1), the mean INR, sdtINR and VGR. It gives back the original from,to values, as well as the dates actually used for the analyses (min_date, max_date).
#' @export
#' @examples
#' x <- data.frame(INR_date = as.Date("1970-01-01") + c(10, 50, 90), INR = c(1.9, 2.0, 3.6))
#' qoac_statistics(x)

qoac_statistics <- function( INR_meas,
							 to = max( INR_meas$INR_date ),
							 from = to - 365,
							 range.lower = 2, range.upper = 3,
							 period_id = NA_character_,
							 calculate = c("TTR", "VGR", "mean_INR")) {
	
	#---- Correct input data ----
	INR_meas$INR_date <- to_date(INR_meas$INR_date)
	data.table::setDT(INR_meas, key = "INR_date")
	# this automatically sorts on INR_date
	INR_meas[ INR > 10, INR := 10 ]
	INR_meas[ INR < 0.8, INR := 0.8 ]
	INR_meas$tt  <- as.integer(difftime(INR_meas$INR_date, min(INR_meas$INR_date), units = "days"))
	INR_meas$dtt <- c(diff(INR_meas$tt), 0L)
	
	#---- Determine periods ----
	periods <- data.table(
		period_id = period_id,
		from      = as.integer(from),
		to        = as.integer(to),
		per_id    = paste(from, to)
	)
	
	if("TTR" %in% calculate) {
		periods$rows <- map2(from, to, function(From, To) {
			i <- pmax( detect_index(INR_meas$INR_date, `<=`, From, .dir = "backward"), 1)
			j <- pmin( detect_index(INR_meas$INR_date, `<=`, To, .dir = "backward"), nrow(INR_meas))
			
			seq(i, j)
		})
		
		periods$TTR <- pmap(periods[ , c("from", "to", "rows")], function(...) {
			args <- list(...)
			
			x <- INR_meas[args$rows, ]
			if(any(x$dtt > 56)) {
				warning("More than 56 days between measurements, no interpolation appropriate")
				data.frame()
			} else {
				y <- approx(as.integer(x$INR_date), x$INR, seq(args$from, args$to, by = 1))$y %>%
					na.omit()
				data.table(
					below_range = mean(y < range.lower),
					in_range    = mean(y <= range.upper) - mean(y < range.lower),
					above_range = mean(y > range.upper)
				)
			}
		})
	}
	if("VGR" %in% calculate | "mean_INR" %in% calculate) {
		
		splitted_data <- periods[ INR_meas,
								  .( INR_date = i.INR_date, INR = i.INR, tt = i.tt,
								     per_id = x.per_id
								  ),
								  on = .(from <= INR_date, to >= INR_date),
								  nomatch = NULL, mult = "all", allow.cartesian = TRUE]
		
		if("VGR" %in% calculate) {
			periods$VGR <- splitted_data[ , .(
				VGR = mean( diff(INR)^2 / diff(tt / 7) )
			), by = "per_id"][ periods, .(VGR), on = "per_id", nomatch = NA]
		}
		if("mean_INR" %in% calculate) {
			periods$mean_INR <- splitted_data[ , .(
				mean_INR = mean( INR )
			), by = "per_id"][ periods, .(mean_INR), on = "per_id", nomatch = NA]
		}
	}
	
	periods %>%
		select(period_id, from, to, one_of(intersect(calculate, colnames(periods)))) %>%
		unnest()
}
