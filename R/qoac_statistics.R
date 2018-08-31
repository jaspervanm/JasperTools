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
							 period_id = NA_character_) {

	From <- to_date( pmin(from, to) )
	To   <- to_date( pmax(from, to) )

	if(all(is.na(period_id))) {
		period_id <- as.character(seq_along(period_id))
	}
	periods <- tibble(
		period_id = period_id,
		from      = From,
		to        = To
	)

	INR_meas$INR_date <- to_date(INR_meas$INR_date)
	INR_meas <- as.data.table(INR_meas, key = "INR_date")
	# this automatically sorts on INR_date
	INR_meas[ INR > 10, INR := 10 ]
	INR_meas[ INR < 0.8, INR := 0.8 ]

	rows <- map2( From, To, ~(INR_meas$INR_date >= .x & INR_meas$INR_date <= .y))
	names(rows) <- period_id
	no_meas <- map_lgl(rows, ~sum( .x, na.rm = TRUE) == 0)

	if( all( no_meas ) ) {
		warning("Voor geen enkele periode INR's beschikbaar", call. = FALSE)
		return( data.frame() )
	} else if( any( no_meas ) ) {
		warning("Geen INR's beschikbaar voor periode", map2(to, from, ~paste(.x, "-", .y)), call. = FALSE)
		starters <- starters[ !no_meas ]
		stoppers <- stoppers[ !no_meas ]
		From <- From[ !no_meas ]
		To   <- To[ !no_meas ]
	}

	ittr <- pmap_dfr(
		list( R = map(rows, ~append( . , TRUE, first(which( . )))), from = From, to = To ),
		~calc_ttr(INR_meas[ R ], range.lower, range.upper, to, from ),
		# we use append so calc_ttr will be fed some extra data to interpolate from history to to
		.id = "period_id"
	) %>%
		left_join(periods, by = "period_id")

	variability <- map_dfr(rows, ~tibble(
		vgr = calc_vgr(INR_meas[ .x ]),
		sdtINR = calc_sdtINR(INR_meas [ .x ])
	), .id = "period_id") %>%
		left_join(periods, by = "period_id")

	full_join( ittr, variability, by = c("from", "to", "period_id") )
}
