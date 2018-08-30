#' Quality of Anticoagulation
#'
#' Calculate summary measures for quality of anticoagulation (QOAC) with VKA on a certain time period.
#' Calculates the time within the therapeutic range (TTR) based on the Rosendaal method,
#' the variance growth rate according to Cannegieter,
#' and the sdtINR according to Lind (2012).
#' INR measurements more than 56 days apart will not be interpolated.
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
	
	INR_meas$INR_date <- to_date(INR_meas$INR_date)
	INR_meas <- as.data.table(INR_meas, key = "INR_date")
	# this automatically sorts on INR_date

	starters <- map_dbl( From, ~detect_index( INR_meas$INR_date, `>=`, . ) )
	stoppers <- map2_dbl(  To, starters,
						   ~detect_index( unlist( INR_meas[ .y:nrow(INR_meas), "INR_date"] ), `<=`, .x, .right = TRUE ) + .y - 1 )
	no_meas  <- ( starters * stoppers == 0 | starters >= stoppers )

	if( all( no_meas ) ) {
		warning("Voor geen enkele periode INR's beschikbaar", call. = FALSE)
		return( data.frame() )
	} else if( any( no_meas ) ) {
		warning("Geen INR's beschikbaar voor periode", map2(to, from, ~paste(.x, "-", .y)), call. = FALSE)
		starters <- starters[ !no_meas ]
		stoppers <- stoppers[ !no_meas ]
	}

	rows <- map2( starters, stoppers, ~sort( c( .x, .y) ) ) %>%
		map( . , ~seq( .[1], .[2] ) )

	### iTTR
	ittr <- pmap_df(
		list(
			R = rows,
			from = From[ !no_meas ],
			to = To[ !no_meas ]
		),
		function( R, from, to ) {
			from <- as.Date( from, origin = "1970-01-01")
			to <- as.Date( to, origin = "1970-01-01")

			Data <- R %>%
				c(
					ifelse( min(.) <= 1, NA, min(.) - 1 ),
					. ,
					ifelse(interpolate_end, max(.) + 1, NA )
				) %>%
				slice( INR_meas, . ) %>%
				within(
					INR_date <- trunc( INR_date ), # let's use only days
					INR[ INR > 10] <- 10
				) %>%
				group_by( INR_date ) %>%
				dplyr::summarise( INR = mean( INR ) ) %>%
				ungroup()

			if(nrow(Data) < 2) {
				warning("Onvoldoende INR's voor een TTR")
				return( data.frame( from, to ) )
			}

			diff_date <- Data$INR_date %>%
				diff( units = "days" ) %>%
				as.integer()
			use_date <- sapply( diff_date, function(x) {
				c( TRUE, rep( x <= MAX_BETWEEN, x - 1 ) )
			} ) %>%
				unlist() %>%
				c( . , TRUE)

			trim_dates <- c(
				as.integer( from - min( Data$INR_date ) ) %>%
					max( . , 0 ) %>%
					seq_len(),
				as.integer( max( Data$INR_date ) - to ) %>%
					max( . , 0 ) %>%
					seq_len( . ) * -1 + length( use_date ) + 1
			)
			use_date[ trim_dates ] <- FALSE

			interpolated_INR <- with( Data, {
				approx( INR_date, INR,
						(min( Data$INR_date ) -1 + seq_along( use_date ) )[ use_date ] )
			})$y

			tibble(
				below_range = mean(interpolated_INR < range.lower),
				in_range    = mean(interpolated_INR <= range.upper) - below_range,
				above_range = mean(interpolated_INR > range.upper),
				mean_inr    = mean(interpolated_INR),
				from        = from,
				to          = to,
				min_date    = min( Data$INR_date ),
				max_date    = max( Data$INR_date )
			)
		} )

	variability <- pmap_df(
		list(
			R = rows,
			from = From[ !no_meas ],
			to = To[ !no_meas ]
		),
		function( R, from, to ) {
			from <- as.Date( from, origin = "1970-01-01")
			to <- as.Date( to, origin = "1970-01-01")

			INR_meas[ R , ] %>%
				mutate(
					t_inr = transform_norm( INR ) %>%
						pmax( . , 0.8) %>%
						pmin( . , 10 ),
					tt    = as.integer( difftime( INR_date, min( INR_date ), units = "days" ) )
				) %>%
				dplyr::filter( t_inr > 0, t_inr < 10 ) %>%
				do({
					reg_line <- try( lm( t_inr ~ tt, . ), silent = TRUE)

					tibble(
						sdtINR = ifelse( class(reg_line) == "lm",
										 sqrt( deviance(reg_line) / df.residual(reg_line)),
										 NA ),
						VGR = ifelse( nrow( . ) > 1,
									  sum( diff( .$INR ) ^ 2 / as.numeric( diff( .$INR_date ) / 7 ) ) / length( .$INR ) ,
									  NA ),
						from = from,
						to   = to
					)
				})
		} )

	output <- full_join( ittr, variability, by = c("from", "to") )

	if(all(is.na(period_id))) {
		return( output )
	} else {
		left_join(
			output,
			data.frame( period_id,
						from = as.Date(from, origin = "1970-01-01"),
						to = as.Date(to, origin = "1970-01-01")),
			by = c("from", "to")
		)
	}
}
