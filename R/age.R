#' Age
#'
#' Calculate a subjects age in years.
#' @param from Date (or character vector that can be parsed to Date) to start calculating age, typically date of birth.
#' @param to Date on which to determine the age.
#' @return The age in years, integer.
#' @keywords numeric format
#' @export
#' @examples
#' age("1990-11-23", Sys.Date())

age <- function(from, to) {
	from_lt <- as.POSIXlt(from)
	to_lt <- as.POSIXlt(to)

	age <- to_lt$year - from_lt$year

	ifelse(to_lt$mon < from_lt$mon |
		   	(to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
		   age - 1, age)
}
