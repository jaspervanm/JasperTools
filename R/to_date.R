
to_date <- function(x) {
	switch(class(x)[1],
		   "character" = as.Date(x),
		   "integer"   = as.Date(x, origin = "1970-01-01"),
		   "numeric"   = as.Date(x, origin = "1970-01-01"),
		   "Date"	   = x,
		   "POSIXct"   = as.Date(x, tz = "Europe/Amsterdam"),
		   "POSIXlt"   = as.Date(x, tz = "Europe/Amsterdam"),
		   NA )
}
