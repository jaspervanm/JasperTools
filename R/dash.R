dash <- function(x, dash = "auto") {
	ifelse( dash == "auto",
			ifelse( any(x$conf.low < 0), "to", "-"),
			dash )
}