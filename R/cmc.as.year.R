cmc.as.year <-
function (x,selectday)
 { 	# Decimal year
	if (is.character(x)) x<- as.numeric(x)
	if (!is.numeric(x))
 	   { print ("WARNING in cmc.as.Date: x is not numeric")
 	   	 if (is.character(x)) x <- as.numeric(x) else print ("ERROR in cmc.as.Date: x not numeric and not character.")
 	   }
 	if (missing(selectday)) selectday <- 1
 	d <- cmc.as.Date(x,selectday) # d is character vector
 	year <- Date.as.year (d,format.in="%Y-%m-%d")
    return (year) 
}
