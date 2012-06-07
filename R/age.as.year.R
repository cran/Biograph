age.as.year <-
function (x,born,format.born)
 { 	if (is.character(x)) x<- as.numeric(x)
	if (!is.numeric(x))
 	   { print ("WARNING in age.as.year: age is not numeric")
 	   	 if (is.character(x)) x <- as.numeric(x) else print ("ERROR in age.as.year: age not numeric and not character.")
 	   }
 	if (missing(format.born)) format.born <- "%Y-%m-%d" # ISO 
 	if (missing(born)) stop("ERROR: age.as.year: birth date is missing")
 	if (is.numeric(born))
 	  { if (born[1] == 0) {year <- x; return(year)} 	  	
 	  }
 	if (is.character(born)) b <-as.Date(born,format.born)
 	if (class(born)=="Date") b <-as.Date(born,format.born)
 	d <- Date.as.year (b,format.in="%Y=%m-%d")  # convert date of birth to year
 	year <- d+x
    return (year) 
}
