year.as.cmc <-
function(x)
{ if (!is.numeric(x[1])) stop(paste("year.as.Date: date is not numeric. The (first) date given is: ",x[1],sep=""))
  if (!FALSE %in% is.na(x))  # at least one element of vector is not NA
   { cmc <- x
   	 return (cmc)  } else
  { d <- year.as.Date(x)
  	cmc <- Date.as.cmc(d,format.in="%Y-%m-%d")  }
return (cmc)

}
