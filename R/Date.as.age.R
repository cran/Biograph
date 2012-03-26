Date.as.age <-
function (x,format.in,born)
 {  if (missing(format.in)) stop("Date.as.age: format.in is missing")
 	if (missing (born)) stop("Date.as.age: date of birth is missing")
  	if (substr(format.in,1,1)!="%") stop ("Date.as.age: format.in is not date format")
 	if (is.character(x)) x<- as.Date(x,format.in)
    year <- Date.as.year (x,format.in)
    birth <- Date.as.year (born,format.in) 
    age <- year - birth
    return (age) 
}
