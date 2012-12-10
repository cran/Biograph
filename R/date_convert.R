date_convert <-
function (d,format.in,selectday,format.out,born,format.born)
# born is reference age(time) to determine age (or time since reference event)
# Converts date in any format to date of class Date (R-base)
# format input data can be: CMC, year, day, age (in years), %d%b%Y 
{  if (missing(format.in)) {stop ('No date format specified for input data. Add attr(inputfile,"format.date") to input data.') } 
   if (missing(format.born)) format.born = format.in
   if (!is.character(format.in)) stop ("date_convert: input format is not character string.")
   if (format.in=="days"|format.in=="Day"|format.in=="Days") format.in="day"
   if (format.in=="years"|format.in=="Year"| format.in=="Years") format.in=="year"
   if (format.in=="ages") format.in="age"
   if (format.in=="cmc") format.in="CMC"
   if (missing(format.out)) format.out <- "%d%b%Y"  # was "%d%b%y"
   if (format.out=="days") format.out="day"
   if (format.out=="years") format.out=="year"
   if (format.out=="ages") format.out="age"
   if (is.numeric(d) & missing(format.in)) format.in="year"
   if (length(na.omit(d))==0) 
    		             {# print ("date_convert: The vector you supplied has no values.") 
        		         date <- rep(NA,length(d))
        		         return (date)}
   if (missing(born)) born=rep(0,length(d))
   if (missing(format.born)) format.born <- format.in
   if (missing(selectday)) selectday <- 1
   if (format.out=="day-mon-year") format.out <- "%d%b%Y" # read in date info in format 'ddmmmyyyy'
   if (format.out=="day-month-year") format.out <- "%d%B%Y" 
   if (format.born=="day-month-year") format.born <- "%d%B%Y" 
   format.standard <- "%Y-%m-%d"   # ISOdate
   if (format.in==format.out)
      { # print ("date_convert: format.out = format.in. ")
       return(date=d)
      }
   if (format.out=="age" & format.in!="age" & missing(born)) stop("date_convert: format.in is not age, format.out = age, but born is missing")
     
 
# age to year
   if (format.in=="age" & format.out=="year")
     {  g <- age_as_year (x=d,born=born,format.born=format.in)
     	return (year=g)
     }
# age to Date
   if (format.in=="age" & format.out==format.out)
     {  g <- age_as_Date (x=d,born=born,format.born=format.born,format.out=format.out)
     	return (date=g)
     }
# CMC to age
   if (format.in=="CMC" & format.out=="age")
     { g <- cmc_as_age (x=d,born=born,format.born=format.in)
       return(date=g$age)     	
     }
# CMC (origin=1900) to years
   if (format.in=="CMC" &format.out=="year")
      { g <- cmc_as_year (x=d,selectday=1)
       return(date=g)
      }
# Convert CMC to Date
   if (format.in=="CMC" & substr(format.out,1,1)=="%")
    {   g <- cmc_as_Date (x=d,selectday=selectday,format.out=format.out)
         return(date=g)
   }  
# Date to age
   if ((format.in=="Date" | substr(format.in,1,1)=="%") & format.out=="age")
     { g <- Date_as_age (x=d,format.in=format.in,born=born)
       return(date=g)     	
     }
# Date to cmc
   if ((format.in=="Date" | substr(format.in,1,1)=="%") & format.out=="cmc")
     { g <- Date_as_cmc (x=d,format.in=format.in)
       return(date=g$cmc)
                #  selectday=g$selectday))     	
     }
# Date to year
   if ((format.in=="Date" | substr(format.in,1,1)=="%") & format.out=="year")
     { g <- Date_as_year (x=d,format.in=format.in)
       return(date=g)     	
     }
#  year to age
   if (format.in=="year"&format.out=="age") 
     {  g <- year_as_age(x=d,born=born,format.born=format.born)
     	return (date=g)
     }
#  year to cmc
   if (format.in=="year"&format.out=="cmc") 
     {  g <- year_as_cmc(x=d)
     	return(date=g)
     }
# year to Date
   if (format.in=="year" & substr(format.out,1,1)=="%") 
     {  g <- year_as_Date (x=d,format.out=format.out)
        return(date=g)       
     }
# Date to Date
   if ((format.in=="Date" | substr(format.in,1,1)=="%") & substr(format.out,1,1)=="%")
     { g <- as.Date (x=d,format=format.in)
       gg <- format (x=g,format="%d%b%Y")
       return(date=gg)     	
     }

if (is.null(g)) print ("date_convert: Input format is probably wrong." )
if (!exists("g")) stop ("date_convert: The function cannot handle your request. Please check date formats.")
return ()
}
