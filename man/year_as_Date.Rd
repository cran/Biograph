\name{year_as_Date}
\alias{year_as_Date}
\title{Converts a vector of dates in decimal years (calendar years and fractions of year) to object of class 'Date'.}
\description{Converts vector of calendar years and fractions of year to object of class 'Date'.}
\usage{year_as_Date (x,format.out)}
\arguments{
  \item{x}{Calendar year and fraction of year, e.g. 2012.448}
  \item{format.out}{Format of date }
  }
\value{date as object of class Date: days since 1-1-1970 and printed as date in Gregorian calendar, e.g. "1946-03-01"}
\author{Frans Willekens}
\examples{
	# d=1946.167, which is March 1,1946 
	year_as_Date (x=1946.167,format.out='\%d-\%m-\%Y')
     }


