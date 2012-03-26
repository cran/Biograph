\name{cmc.as.Date}
\alias{cmc.as.Date}
\title{Converts a vector of dates in Century Month Code (CMC) to object of class 'Date'.}
\description{Converts vector of dates in Century Month Code (CMC) to object of class 'Date'.}
\usage{cmc.as.Date (x,selectday,format.out)}
\arguments{
  \item{x}{Date in Century Month Code}
  \item{selectday}{If input date is in Century Month Code (year and month), selectday gives the day of the month. Default value is one, the transition occurs on the 1st of the month. }
  \item{format.out}{Format of output dates}
  }
\value{
  \item{date}{Dates (object of class 'Date'): days since 1-1-1970 and printed as date in Gregorian calendar, e.g. "1946-03-01"}
  }
\author{Frans Willekens}
\examples{
	# CMC 555, which is March 1,1946 or 1946.167
	cmc.as.Date (x=555,selectday=1)
  }


