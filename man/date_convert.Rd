
\name{date_convert}
\alias{date_convert}
\title{Converts vector of dates from one format to another}
\description{Converts dates at transition to dates in another format or to ages}
\usage{date_convert(d,format.in,selectday,format.out,born,format.born)}
\arguments{
  \item{d}{A vector of dates to be converted}
  \item{format.in}{Format of input date}
  \item{selectday}{If input date is in Century Month Code (year and month), selectday gives the day of the month. Default value is one, the transition occurs on the 1st of the month. The argument may be omitted.}
  \item{format.out}{Format of output date}
  \item{born}{Date of birth for computation of age. Format the date of birth is format.in. May be omitted, unless date at transition need to be converted to age.}
  \item{format.born}{Format of date of birth}
}
\value{
  \item{date}{Dates at transition. The object is of class 'Date' (see Dates in R base. Dates are represented as the number of days since 1970-01-01, with negative values for earlier dates. They are printed following the rules of the current Gregorian calendar: year, month and date, e.g. Sys.Date()}
  \item{datep}{Dates for printing}
  }
\author{Frans Willekens}
\examples{
    data(GLHS)
	b <- date_convert(d='01/01/2011',format.in='\%m/\%d/\%Y',selectday=1,format.out='day-month-year')  
    bb<- date_convert(d=GLHS$marriage,format.in='CMC',selectday=1,format.out='day-mon-year',born=GLHS$born)
    }
