
\name{date_convert}
\alias{date_convert}
\title{Converts vector of dates from one format to another}
\description{Converts dates at transition to dates in another format or to ages}
\usage{date_convert(d,format.in,selectday,format.out,born,format.born)}
\arguments{
  \item{d}{A vector of dates to be converted}
  \item{format.in}{Format of input date}
  \item{selectday}{If input date is in CMC, selectday gives the day of the month. Default is 1}
  \item{format.out}{Format of output date}
  \item{born}{Date of birth for computation of age. Format the date of birth is format.in. May be omitted, unless date at transition need to be converted to age}
  \item{format.born}{Format of date of birth}
}
\value{
  \item{date}{Dates at transition. Object of class 'Date'(number of days since 1970-01-01)}
  \item{datep}{Dates for printing}
  }
\author{Frans Willekens}
\examples{
    data(GLHS)
    b <- date_convert(d='01/01/2011',format.in='\%m/\%d/\%Y',selectday=1,format.out='day-month-year')  
    dd <- GLHS$marriage
    fo <- 'day-mon-year'
    gb <- GLHS$born
    bb<- date_convert(d=dd,format.in="CMC",selectday=1,format.out=fo,born=gb,format.born="CMC")
    }
