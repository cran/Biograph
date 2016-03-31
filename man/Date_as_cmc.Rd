\name{Date_as_cmc}
\alias{Date_as_cmc}
\title{Converts a vector of dates (class 'Date') to Century Month Codes.}
\description{Converts a vector of dates (class 'Date') to months elapsed since 1st january 1900.}
\usage{Date_as_cmc (x,format.in)}
\arguments{
  \item{x}{Vector of dates. The Date vector is of class 'Date'}
  \item{format.in}{Format of date}
  }
\value{
  \item{cmc}{Dates in cmc}
  \item{selectday}{Day of the month}
  }
\author{Frans Willekens}
\examples{
	Date_as_cmc("2010-10-30","\%Y-\%m-\%d")
    # cmc is 1330 and day of month is 30
}


