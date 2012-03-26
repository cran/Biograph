\name{Date.as.age}
\alias{Date.as.age}
\title{Converts a vector of dates (class 'Date') to ages.}
\description{Converts a vector of dates (class 'Date') to ages. Function needs birth dates.}
\usage{Date.as.age (x,format.in,born)}
\arguments{
  \item{x}{Vector of dates of class 'Date'}
  \item{format.in}{date format}
  \item{born}{Vector of dates of birth. Birth dates have the same format as the other dates.}
  }
\value{age}
\author{Frans Willekens}
\examples{
	Date.as.age ("2010-10-30","\%Y-\%m-\%d","1990-4-20")
    # result: 20.52877
}


