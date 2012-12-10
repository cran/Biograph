\name{Date_as_age}
\alias{Date_as_age}
\title{Converts a vector of dates (class 'Date') to ages.}
\description{Converts a vector of dates (class 'Date') to ages. Function needs birth dates.}
\usage{Date_as_age (x,format.in,born)}
\arguments{
  \item{x}{Vector of dates of class 'Date'}
  \item{format.in}{date format}
  \item{born}{Vector of dates of birth. Birth dates have the same format as the other dates.}
  }
\value{
	\item{age.sec}{Age in seconds}
	\item{age.day}{Age in days} 
	\item{age}{Age}
	\item{age.year}{Age in decimal year}
	}
\author{Frans Willekens}
\examples{
	Date_as_age ("2010-10-30","\%Y-\%m-\%d","1990-4-20")
    # result: 20.52877
}


