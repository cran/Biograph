\name{Date.as.year}
\alias{Date.as.year}
\title{Converts a vector of dates (class 'Date') to decimal year (calendar year and fraction of year).}
\description{Converts a vector of dates (class 'Date') to calendar years and fractions of year.}
\usage{Date.as.year (x,format.in)}
\arguments{
  \item{x}{Date as variable of class 'Date'}
  \item{format.in}{Format of the date.}
  }
\value{
  \item{year.frac}{Calendar year and fraction of year}
  }
\author{Frans Willekens}
\examples{
	Date.as.year (x="1946-03-01")

}


