\name{Date_as_year}
\alias{Date_as_year}
\title{Converts a vector of dates (class 'Date') to decimal year (calendar year and fraction of year).}
\description{Converts a vector of dates (class 'Date') to calendar years and fractions of year.}
\usage{Date_as_year (x,format.in)}
\arguments{
  \item{x}{Date as variable of class 'Date'}
  \item{format.in}{Format of the date.}
  }
\value{
  \item{}{Calendar year and fraction of year}
  }
\author{Frans Willekens}
\examples{
	Date_as_year (x="1946-03-01")

}


