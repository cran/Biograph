\name{cmc_as_year}
\alias{cmc_as_year}
\title{Converts date in Century Month Code (CMC) to date in calendar year and fraction of year.}
\description{Converts date in Century Month Code (CMC) to date in year and fraction of year.}
\usage{cmc_as_year (x,selectday)}
\arguments{
  \item{x}{Date in Century Month Code}
  \item{selectday}{If input date is in Century Month Code (year and month), selectday gives the day of the month. Default value is one, the transition occurs on the 1st of the month. The argument }
  }
\value{
  \item{year.frac}{Dates in calendar year and fraction of year}}
\author{Frans Willekens}
\examples{
	# CMC 555, which is March 1,1946 or 1946.167
	cmc_as_year (x=555,selectday=1)

}


