\name{age_as_year}
\alias{age_as_year}
\title{Converts a vector of ages to decimal year (calendar year and fraction of year).}
\description{Converts a vector of ages (class 'numeric') to calendar years and fractions of year. The function needs dates of births. }
\usage{age_as_year(x,born,format.born)}
\arguments{
  \item{x}{vector of ages}
  \item{born}{vector of dates of birth (class 'Date')}
  \item{format.born}{format of birth dates}
  }
\value{
  \item{year}{Calendar year and fraction of year}
  }
\author{Frans Willekens}
\examples{
	age_as_year(23,"1990-10-30","\%Y-\%m-\%d")

}


