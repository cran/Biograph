\name{year_as_age}
\alias{year_as_age}
\title{Converts decinal years to age.}
\description{Converts a vector of decimal years to ages. The function requires birth dates.}
\usage{year_as_age (x,born,format.born)}
\arguments{
  \item{x}{Vector of decimal years.}
  \item{born}{Vector of dates of birth. Class is 'Date'}
  \item{format.born}{format of dates of birth.}
  }
\value{age }
\author{Frans Willekens}
\examples{
	year_as_age (2010.578,"1990-10-30","\%Y-\%m-\%d")
    # result: 19.7506
}


