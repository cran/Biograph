\name{year.as.age}
\alias{year.as.age}
\title{Converts decinal years to age.}
\description{Converts a vector of decimal years to ages. The function requires birth dates.}
\usage{year.as.age (x,born,format.born)}
\arguments{
  \item{x}{Vector of decimal years.}
  \item{born}{Vector of dates of birth. Class is 'Date'}
  \item{format.born}{format of dates of birth.}
  }
\value{age }
\author{Frans Willekens}
\examples{
	year.as.age (2010.578,"1990-10-30","\%Y-\%m-\%d")
    # result: 19.7506
}


