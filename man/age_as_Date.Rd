\name{age_as_Date}
\alias{age_as_Date}
\title{Converts a vector of ages to object of class 'Date'.}
\description{Converts vector of ages to object of class 'Date'.}
\usage{age_as_Date (x,born,format.born,format.out)}
\arguments{
  \item{x}{vector of ages}
  \item{born}{vector of dates of birth}
  \item{format.born}{Format of birth dates}
  \item{format.out}{Format of output dates}
  }
\value{decimal year (year and fraction of year)}
\author{Frans Willekens}
\examples{
     age_as_Date (23.456,"1990-10-30","\%Y-\%m-\%d")
     # date is 14 April 2014
     }


