\name{YearTrans}
\alias{YearTrans}
\title{Converts dates at transition to years at transition}
\description{Converts dates at transition to decimal years at transition (years and fraction of year) }
\usage{YearTrans(Bdata)}
\arguments{
  \item{Bdata}{Biograph object}
}
\value{For each subject, years at transition}
\seealso{CMC_as_year and AgeTrans}
\author{Frans Willekens}
\note{For each subject under observation, the years of the following events are given: birth, entry into observation, end of observation, each transition.}
\examples{
  data(GLHS)
  yeartrans <- YearTrans(GLHS)
}
