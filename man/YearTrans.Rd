\name{YearTrans}
\alias{YearTrans}
\title{Years at transition}
\description{Converts dates at transition to years at transition. Year is given as real number, i.e. with fraction of year.}
\usage{YearTrans(survey)}
\arguments{
  \item{survey}{Input data}
}
\value{Years at transition
	}
\seealso{CMC.years}
\author{Frans Willekens}
\note{For each subject under observation, the years are given for birth, entry into observation, censoring and for the sequence of transitions is same as in Biograph object.}
\examples{
  data(GLHS)
  z <- Parameters(GLHS)
  yeartrans <- YearTrans(GLHS)
}
