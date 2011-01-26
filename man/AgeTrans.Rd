\name{AgeTrans}
\alias{AgeTrans}
\title{Ages at transition}
\description{Converts dates at transition to ages at transition}
\usage{AgeTrans(survey)}
\arguments{
  \item{survey}{Input data in Biograph format}
}
\value{
  \item{ages}{ages at transition}
  \item{ageentry}{ages at entry into observation}
  \item{agecens}{ages at censoring}
  \item{st_entry}{states occupied at entry into observation}
  \item{st_censoring}{states occupied at censoring}
  }
\seealso{CMC.ages}
\author{Frans Willekens}
\note{The sequence of transitions is same as in Biograph object}
\examples{
  data(GLHS)
  agetrans <- AgeTrans(survey=GLHS)
}


