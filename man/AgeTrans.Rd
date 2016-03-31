\name{AgeTrans}
\alias{AgeTrans}
\title{Ages at transition}
\description{Converts dates at transition to ages at transition}
\usage{AgeTrans(Bdata)}
\arguments{
  \item{Bdata}{Biograph object: data in Biograph format}
}
\value{
  \item{ages}{ages at transition}
  \item{ageentry}{ages at entry into observation}
  \item{agecens}{ages at end of observation (censoring)}
  \item{st_entry}{states occupied at entry into observation}
  \item{st_censoring}{states occupied at censoring}
  }
\details{
  Ages are in years, even when dates are in CMC. }
\seealso{YearTrans}
\author{Frans Willekens}
\note{The sequence of transitions in the component 'ages' is same as in the Biograph object}
\examples{
  data(GLHS)
  agetrans <- AgeTrans(Bdata=GLHS)
}


