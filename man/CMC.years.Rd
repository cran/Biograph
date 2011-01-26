\name{CMC.years}
\alias{CMC.years}
\title{CMC at transition to year of transition}
\description{The function converts dates in CMC to dates in years. The year is a real variable and includes fraction of year}
\usage{
CMC.years(survey, covsCMC)
}
\arguments{
  \item{survey}{Biograph object}
  \item{covsCMC}{Covariates that relate to events and dates at these events.}
}
\value{Biograph object with dates in CMC replaced by dates in years (real variables)}
\seealso{YearTrans}
\author{Frans Willekens}
\examples{
  data(GLHS)
  covsCMC <- c("marriage","TE")
  surveyY <- CMC.years(GLHS,covsCMC)
}
