\name{CMC.ages}
\alias{CMC.ages}
\title{From CMC to age}
\description{The function transforms a Biograph object with dates in CMC to a Biograph object with ages at events and transitions. The dates of the events included in the list of covariates are also converted to age. }
\usage{
CMC.ages(survey, covsCMC)
}
\arguments{
  \item{survey}{Biograph object}
  \item{covsCMC}{Names of covariates that are dates of events. }
}
\details{Some covariates are events. For instance, the date at marriage may be a covariate in labour force studies. The date at marriage is converted to age.}
\value{Biograph object with date of event or transition replaced by age at event or transition}
\seealso{AgeTrans}
\author{Frans Willekens}
\examples{
  data(GLHS)
 # Example 1:dates in covariates are converted to age
  covsCMC <- c("marriage","TE")
  surveyA <- CMC.ages(GLHS,covsCMC)
 # Example 2:dates in covariates are not converted to age
 covsCMC <- NULL
  surveyA <- CMC.ages(GLHS,covsCMC) 
}
