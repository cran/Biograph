\name{Occup}
\alias{Occup}
\title{State occupancies and sojourn times}
\description{Determines states occupied at each age (birthday) and sojourn time in each state during intervals of one-year age.}
\usage{Occup(Bdata)}
\arguments{
  \item{Bdata}{Biograph object}
}
\value{
  \item{state_occup}{For each age: number of subjects by state and number of subjects censored}
  \item{st_age_1}{For each subject: state occupied at each consecutive age. This component may be used as input to the TraMineR package.}
  \item{sjt_age_1}{For each subject: sojourn time (years) in each state during age intervals of one year.}
  \item{tsjt}{For total sample: number of years spent in each state during age intervals of one year.}
}
\author{Frans Willekens}
\seealso{Trans is used after Occup}
\examples{
  data(GLHS)
  param <- Parameters(GLHS)
  z <- Occup (GLHS)
  }
