\name{Occup}
\alias{Occup}
\title{State occupancies and sojourn times}
\description{Determines states occupied at each age and sojourn time in each state during an one-year age interval.}
\usage{Occup(survey)}
\arguments{
  \item{survey}{input datat}
}
\value{
	state_occup = state_occup,
               st_age_1 = st_age_1[,,2],
               sjt_age_1 = sjt_age_1,
               tsjt = tsjt))
  \item{state_occup}{For each age, number of subjects by state and number censored}
  \item{st_age-1}{For each subject, state occupied at each consecutive age. This component may be used as input to the TraMineR package.}
  \item{sjt_age_1}{For each subject, sojourn time (years) in each state during age intervals of one year.}
  \item{tsjt}{Total number of years spent in each state during age intervals of one year.}
}
\author{Frans Willekens}
\note{Occup uses the AgeTrans and stringf functions.}
\seealso{Trans is used after Occup}
\examples{
  data(GLHS)
  z <- Occup (GLHS)
  }
