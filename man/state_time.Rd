\name{state_time}
\alias{state_time}
\title{Determines individual state occupation times}
\description{Determines for given individuals state occupation times at all ages. The function is used in the Occup function}
\usage{state_time (Bdata,ID)}
\arguments{
  \item{Bdata}{Biograph object}
  \item{ID}{Vector of IDs of individuals for whom state occupancy should be determined} 
}
\value{
  \item{state}{Individual state occupancies at each birthday}
  \item{state.n}{State occupancies by age}
  \item{sjt_age_1}{For each subject selected: occupation time in each state during age intervals of one year (time in years).}
  \item{tsjt}{For total of selected individuals: number of years spent in each state during age intervals of one year (all ages) (time in person-years).}
}
\author{Frans Willekens and Sabine Zinn}
\seealso{Occup}
\examples{
  data(GLHS)
  param <- Parameters(GLHS)
  zz <- state_time (Bdata=GLHS,ID=c(15,208))
  }
