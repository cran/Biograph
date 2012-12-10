\name{state_age}
\alias{state_age}
\title{Determines for given individuals state occupied at given ages}
\description{Determines state occupied at given exact ages (birthdays). The function is used in the Occup function}
\usage{state_age (Bdata,age,ID)}
\arguments{
  \item{Bdata}{Biograph object}
  \item{age}{Vector of ages for which state occupancy should be determined} 
  \item{ID}{Vector of IDs of individuals for whom state occupancy should be determined} 
}
\value{
  \item{nam}{Names of states that can be occupied. The list includes '-' to indicate that at that age observation did not start yet and '+' to indicate that observation ended.}
  \item{state}{State occupied at given age by each individual in the study.}
  \item{state.n}{State occupied at given age: all individuals (table).}
}
\author{Frans Willekens}
\seealso{Occup}
\examples{
  data(GLHS)
  param <- Parameters(GLHS)
  agetrans <- AgeTrans(Bdata=GLHS)
  z <- state_age (Bdata=GLHS,age=c(20,30),ID=c(15,208))
  }
