\name{StateSpace}
\alias{StateSpace}
\title{Get state space}
\description{Obtains statespace from the data}
\usage{
StateSpace(survey)
}
\arguments{
  \item{survey}{data}
}
\details{
StateSpace derives statespace from the character vector 'path' in the Biograph object. In that vector, each state is uniquely identified by a single character (ASCII). The function finds the different characters. The results are stored in two global variables: numstates (number of states) and namstates (names of the states). The function also returns an object identifying the absorbing state (if any). }
\value{
 \item{absorbstates}{absorbing states}
}
\references{willekens}
\author{Frans Willekens}
\examples{
data(GLHS)
z<- StateSpace (GLHS)
}
