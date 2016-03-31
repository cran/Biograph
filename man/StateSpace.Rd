\name{StateSpace}
\alias{StateSpace}
\title{Get state space}
\description{Obtains statespace from the data}
\usage{
StateSpace(d,newnamstates)
}
\arguments{
  \item{d}{Biograph object (e.g. Bdata) or vector of state sequences (e.g. Bdata$path) }
  \item{newnamstates}{The preferred sequence of states.}
}
\details{
StateSpace derives statespace from the character vector 'path' in the Biograph object. In that vector, each state is uniquely identified by a single character (ASCII). The function finds the different characters. The sequence of states is determined by characters popping up first, second, etc. You may impose a preferred sequence by specifying the argument newnamstates. If no preferred sequence exists, newnamstates is missing from the argument list. The results are stored in two variables: numstates (number of states) and namstates (names of the states). The function also returns an object identifying the absorbing state (if any). A state that is entered but not left is an absorbing state. }
\value{
 \item{namstates}{Names of the states in the state space}
 \item{absorbstates}{List of absorbing states. It is NULL if there is no absorbing state.}
}
\author{Frans Willekens}
\examples{
data(GLHS)
z<- StateSpace (GLHS)
data (NLOG98)
z<- StateSpace (NLOG98,c("H","A","C","M","K"))
z <- StateSpace (NLOG98$path)
}
