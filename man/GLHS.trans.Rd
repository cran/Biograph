\name{GLHS.trans}
\alias{GLHS.trans}
\title{Transition matrix of illness-death model (tmat)}
\description{The function produces the transition matrix of an illness-death model with three states: Job1, Job2+ and NoJob. The function is called by GLHS.IllnessDeath().}
\usage{
GLHS.trans(names)
}
\arguments{
  \item{names}{The names of the three states. If the names are missing, the default names Job1, Job2 and NoJob are given}
}
\value{
  \item{tmat}{The transition matrix of possible transitions in the illness-death model. A possible transition is denoted by a number.}
     }
\examples{
	data (GLHS)
	z <- GLHS.trans()
}
