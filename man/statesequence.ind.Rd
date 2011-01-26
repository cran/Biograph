\name{statesequence.ind}
\alias{statesequence.ind}
\alias{statesequences}
\title{Individual trajectories}
\description{Shows for each subject the state sequence (trajectory)}
\usage{statesequence.ind(survey)}
\arguments{
  \item{survey}{input data}
}
\details{Individual sequences are basis for exploratory sequence analysis}
\value{The value returned is a two-way table with for each subject the the sequences of states occupied between onset and end of observation}
\author{Frans Willekens}
\note{statesequence.ind uses stringf}
\note{Extract required before the statesequence.ind function}
\examples{
	data(GLHS)
	z <- Parameters (GLHS)  
	seq.ind <- statesequence.ind (GLHS)
}
