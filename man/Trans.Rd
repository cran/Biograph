\name{Trans}
\alias{Trans}
\title{Transitions by age}
\description{Produces tables of transitions: (1) by origin and destination and (2) by origin, destination and age. It also computes the mean ages at transition.}
\usage{Trans(Bdata)}
\arguments{
  \item{Bdata}{Biograph object}
}
\details{The function calls Sequences.ind}
\value{
	\item{Ttrans}{Total number of transitions by origin and destination.}
	\item{meanage}{Mean age of transitions by origin and destination}
	\item{trans}{Number of transitions by origin, destination and age}
	\item{states_during_interval}{Number of states occupied during an age interval of one year by an average subject.}
	}
\author{Frans Willekens}
\examples{
	data (GLHS)
	y<- Parameters(GLHS)
	z <- Trans (GLHS)
}
