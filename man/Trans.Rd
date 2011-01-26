\name{Trans}
\alias{Trans}
\title{Transitions by age}
\description{Tabulates transitions by origin, destination and age.}
\usage{Trans(survey, seq.ind)}
\arguments{
  \item{survey}{Input data}
  \item{seq.ind}{Individual sequences}
}
\details{Tabulates transitions and determines mean ages.}
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
	ist <- statesequence.ind (GLHS)
	z <- Trans (GLHS,seq.ind=ist)
}
