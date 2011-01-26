\name{RateTable}
\alias{RateTable}
\alias{Ratetable}
\title{Table for rate calculation}
\description{Ratetable produces a table of transitions and exposure times for calculating transition rates (occurrence-exposure rates)}
\usage{
RateTable(survey, occup, trans)
}
\arguments{
  \item{survey}{Input data}
  \item{occup}{Object produced by the Occup function}
  \item{trans}{Object produced by the Trans function}
}
\details{RateTable is one of the key functions of the package. It provides in table format the data needed for determining transition rates by origin, destination and age}
\value{ 
    \item{Stable}{The table of transitions and exposure times}
    \item{censored_by_age}{Number of censored cases by age and state at censoring}
   }
\references{Willekens (2011)}
\author{Frans Willekens}
\note{The functions Occup and Trans must be invoked before the RateTable function. Otherwise an error message follows.}
\examples{
 data (GLHS)
 z<- Parameters (GLHS)
 occup <- Occup(GLHS)
 ist <- statesequence.ind (GLHS)
 trans <- Trans (GLHS,seq.ind=ist)
 w <- RateTable(GLHS, occup, trans)
}
\keyword{ Transitions }
\keyword{ Exposure }
