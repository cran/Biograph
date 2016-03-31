\name{RateTable}
\alias{RateTable}
\alias{Ratetable}
\title{Table for rate calculation}
\description{Ratetable produces a table of transition counts and exposure times for the computation of transition rates (occurrence-exposure rates)}
\usage{
RateTable(Bdata,occup, trans)
}
\arguments{
  \item{Bdata}{Biograph object}
  \item{occup}{Object produced by the Occup function}
  \item{trans}{Object produced by the Trans function}
}
\details{RateTable is one of the key functions of the package. The Stable object it produces contains the data needed for the computation of transition rates by origin, destination and age}
\value{ 
    \item{Stable}{The table of transitions and exposure times}
    \item{censored_by_age}{Number of censored cases by age and state at censoring}
   }
\references{Willekens (2011)}
\author{Frans Willekens}
\examples{
 data (GLHS)
 z<- Parameters (GLHS)
 occup <- Occup(GLHS)
 ist <- Sequences.ind (GLHS$path,attr(GLHS,"param")$namstates)
 trans <- Trans (GLHS)
 w <- RateTable(GLHS,occup, trans)
}
\keyword{ Transitions }
\keyword{ Exposure }
