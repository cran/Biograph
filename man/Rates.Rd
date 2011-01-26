\name{Rates}
\alias{Rates}
\title{Estimate occurrence-exposure rates}
\description{Estimates the occurrence-exposure rates from the Stable object}
\usage{
Rates(pc_ac, Stable)
}
\arguments{
  \item{pc_ac}{Type of rate: 1 = period-cohort; 2 = age-cohort}
  \item{Stable}{Stable is object produced by RateTable function}
}
\value{
	\item{M}{Transition rates by origin, destination and age in standard format [M-format]}
	\item{Mcum}{Cumulative occurrence-transition rates}
	}
\author{Frans Willekens}
\examples{
#   Example 1: Transition rates between NOJOB and JOB, based on GLHS
   data(GLHS)
   z<- Parameters (GLHS)
   occup <- Occup(GLHS)
   seq.ind <- statesequence.ind (GLHS)
   trans <- Trans (GLHS,seq.ind=seq.ind)
   ratetable <- RateTable (survey=GLHS, occup=occup,trans=trans)
   rates <- Rates (pc_ac=2,Stable=ratetable$Stable)

#   Example 2: Rates of transition between living arrangements, based on NLOG98
   data(NLOG98)
   z <- Parameters (NLOG98)
   seq.ind <- statesequence.ind (NLOG98[NLOG98$kerk=="Roman Catholic",])
   occup <- Occup (NLOG98[NLOG98$kerk=="Roman Catholic",])
   trans <- Trans (survey=NLOG98[NLOG98$kerk=="Roman Catholic",],seq.ind=seq.ind)
   ratetable <- RateTable (survey=NLOG98[NLOG98$kerk=="Roman Catholic",], occup=occup,trans=trans)
   rates <- Rates (pc_ac=2,Stable=ratetable$Stable)   
   
}
