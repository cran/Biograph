\name{Rates.ac}
\alias{Rates.ac}
\title{Estimates occurrence-exposure rates}
\description{Estimates occurrence-exposure rates starting from the Stable object. The transition rates are of the age-cohort type.}
\usage{Rates.ac (Stable)}
\arguments{
  \item{Stable}{Stable is object produced by RateTable function}
}
\value{
	\item{M}{Transition rates by origin, destination and age in standard format [M-format]}
	\item{Mcum}{Cumulative occurrence-transition rates}
	}
\details{The transition rates and the cumulative transition rates are organized as follows: row variable is age, column variable is destination state and layer variable is origin state. The age-cohort transition rates are used to construct multistate life tables.}
\author{Frans Willekens}
\examples{
#   Example 1: Transition rates between NOJOB and JOB, based on GLHS
   data(GLHS)
   z<- Parameters (GLHS)
   occup <- Occup(GLHS)
   seq.ind <- Sequences.ind (GLHS$path,namstates)
   trans <- Trans (GLHS)
   ratetable <- RateTable (occup=occup,trans=trans)
   rates <- Rates.ac (Stable=ratetable$Stable)

#   Example 2: Rates of transition between living arrangements, based on NLOG98
   data(NLOG98)
   z <- Parameters (NLOG98)
   seq.ind <- Sequences.ind (NLOG98[NLOG98$kerk=="Roman Catholic",]$path,namstates)
   occup <- Occup (NLOG98[NLOG98$kerk=="Roman Catholic",])
   trans <- Trans (Bdata=NLOG98[NLOG98$kerk=="Roman Catholic",])
   ratetable <- RateTable (occup=occup,trans=trans)
   rates <- Rates.ac (Stable=ratetable$Stable)   
   
}
