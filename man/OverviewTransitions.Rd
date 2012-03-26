\name{OverviewTransitions}
\alias{OverviewTransitions}
\title{Overview of transitions}
\description{Produces summary indicators of transitions.}
\usage{
OverviewTransitions(Bdata, seq.ind, agetrans)
}
\arguments{
  \item{Bdata}{input data}
  \item{seq.ind}{individual state sequences. If seq.ind is missing, the function calls Sequences.ind to obtain the individual state sequences.}
  \item{agetrans}{Ages at the different transitions. If agetrans is missing, the function calls AgeTrans.}
 }
\value{
 \item{Ttrans}{Number of transitions by origin and destination and censored cases by state occupied at time of censoring.}
 \item{meanage}{Mean age at transition by origin and destinatin and mean age at censoring.}}
\note{The function checks for availability of numstates (number of states). If numstates is not available, the function Parameters is called.}
\author{Frans Willekens}
\examples{
 data (GLHS)
 z <- Parameters (GLHS)
 seq.ind <- Sequences.ind (GLHS$path,namstates)
 agetrans <- AgeTrans (GLHS)
 overviewT <- OverviewTransitions (GLHS, seq.ind=seq.ind, agetrans)
}

