\name{OverviewTransitions}
\alias{OverviewTransitions}
\alias{p.OverviewTransitions}
\title{Overview of transitions}
\description{Produces standard summary tables on transitions.}
\usage{
OverviewTransitions(survey, seq.ind, agetrans)
p.OverviewTransitions(data, path, file.in, file.out)
}
\arguments{
  \item{survey}{input data}
  \item{seq.ind}{individual state sequences}
  \item{agetrans}{Ages at the different transitions}
  \item{data}{Object produced by the OverviewTransitions function}
  \item{path}{Path to the folder where the tabulations should be saved}
  \item{file.in}{Name of input data file}
  \item{file.out}{Name of output file}
 }
\value{
 \item{Ttrans}{Number of transitions by origin and destination and censored cases by state occupied at time of censoring.}
 \item{meanage}{Mean age at transition by origin and destinatin and mean age at censoring.} 
 }
\author{Frans Willekens}
\examples{
 data (GLHS)
 z <- Parameters (GLHS)
 ist <- statesequence.ind (GLHS)
 agetrans <- AgeTrans (GLHS)
 overviewT <- OverviewTransitions (GLHS, seq.ind=ist, agetrans)

  path1 <- getwd() # save output file in current directory
  p.OverviewTransitions (overviewT,path=path1,file.in="GLHS",file.out="transitions.out")
}

