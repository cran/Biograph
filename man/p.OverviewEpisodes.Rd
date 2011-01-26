\name{p.OverviewEpisodes}
\alias{p.OverviewEpisodes}
\alias{OverviewEpisodes}
\title{Overview of episodes}
\description{Produces a number of standard tables that summarize information on open and closed episodes. The function print.OverviewEpisodes tabulates summary measures on the episodes in the life history data. The tabulations include the number of episodes, the types of episodes (open or closed) and the durations of episodes.}
\usage{
p.OverviewEpisodes(survey, seq.ind, file.in,path,file.out)
}
\arguments{
  \item{survey}{input data: Biograph object}
  \item{seq.ind}{individual state sequences}
  \item{file.in}{Data file used as input}
  \item{path}{path to directory where output should be stored}
  \item{file.out}{name of output file. The table is saved as a text file}
}
\details{Open intervals start before onset of observation (left truncated) or end after observation ended (right censored). Closed intervals start and end during the observation period. 
	The function print.OverviewEpisodes uses the sink function of the R base. The function diverts R output to a file connection. The use of sink comes with a number of pitfalls. The reader is referred to the R documentation.}
\references{Willekens (2011)}
\author{Frans Willekens}
\examples{
 data (GLHS)
 z<- Parameters (GLHS)
 ist <- statesequence.ind (GLHS)
 path1 <- getwd() # path to current directory
 z <- p.OverviewEpisodes (survey=GLHS,seq.ind=ist,file.in="data.in",path=path1,file.out="TabEpisodes.out")


}
