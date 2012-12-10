\name{OverviewEpisodes}
\alias{OverviewEpisodes}
\title{Overview of episodes}
\description{Computes several indicators of the episodes in the life history data: number of episodes, types of episodes (open or closed) and total and mean durations of episodes.}
\usage{OverviewEpisodes(Bdata,seq.ind)}
\arguments{
  \item{Bdata}{input data: Biograph object}
  \item{seq.ind}{individual state sequences. If seq.ind is missing, the function calls Sequences.ind to obtain the individual state sequences.}
}
\details{Open intervals start before onset of observation (left truncated) or continue after observation ended (right censored). Closed intervals start and end during the observation period.}
\value{
  \item{n}{Sample size}
  \item{ne}{Total number of episodes}
  \item{nt}{Total number of transitions}
  \item{types}{Number of episodes by type [open. closed]}
  \item{sojourn}{Total sojourn time in each type of episode}
}
\references{Willekens (2011)}
\author{Frans Willekens}
\examples{
 data (GLHS)
 z<- Parameters (GLHS)
 seq.ind <- Sequences.ind (GLHS$path,attr(GLHS,"param")$namstates)
 z <- OverviewEpisodes(Bdata=GLHS,seq.ind=seq.ind)


}
