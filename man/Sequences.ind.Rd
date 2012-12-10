\name{Sequences.ind}
\alias{Sequences.ind}
\title{Individual state trajectories}
\description{Shows for each subject in the sample the state sequence (trajectory) from onset to end of observation.}
\usage{Sequences.ind(path,namstatesnew)}
\arguments{
  \item{path}{State sequence}
  \item{namstatesnew}{state labels (labels of single characters)}
}
\details{Individual sequences are basis for exploratory sequence analysis}
\value{The value returned is a two-way table with for each subject the the sequences of states occupied between onset and end of observation}
\author{Frans Willekens}
\examples{
	data(GLHS)
	z <- Parameters (GLHS)  
	seq.ind <- Sequences.ind (path=GLHS$path,namstatesnew=attr(GLHS,"param")$namstates)
}
