\name{Sequences.ind.0}
\alias{Sequences.ind.0}
\title{Sorts dates in ascending order and generates state sequence (path).}
\description{Sorts dates in ascending order: earlier dates come first. The function also generates state sequences (life path or career path). The function is used in generating a Biograph object from raw data. The state sequence is the path variable of the Biograph object.}
\usage{Sequences.ind.0 (d,namstates,absorb)}
\arguments{
  \item{d}{Vector of dates}
  \item{namstates}{state labels (labels of single characters)}
  \item{absorb}{Vector of absorbing states}
  }
\value{
  \item{namstates}{Labels of the states in the state space.}
  \item{d}{Sorted dates}
  \item{sequence}{Vector of state sequences.}
  \item{path}{Character variable of state sequences. This variable is the 'path' variable in the Biograph object.}
  \item{ns}{Number of states occupied between onset and end of observation. This variable is included in Biograph object.}
  }
\author{Frans Willekens}
\examples{
	d <- c(300,250,340)  # dates in CMC
	f<- Sequences.ind.0 (d,c('A','B','C'))
   }


