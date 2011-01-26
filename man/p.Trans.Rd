\name{p.Trans}
\alias{p.Trans}
\title{Prints transitions}
\description{Prints overview of transitions, mean ages at transition, transitions by origin, destination and age, and table with input for rate calculation (Stable)}
\usage{
p.Trans(trans, ratetable, path, filnam.trans, filnam.Stable)
}
\arguments{
  \item{trans}{Object produced by Trans object}
  \item{ratetable}{Object produced by RateTable}
  \item{path}{path to directory where output should be saved}
  \item{filnam.trans}{Name of output file with rates}
  \item{filnam.Stable}{Name of output file with Stable object}
}
\author{Frans Willekens}
\examples{
	data (GLHS)
	z<- Parameters(GLHS)
	occup <- Occup (GLHS)
	ist <- statesequence.ind (GLHS)
	trans <- Trans (GLHS,seq.ind=ist)
    ratetable <- RateTable (survey=GLHS, occup=occup,trans=trans)
    path1 <- getwd() # save output in current directory
	trans.p <- p.Trans (trans=trans,ratetable=ratetable,path=path1, filnam.trans="trans1.out",filnam.Stable="Stable.out")

	}
