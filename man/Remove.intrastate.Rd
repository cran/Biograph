\name{Remove.intrastate}
\alias{Remove.intrastate}
\title{Removes intrastate transitions from Biograph object}
\description{Removes intrastate transitions. }
\usage{
Remove.intrastate(Bdata)
}
\arguments{
  \item{Bdata}{Biograph object}
}
\details{Intrastate transitions are removed and new characteristics of the data set are obtained.}
\author{Frans Willekens}
\value{
	\item{D}{Biograph object with intrastate transitions removed}
	\item{par}{Object created by function Parameters: parameters of Biograph object with intrastate transitions removed.}
	}
\examples{
  data (GLHS)
  Bdata2 <- Remove.intrastate (GLHS)
}
