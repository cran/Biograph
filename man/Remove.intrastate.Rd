\name{Remove.intrastate}
\alias{Remove.intrastate}
\title{Remove intrastate transitions from Biograph object}
\description{Removes intrastate transitions. }
\usage{
Remove.intrastate(survey)
}
\arguments{
  \item{survey}{Biograph object}
}
\details{Intrastate transitions are removed and characteristics of the resulting data set are computed.}
\author{Frans Willekens}
\value{
	\item{D}{Biograph object with intrastate transitions removed}
	\item{par}{Object created by function Parameters: parameters of Biograph object with intrastate transitions removed.}
	}
\examples{
  data (GLHS)
  survey2 <- Remove.intrastate (GLHS)
}
