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
\value{Biograph object with intrastate transitions removed. The "param" attribute is adjusted. }
\examples{
  data (GLHS)
  Bdata2 <- Remove.intrastate (GLHS)
}
