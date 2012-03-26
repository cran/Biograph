\name{Biograph.msm}
\alias{Biograph.msm}
\title{Biograph to msm format}
\description{Converts Biograph object to input data for msm package (long format)}
\usage{
Biograph.msm(Bdata)
}
\arguments{
  \item{Bdata}{Biography object}
}
\value{Data frame with data in msm format}
\references{For msm, see Jackson, 2009. }
\author{Frans Willekens}
\examples{
 data(GLHS)
 D <- Biograph.msm (GLHS)
 # Function statetable.msm of msm package
 require(msm)
 statetable.msm(state,ID,data=D)
}
