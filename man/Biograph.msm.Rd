\name{Biograph.msm}
\alias{Biograph.msm}
\title{Biograph to msm format}
\description{Conferts Biograph object to input data for msm package (logn format)}
\usage{
Biograph.msm(survey)
}
\arguments{
  \item{survey}{Biography object}
}
\details{The function requires the global variable locpath determined in Extract function. }
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
