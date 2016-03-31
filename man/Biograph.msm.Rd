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
\references{
Jackson, C. (2011). Multi-state models for panel data: the msm package for R. Journal of Statistical Software, 38(8), 28 pp.

Jackson, C. (2013). msm: Multi-state Markov and hidden Markov models in continuous time. Available at the CRAN repository. 
}
\author{Frans Willekens}
\examples{
 data(GLHS)
 D <- Biograph.msm (GLHS)
 # Function statetable.msm of msm package
 require(msm)
 statetable.msm(state,ID,data=D)
}
