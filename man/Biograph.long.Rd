\name{Biograph.long}
\alias{Biograph.long}
\title{Converts Biograph object to long format}
\description{Converts Biograph object to long format used by e.g. the survival and eha packages.}
\usage{Biograph.long(survey)}
\arguments{
  \item{survey}{Input data}
}
\details{The functions firsts calls Extract if that function was not called before. After calling the reshape function to generate the long format, a few variables are added. The long format produced by Biograph.long is used by the survival and eha packages. The mstate, mvna and msm packages require a long, but different format. Biograph.mstate, Biograph.mvna and Biograph.msm produce these formats.}
\value{
	\item{Devent}{Event file: one record for each transition}
	\item{Depisode}{Episode file: one record for each episode}
	}
\references{
Willekens (2011)}
\author{Frans Willekens}
\seealso{
	  \code{\link{Biograph.mstate}}, \code{\link{Biograph.mvna}}, \code{\link{Biograph.msm}}
	  }
\examples{
 data(GLHS)
 D <- Biograph.long (GLHS)
}
\keyword{ long }
\keyword{ survival }
