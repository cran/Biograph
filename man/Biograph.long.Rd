\name{Biograph.long}
\alias{Biograph.long}
\title{Converts Biograph object to long format}
\description{Converts Biograph object to long format used by e.g. the survival and eha packages.}
\usage{Biograph.long(Bdata)}
\arguments{
  \item{Bdata}{Biograph object}
}
\details{The function firsts calls Parameters if that function was not called before. The long format is produced by the reshape function.  A few variables are added. The long format produced by Biograph.long is used by the survival and eha packages. The mstate, mvna and msm packages require a different long format. Biograph.mstate, Biograph.mvna and Biograph.msm produce these formats.}
\value{
	\item{Devent}{Data frame with event data: one record for each transition}
	\item{Depisode}{Data frame with episode data: one record for each episode}
	}
\references{
Willekens (2011)}
\author{Frans Willekens}
\seealso{
	  \code{\link{Biograph.mstate}}, \code{\link{Biograph.mvna}}, \code{\link{Biograph.msm}}
	  }
\examples{
 data(GLHS)
 D <- Biograph.long (Bdata=GLHS)
}
\keyword{ long }
\keyword{ survival }
