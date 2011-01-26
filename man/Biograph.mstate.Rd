\name{Biograph.mstate}
\alias{Biograph.mstate}
\title{Biograph to mstate object of class "msdata"}
\description{Converts Biograph object to input data for mstate package.}
\usage{
Biograph.mstate(survey)
}
\arguments{
  \item{survey}{Biograph object}
}
\details{The procedure consists of four steps:
	a. Remove intra-state transitions. First the functions Extract and Remove_intrastate are called. 
	b. Produce long format
	c. Create msdata format (mstate format) from long format by producing one record of input data for each possible destination from a given origin and by adding the attribute "trans". 
}
\value{Data in msdata format, used by the mstate package.}
\references{
ADD
}
\author{Frans Willekens}
\examples{
  data (GLHS)
  D <- Biograph.mstate (GLHS)
  # Function events of mstate package
  library (mstate)
  events (D)
  s <- coxph(Surv(Tstart,Tstop,status)~edu+sex,data=D,method="breslow")
}
\keyword{ msdata }
\keyword{ remove.intrastate }
