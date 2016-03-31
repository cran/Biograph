\name{Biograph.mstate}
\alias{Biograph.mstate}
\title{Converts Biograph object to mstate object of class "msdata"}
\description{Converts Biograph object to input data for mstate package (long format).}
\usage{
Biograph.mstate(Bdata)
}
\arguments{
  \item{Bdata}{Biograph object}
}
\details{The procedure consists of four steps:

	a. Remove intra-state transitions. First the functions Parameters and Remove.intrastate are called. 

	b. Produce long format

	c. Create object of class "msdata" (mstate format) from long format by producing one record of input data for each possible destination from a given origin and by adding the attribute "trans". 
}
\value{Data in msdata format, used by the mstate package.}
\references{
Putter, H., M. Fiocco and R.B. Geskus (2007) Tutorial in biostatistics: competing risks and multi-state models. Statistics in Medicine, 26:2389-2430.

De Wreede, L.C., M. Fiocco and H. Putter (2010) The mstate package for estimation and prediction in non- and semi-parametric multi-state and competing risks models. Computer Methods and Programs in Biomedicine, doi:10.1016/j.cmpb.2010.01.001


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
