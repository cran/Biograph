\name{Biograph.mvna}
\alias{Biograph.mvna}
\title{Converts Biograph object to input data for mvna package}
\description{Converts Biograph object to long format used as input data in mvna package. }
\usage{
Biograph.mvna(Bdata)
}
\details{The function performs four operations on data in Biograph format:
	
 a. It checks whether intrastate transitions are omitted, i.e. that the diagonal elements are zero. If that is not the case, then it calls the Remove.intrastate function to remove the intrastate moves.
 
 b. It calls the Parameters function to determine the parameters associated with the Biograph object with diagonal elements removed. 
 
 c. It calls the Biograph.long function to create an object having the data in long format. 
 
 d. It adds to the data frame the variables entry and exit and it changes the variable name of the subject identification number from ID to id. }
\arguments{
  \item{Bdata}{Biograph object}
}
\note{Function Biograph.mvna uses the functions Extract, remove.intrastate and Biograph.long}
\value{
  \item{D}{Data frame of class "mvna" (only variables that are required)}
  \item{D.cov}{Data in mvna format, including covariates and some other variables}
  \item{par}{Object produced by Parameters function, including the object trans_possible: possible transitions}
  \item{cens}{Character string denoting censoring (it is "cens")}
  }
\references{
For mvna package: 
Allignol, A., J. Beyersmann and M. Schumacher (2008) mvna: An R package for the Nelson-Aalen estimator in multistate models. R Newsletter, 8(2):48-50
}
\author{Frans Willekens}
\examples{
  data(GLHS)
  D <- Biograph.mvna (GLHS)
  # Run mvna:
  require (mvna)
  na <- mvna(data=D$D,state.names=D$par$namstates,tra=D$par$trans_possible,cens.name=D$cens)
  
}
