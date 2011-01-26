\name{Biograph.mvna}
\alias{Biograph.mvna}
\title{Biograph object to input data for mvna package}
\description{Converts Biograph object to long format used as input data in mvna package. }
\usage{
Biograph.mvna(survey)
}
\details{The function performs four operations on data in Biograph format. First it checks whether intrastate transitions are omiited, i.e. that the diagonal elements are NA. If that is not the case, then it calls the Remove.intrastate function to remove the intrastate moves. Second, it calls the Parameters function to determine the parameters associated with the Biograph object with diagonal elements removed. Third, it calls the Biograph.long function to create an object having the data in the long format. Fourth, it adds to the data frame the variables entry and exit and it changes the variable name of the subject identification number from ID to id. }
\arguments{
  \item{survey}{Biograph object}
}
\note{Function Biograph.mvna uses the functions Extract, remove.intrastate and Biograph.long}
\value{
  \item{D}{Data in mvna format (only variables that are required)}
  \item{D.cov}{Data in mvna format, including covariates and some other variables}
  \item{par}{Object produced by Parameters function, including par$trans_possible: possible transitions}
  \item{cens}{Character string denoting censoring (it is "cens")}
  }
\references{
For mvna package: ADD}
\author{Frans Willekens}
\examples{
  data(GLHS)
  D <- Biograph.mvna (GLHS)
  # Run mvna:
  namstates.numeric <- as.character(1:length(namstates))
  require (mvna)
  na <- mvna(data=D$D,state.names=namstates.numeric,tra=D$par$trans_possible,cens.name=D$cens)
}
