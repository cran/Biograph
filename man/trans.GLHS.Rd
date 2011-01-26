\name{trans.GLHS}
\alias{trans.GLHS}
\title{Creates transition matrix for illness-death model for GLHS data}
\description{The function creates an illness-death model with three states: Job1, Job2+ and NoJob}
\usage{
trans.GLHS(names)
}
\arguments{
  \item{names}{The names of the three states. If the names are missing, the names Job1, Job2 and NoJob are given}
}
\value{
  \item{tmat}{The transition matrix with logical values as elements.}
     }
\examples{
	data (GLHS)
    z <- trans.GLHS()
}
