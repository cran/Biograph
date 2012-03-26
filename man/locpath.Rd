\name{locpath}
\alias{locpath}
\title{Determines location of state sequence in Biograph object}
\description{Determines location of state sequence  (path variable)in Biograph object}
\usage{locpath(Bdata)}
\arguments{
  \item{Bdata}{Biograph object}
}
\author{Frans Willekens}
\note{The locpath function is used in Extract and other functions.}
\examples{
data(GLHS)
loc <- locpath(GLHS)
}