\name{SamplePath}
\alias{SamplePath}
\title{Obtains samplepath for selected subjects}
\description{Obtains empirical lifepaths for selected subjects}
\usage{
SamplePath(Bdata, subjectsID)
}
\arguments{
  \item{Bdata}{Biograph object}
  \item{subjectsID}{Vector with IDs of selected subjects}
}
\details{Displays lifepaths for selected individuals}
\author{Frans Willekens}
\examples{
	data (GLHS)
	z <- Parameters (GLHS)
   subjectsID <- c(1,6,7,19,136,208)
   samplepaths <- SamplePath (Bdata=GLHS,subjectsID=subjectsID)  
}
