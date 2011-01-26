\name{p.SamplePath}
\alias{p.SamplePath}
\title{Print samplepath for selected subjects}
\description{Prints empirical lifepaths for selected subjects}
\usage{
p.SamplePath(data, timeunit, subjectsID, seq.ind, ages, path, file)
}
\arguments{
  \item{data}{Biograph object}
  \item{timeunit}{Time unit used in date expression (year or month)}
  \item{subjectsID}{Vector with IDs of selected subjects}
  \item{seq.ind}{Individual sequences or lifepaths}
  \item{ages}{Ages at transitions}
  \item{path}{Path to folder where sample paths should be saved.}
  \item{file}{Output file name}
}
\details{Displays lifepaths for selection of individuals}
\author{Frans Willekens}
\examples{
	data (GLHS)
	z <- Parameters (GLHS)
   agetrans <- AgeTrans (GLHS)
   ist <- statesequence.ind (GLHS)
   timeunit <- "month"
   subjectsID <- c(1,6,7,19,136,208)
   path1 <- getwd() # save output in current directory
   samplepaths <- p.SamplePath (data=GLHS, timeunit=timeunit,subjectsID=subjectsID,
         seq.ind=ist,ages=agetrans$ages,path=path1,file="biograph.out")  
}
