\name{Biograph.Lexis}
\alias{Biograph.Lexis}
\title{Converts Biograph object to Lexis object of class "Lexis"}
\description{Converts Biograph object to Lexis object.}
\usage{
Biograph.Lexis(Bdata,Dlong)
}
\arguments{
  \item{Bdata}{Biograph object}
  \item{Dlong}{Data in episode (long) format}
}
\details{The argument Dlong is Dlong$Depisode. If Dlong is missing, it is computed. }
\value{Lexis object. Time scale is age and time unit is year.}
\author{Frans Willekens}
\examples{
  data (GLHS)
  Dlongg <- Biograph.long (GLHS)
  D <- Biograph.Lexis (GLHS,Dlongg$Depisode)
  
  D<- Biograph.Lexis (GLHS)
}
\keyword{ Lexis }

