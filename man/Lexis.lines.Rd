\name{Lexis.lines}
\alias{Lexis.lines}
\title{Draws Lexis diagram with lifelines for selected subjects}
\description{The function draws a Lexis diagram and draws lifelines for selected subjects  (using ggplot2 library)}
\usage{
Lexis.lines(Bdata,Dlong, subjectsID, title)
}
\arguments{
  \item{Bdata}{Biograph object}
  \item{Dlong}{Input data in long format}
  \item{subjectsID}{Lifelines are drawn for subjects, identified by ID, included in the vector subjectsID.}
  \item{title}{Title of the Lexis diagram.}
}
\details{The function produces a Lexis object and plots the object for selected subjects. Transitions are marked on the lifelines. It requires data in long format. If the long format is not available, the function calls the function Biograph.long to produce the required object. }
\value{
	\item{subjectsID}{Subjects selected}
	\item{p}{The plot of the lifelines}
	}
\author{Frans Willekens}
\note{requires ggplot2}
\seealso{Lexislines.episodes}
\examples{
#  Example 1: Employment careers
  data(GLHS) 
  z<- Parameters (GLHS)
  GLHS.yr <- date_b(Bdata=GLHS,format.in=attr(GLHS,"format.date"),selectday=1,format.out="year")
  D <- Biograph.long (GLHS.yr)
  tit5 <- "Employment careers for a selection of subjects. GLHS"
  subjects <- c(1,78,120,208)
  z <- Lexis.lines (Bdata=GLHS.yr,Dlong=D$Depisode,subjectsID = subjects,title = tit5)

#  Example 2: Long data format need to be obtained
  z <- Lexislines.episodes (Bdata=GLHS.yr,subjectsID = subjects,title = tit5)  

#  Example 3: Living arrangements 
  data(NLOG98)
  z<- Parameters (NLOG98)
  NLOG98.yr <- date_b(Bdata=NLOG98,format.in=attr(NLOG98,"format.date"),selectday=1,format.out="year")
  D <- Biograph.long (NLOG98.yr)
  tit5 <- "Living arrangements for a selection of subjects, NLOH98"
  subjectsID <- c(12,15,19,5442)
  z <- Lexis.lines (NLOG98.yr,D$Depisode,subjectsID = subjectsID,title = tit5)
}
