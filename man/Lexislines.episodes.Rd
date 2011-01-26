\name{Lexislines.episodes}
\alias{Lexislines.episodes}
\alias{Lexislines}
\title{Draw Lexis diagram with lifelines for selected subjects}
\description{The function draws a Lexis diagram, using the Lexis object defined in the Epi package, and draws lifelines for selected subjects.}
\usage{
Lexislines.episodes(Dlong, subjectsID, title1)
}
\arguments{
  \item{Dlong}{Input data in long format}
  \item{subjectsID}{Lifelines are drawn for subjects, identified by ID, included in the vector subjectsID.}
  \item{title1}{Title of the Lexis diagram.}
}
\details{The data should be in the long format. The function produces a Lexis object and plots the object. Transitions are marked on the lifelines.}
\value{
	\item{Lcoh11}{Lexis object}
	\item{Lcohh12}{Lexis object}
	\item{sub}{vector of IDs of subjects selected for the lifelines in the Lexis diagram}
	\item{k}{Number of subjects selected}
	}
\author{Frans Willekens}
\note{requires Epi}
\examples{
#  Example 1: Employment careers
  data(GLHS) 
  z<- Parameters (GLHS)
  D <- Biograph.long (GLHS)
  tit5 <- "Employment careers for a selection of subjects. GLHS"
  subjects <- c(1,78,120,208)
  z <- Lexislines.episodes (D$Depisode,subjectsID = subjects,title1 = tit5)
  
#  Example 2: Living arrangements 
  data(NLOG98)
  z<- Parameters (NLOG98)
  D <- Biograph.long (NLOG98)
  tit5 <- "Living arrangements for a selection of subjects, NLOH98"
  subjectsID <- c(12,15,19,5442)
  rainbow(length(namstates))
  z <- Lexislines.episodes (D$Depisode,subjectsID = subjectsID,title1 = tit5)
}
