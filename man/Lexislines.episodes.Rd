\name{Lexislines.episodes}
\alias{Lexislines.episodes}
\alias{Lexislines}
\title{Draws Lexis diagram with lifelines for selected subjects}
\description{The function draws a Lexis diagram, using the Lexis object defined in the Epi package, and draws lifelines for selected subjects.}
\usage{
Lexislines.episodes(Bdata,Dlong, subjectsID, title)
}
\arguments{
  \item{Bdata}{Biograph object}
  \item{Dlong}{Input data in long format}
  \item{subjectsID}{Lifelines are drawn for subjects, identified by ID, included in the vector subjectsID.}
  \item{title}{Title of the Lexis diagram.}
}
\details{The function produces a Lexis object and plots the object for selected subjects. Transitions are marked on the lifelines. It requires data in long format. If the long format is not available, the function calls the function Biograph.long to produce the required format. }
\value{
	\item{Lcoh11}{Lexis object that includes all subjects}
	\item{Lcohh12}{Lexis object that includes selected subjects}
	\item{sub}{vector of IDs of subjects selected for the lifelines in the Lexis diagram}
	\item{k}{Number of subjects selected}
	}
\references{
  Plummer, M. and B. Carstensen (2011) \emph{Lexis: an R class for epidemiolgoical studies with long-term follow-up.} Journal of Statistical Software, 38(5):1-12 
	
  Carstensen, B. (2009) \emph{The Epi package.} Available at http://cran.r-project.org/web/packages/Epi/Epi.pdf}
\author{Frans Willekens}
\note{requires Epi}
\seealso{Lexis.lines}
\examples{
#  Example 1: Employment careers
  data(GLHS) 
  z<- Parameters (GLHS)
  D <- Biograph.long (GLHS)
  tit5 <- "Employment careers for a selection of subjects. GLHS"
  subjects <- c(1,78,120,208)
  z <- Lexislines.episodes (Bdata=GLHS,Dlong=D$Depisode,subjectsID = subjects,title = tit5)

#  Example 2: Long data format need to be obtained
  z <- Lexislines.episodes (Bdata=GLHS,subjectsID = subjects,title = tit5)  

#  Example 3: Living arrangements 
  data(NLOG98)
  z<- Parameters (NLOG98)
  D <- Biograph.long (NLOG98)
  tit5 <- "Living arrangements for a selection of subjects, NLOH98"
  subjectsID <- c(8,96,980,1056,1496,2883)
  z <- Lexislines.episodes (NLOG98,D$Depisode,subjectsID = subjectsID,title = tit5)
}
