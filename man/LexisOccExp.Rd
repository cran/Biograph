\name{LexisOccExp}
\alias{LexisOccExp}
\title{Displays occurrences, exposures and transition rates in a Lexis diagram}
\description{The Lexis diagram is an age-time framework. The diagram displays transition data in two time dimensions, e.g. calendar date and age. The Lexis class in the EPI package is an object-based framework for managing transition data on multiple time scales. The function displays occurrences, exposures and transition rates for a selected transltion. The user also determines the age and time interval.}
\usage{
LexisOccExp(Bdata, transition, nyear)
}
\arguments{
  \item{Bdata}{Data in Biograph format (Biograph object)}
  \item{transition}{The transition selected. A transition is identified by the state of origin and the state of destination.}
  \item{nyear}{The age and time interval, generally 5 years.}
}
\details{The presentation of data in the Lexis diagram is particularly helpful to explain the concept of occurrence-exposure rate. The functions plots Lexis diagrams for event counts, exposures and transition rates. In addition, it produces an object with as components the event counts, exposures and rates. It also produces a survival object, which can be used for further survival analysis with the survival package, and a Lexis object, which can be used for further analysis with the Epi package. }
\value{
 \item{surv }{The survival object. The survival object is used as a response variable in survival models (see the survival package in CRAN)}
 \item{Lcoh }{The Lexis object. The Lexis object is a key object in the Epi package.}
 \item{nevents}{Event counts: number of transitions by age, origin and destination}
 \item{ndur}{Exposure times: duration exposed to the risk of experiencing the selected transition.}
 \item{rates}{The transition rates (occurrence-exposure rates). They are obtained by dividing the event counts by the exposure times. }
}
\references{
Carstensen, B. (2007) Age-Period-Cohort models for the Lexis diagram. Statistics in Medicine, 26(15):3018-3045.

Carstensen, B. (2013) The Epi package. 
 }
\author{Frans Willekens }
\examples{
# The following example selects the "JN" transitions from the GLHS data 
# and produces Lexis diagrams with age and time intervals of 5 years data (GLHS)
  data (GLHS)
  w <- LexisOccExp (Bdata=GLHS,transition= "JN",nyear=5)
}
