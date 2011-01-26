\name{TransitionAB}
\alias{TransitionAB}
\title{Age profile of selected transition}
\description{Extract from a Biograph object and for a given transition the number of transitions by age}
\usage{
TransitionAB(data, transition)
}
\arguments{
  \item{data}{Biograph object}
  \item{transition}{Selected transition}
}
\value{
  \item{case}{Total count of the selected transition}
  \item{N}{Position of the selected transition in the path variable of the Biograph object}
  \item{date}{For each subject experiencing the transition, date of transition}
  \item{age}{For each subject experiencing the transition, age at transition}
  \item{year}{For each subject experiencing the transition, year of transition (year is real variable, including fraction of year)}
  \item{cohort}{For each subject experiencing the transition, birth cohort}
}
\author{Frans Willekens}
\examples{
  data (GLHS)
  y <- Parameters (GLHS)
  z <- TransitionAB (GLHS,"NJ")
}
