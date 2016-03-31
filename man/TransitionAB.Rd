\name{TransitionAB}
\alias{TransitionAB}
\title{Occurrences of selected transition by age}
\description{Determines for a given transition the number of transitions by age}
\usage{
TransitionAB(Bdata, transition,keep)
}
\arguments{
  \item{Bdata}{Biograph object}
  \item{transition}{Selected transition}
  \item{keep}{Logical variable indicating whether observations (subjects) with the selected transition missing should be kept (TRUE) or removed (FALSE). Default: keep=FALSE}
}
\value{
  \item{case}{The transition}
  \item{n}{Number of subjects in the sample experiencing the selected transition (transition count).}
  \item{id}{Identification number of subjects that experienced the transition}
   \item{pos}{Position of the selected transition in the state sequence (path variable Bdata$path of the Biograph object). If transition = "*N" and N is the first state, then that state is skipped. If a subject experienced several of the selected transition, the first transition is considered. }
  \item{date}{For each subject experiencing the transition, date of transition}
  \item{age}{For each subject experiencing the transition, age at transition}
  \item{year}{For each subject experiencing the transition, year of transition (year is real variable, including fraction of year)}
  \item{cohort}{For each subject experiencing the transition, birth cohort}
}
\author{Frans Willekens}
\examples{
# Example 1: Transition NJ in GLHS data set
  data (GLHS)
  z <- TransitionAB (GLHS,"NJ",keep=TRUE)

# Example 2: Transition HM in NLOG98 data set
  data (NLOG98)
  z <- TransitionAB (NLOG98,"HM")

# Example 3: Transition 'Leaving parental home", irrespective of destination state
  data (NLOG98)
  z <- TransitionAB (NLOG98,"H*")

# Example 4: First marriage, irrespective of origin state
  data (NLOG98)
  z <- TransitionAB (NLOG98,"*M")
}
