\name{ChangeObservationWindow.t}
\alias{ChangeObservationWindow.t}
\title{Change observation window to period between two points in time}
\description{Observation starts at starttime and ends at endtime.}
\usage{
ChangeObservationWindow.t(survey, starttime, endtime)
}
\arguments{
  \item{survey}{Biograph object}
  \item{starttime}{Observation starts at starttime. Time is measured in unit used in Biograph object.}
  \item{endtime}{Observation ends at endtime. Time is measured in unit used in Biograph object.}
}
\value{Biograph object with information on new observation window and transitions during the new observation window.}
\author{Frans Willekens}
\examples{
 #  The following example defines observation window as period between CMC 500 and CMC 900 (survey = GLHS)
 data (GLHS)
 z <- ChangeObservationWindow.t (GLHS,starttime=500,endtime=800)
}