\name{ChangeObservationWindow.e}
\alias{ChangeObservationWindow.e}
\title{Changes observation window to period between two transitions}
\description{Observation starts at time of entry into given state (entrystate) and ends at time of entry into another given state (exitstate). If exitstate = NULL, then observation ends at last date for which data are available. Entry into entrystate and entry into exitstate must lie in original observation window. }
\usage{
ChangeObservationWindow.e(Bdata, entrystate, exitstate)
}
\arguments{
  \item{Bdata}{Biograph object}
  \item{entrystate}{Observation starts at time of entry into entry state}
  \item{exitstate}{Entry into exitstate implies end of observation. If exitstate = NA, observation ends at end of initial observation window}
}
\value{Biograph object with information on new observation window and transitions during the new observation window.}
\author{Frans Willekens}
\examples{
 #  The following example defines observation window 
 # as period between labour force entry and survey date. 
 data(GLHS)
 entrystate <- "J"
 exitstate <- NA
 GLHS2 <- ChangeObservationWindow.e (GLHS,entrystate,exitstate)
}
