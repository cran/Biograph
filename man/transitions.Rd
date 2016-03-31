\name{transitions}
\alias{transitions}

\title{Generate flow table}
\description{
Generates table of transitions by origin and destination. }
\usage{
transitions(Bdata,newnamstates)
}
\arguments{
  \item{Bdata}{Biograph object}
  \item{newnamstates}{New names of the states}
}
\value{
  \item{nsample}{Sample size}
  \item{namstates}{Names of the states}
  \item{ntrans}{Number of possible interstate transitions}
  \item{nntrans}{Transition count by origin and destination}
  \item{trans_possible}{Logical variable indicating which transitions are feasible}
  \item{transitions}{List of feasible transitions: line number of transition, origin state and destination state}
  \item{tmat}{Line numbers of the feasible transitions}
 }
\author{Frans Willekens}
\examples{
data (GLHS)
z <- transitions (GLHS)
}
