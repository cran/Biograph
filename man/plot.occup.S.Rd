\name{plot.occup.S}
\alias{plot.occup.S}
\title{Plots state occupancies}
\description{Plots state occupancies. It plots the state occupancies (counts in sample or population under observation) as estimated by \code{\link{Occup}}. The data are produced by the Occup function. }
\usage{
\method{plot}{occup.S}(x,namstates.desired,colours,title,area,xmin,xmax,...)
}
\arguments{
  \item{x}{State occupancies in sample population, by age. An object of class \code{occup.S} produced by the \code{Occup} function.}
  \item{namstates.desired}{Desired sequence of states in plot. The argument is used to specify an informative
ordering of the state occupancies or state probabilities to be stacked.}
  \item{colours}{Colours selected to distinguish the states in the state space.}
  \item{title}{title of plot}
  \item{area}{logical variable.If area is TRUE, area plot is displayed (using geom_area of ggplot2). If area is FALSE, a bar plot is displayed (using geom_bar of ggplot2)}
  \item{xmin}{Minimum age in plot}
  \item{xmax}{Maximum age in plot}
   \item{\dots}{Further arguments to plot}
}
\details{
	The function uses the ggplot2 package}
\value{
	\item{occup.S}{State occupancies}
	\item{plot}{The figure of state occupancies}
	}
\author{Frans Willekens}
\examples{
    data(GLHS)
    Bdata.a <- date_b (GLHS,format.out="age",covs=c("marriage","LMentry"))
    occup <- Occup(Bdata.a)
    dd <- occup$state_occup
    t <- "States occupancies. GLHS"
    cc <- c("red","green","lightgrey")
    xx <- c("N","J","Censored")
    z<- plot (x=dd,namstates.desired=xx,colours=cc,title=t,area=TRUE,xmin=10,xmax=55) 
     }
