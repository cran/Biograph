\name{plot.cumrates}
\alias{plot.cumrates}
\title{Plots cumulative transition rates}
\description{Plots cumulative transition rates by origin, destination, and age estimated by \code{\link{Cumrates}. The required input data are provided by the Cumrates function. }
}
\usage{
\method{plot}{cumrates}(x,ptrans,title,...)
}
\arguments{
  \item{x}{An object of class \code{cumrates}, produced by the \code{Cumrates} function. }
  \item{ptrans}{Selection of transitions to be plotted}
  \item{title}{Title}
   \item{\dots}{Further arguments to plot}
}
\details{
	The function plots cumulative transition rates (cumulative hazard rates). If irate = 1 or irate = 3 it plots the Nelson-Aalen estimator at each birthday and adds lower and upper confidence intervals. If irate = 2 or irate = 3, it plots the cumulative occurrence-exposure rates. If irate = 3, the function plots both the Nelson-Aalen estimator (with confidence intervals) and the cumulative occurrence-exposure rate. The variable irate is an argument of the Cumrates function and included in the object produced by Cumrates.}
\author{Frans Willekens}
\examples{
    data(GLHS)
    Bdata.a <- date_b (GLHS,format.out="age",covs=c("marriage","LMentry"))
    cumrates <- Cumrates (irate = 3,Bdata=Bdata.a)
    z<- plot (x=cumrates,ptrans=c("NJ","JN")) 
  }
