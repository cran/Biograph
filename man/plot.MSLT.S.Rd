\name{plot.MSLT.S}
\alias{plot.MSLT.S}
\title{Plots survival function. }
\description{Plot method for object of class 'MSLT.S' using the ggplot2 package. It plots the multistate survival function by as estimated by \code{\link{MSLT.S}}
}
\usage{
\method{plot}{MSLT.S}(x, e0, order,colours,title,area,xmin,xmax,...)
}
\arguments{
  \item{x}{The multistate survival function. An object of class \code{MSLT.S} produced by the \code{MSLT.S} function. It is one of two components of the object produced by the \code{MSLT.S} function.}
  \item{e0}{Life expectancy at reference age (e.g. at birth)}
  \item{order}{Desired sequence of states in plot}
  \item{colours}{Colours}
   \item{title}{Title for multistate survival plot}
  \item{area}{Graph type: area (area=TRUE) or bar (area=FALSE)}
  \item{xmin}{Minimum value on x-axis}
  \item{xmax}{Maximum value on x-axis}

  \item{\dots}{Further arguments to plot}
  }
\value{The function plot.MSLT.S returns the multistate survival function (S) and the plot. It returns an object with the following components:
  \item{S }{The multistate survival function (values used in the plot) }
  \item{plot }{The plot produced by the ggplot2 package. }
      }
\author{Frans Willekens}
\seealso{MSLT.S}
\examples{   	
# The multistate life table based on occurrence-exposure rates
    data (GLHS)
    param <- Parameters (GLHS)
    cr <- Cumrates (irate=3,Bdata=GLHS)
    S <- MSLT.S(cr$oe)
    radix <- c(10000,0)
    mslt <- MSLT.e (S,radix)
    
# Plot the multistate survival function (object of class 'MSLT.S' )
    z<- plot (x=S$S,e0=mslt$e0,title="Multistate survival function",area=TRUE,order=c("N","J"))
}
