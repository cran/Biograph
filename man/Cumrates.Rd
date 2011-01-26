\name{Cumrates}
\alias{Cumrates}
\title{Estimates and plots cumulative transition rates}
\description{Estimates and plots (optional) cumulative transition rates by origin, destination, and age. Two estimation methods are distinguished: the statistical method and the demographic method.}
\usage{
Cumrates(irate, data, plot.cumrates)
}
\arguments{
  \item{irate}{Indicator variable defining option to be used: 1 = Nelson-Aalen method; 2 = Occurrence-Exposure rate; 3 = Both}
  \item{data}{Biograph object, e.g. GLHS or NLOG98}
  \item{plot.cumrates}{Vector of transitions to be plotted. Transitions are identified by transition number. }
}
\details{
	The Nelson- Aalen estimator is obtained using the mvna package. First the function Biograph.mvna is called to converts the Biograph object to data in the mvna format. Then the mvna and the predict functions of the mvna package are called. The cumulative hazard (with confidence intervals) at each birthday is produced by the predict function.
	 
	For estimating the cumulative occurrence-exposure rates, the function Extract, Occup, Trans and RateTable are called. The object M is produced, which contains the age-specific transition rates by origin and destination in a standard format used in multistate demographic analysis.
	
	The function plots cumulative transition rates (cumulative hazard rates). If irate = 1 or irate = 3 it plots the Nelson-Aalen estimator at each birthday and adds lower and upper confidence intervals. If irate = 2 or irate = 3, it plots the cumulative occurrence-exposure rates. If irate = 3, the function plots both the Nelson-Aalen estimator (with confidence intervals) and the cumulative occurrence-exposure rate.}
\value{
	\item{NeAa}{Nelson-Aalen estimator}
	\item{oe}{Occurrence-exposure rates}
	\item{M}{The N-matrix of age-specific transition rates by origin and destination}
	}
\author{Frans Willekens}
\note{The function takes time to execute. It calls Biograph.mvna and Extract.}
\examples{
	# Compute and plot the cumulative hazard of the NJ and JN transitions.Use the Nelson-Aalen estimator and the occurrence-exposure rate. 
    data(GLHS)
    z <- Cumrates (irate = 3,data= GLHS,plot.cumrates=c(1,2))
  }
