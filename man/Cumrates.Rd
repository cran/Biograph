\name{Cumrates}
\alias{Cumrates}
\title{Estimates cumulative transition rates}
\description{Estimates cumulative transition rates by origin, destination, and age. Two estimation methods are distinguished: the statistical method (Nelson-Aalen estimator) and the demographic method (occurrence-exposure rates).}
\usage{
Cumrates(irate, Bdata)
}
\arguments{
  \item{irate}{Indicator variable defining option to be used: 1 = Nelson-Aalen method; 2 = Occurrence-Exposure rate; 3 = Both}
  \item{Bdata}{Biograph object}  
}
\details{
	The Nelson- Aalen estimator is obtained using the mvna package. First the function Biograph.mvna is called to convert the Biograph object to a data frame of class mvna. Then the mvna and the predict functions of the mvna package are called. The predict function produces cumulative hazards (with confidence intervals) at each birthday.
	 
	For estimating the cumulative occurrence-exposure rates, the functions Occup, Trans and RateTable are called (among other functions). The object M is produced; it is a three-dimensional array containing the transition rates by age, origin and destination in a standard format used in multistate demographic analysis.
}
\value{
	\item{D}{The Biograph object Bdata with diagonal elements removed}
	\item{irate}{Option for estimation of rates: (1) Nelson-Aalen, (2) Occurrence-exposure rates, (3) both}
	\item{NeAa}{Nelson-Aalen estimator}
	\item{predicted}{Cumulative transition rates (Nelson-Aalen estimator) predicted at consecutive ages (using predict function of mvna package)}
	\item{astr}{Age-specific transition rate by age, derived from predicted cumulative transition rates}
	\item{oeCum}{Cumulative occurrence-exposure rates}
	\item{oe}{The M-matrix of age-specific transition (occurrence-exposure) rates by origin and destination}
	}
\author{Frans Willekens}
\note{The function takes time to execute. It calls Parameters, date_convert, Remove.intrastate, statesequence.ind, Occup, Trans, RateTable, Rates.ac, Biograph.mvna and the mvna and predict functions of the mvna package.}
\examples{
    data(GLHS)
    Bdata.a <- date_b (GLHS,format.out="age",covs=c("marriage","LMentry"))
    cumrates <- Cumrates (irate = 3,Bdata=Bdata.a)
  }
