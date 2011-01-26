\name{Biograph-package}
\alias{Biograph-package}
\alias{Biograph}
\docType{package}
\title{Explore biograpies / life histories / lifepaths}
\description{Extracts from the data useful information on the life courses of selected individuals or subpopulations with given attributes: 
	- sequences of states and events or transition; 
	- episodes: open (left truncated, right censored) and closed;
	- sojourn times. 
Two time scales are considered: individual time (age) and calendar time. 

In biographic analysis, discrete states of existence are distinguished. Rates of transition between states are the main parameters of life-course models. Biograph helps the user to understand how transition rates are related to event counts (occurrences) and sojourn times (exposures). 

Biograph has several "adapters", functions that convert a Biograph object, which has data structure known as 'wide format' to the 'long format' required by the major packages for (multistate) survival analysis and event history analysis. The long format has one record for each episode (episode data) or transition (event data). }
\details{
\tabular{ll}{
Package: \tab Biograph\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-1-25\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
The user needs to create a Biograph object from original data. The object has a fixed data structure and is input to most functions. }
\author{Frans Willekens <willekens@nidi.nl>}
\references{
Willekens, F. (2011) Biograph. Multistate analysis of life histories with R.}
\keyword{ package }
\keyword{ datasets }
\keyword{ survival }
\examples{
Parameters         #  extract information from data set
TransitionAB       # extract from the raw data information on a selected transition (age profile)
Occup              # determines state occupancies at each age and state-specific sojourn times in age interval
Sequences          # shows the different state sequences (trajectories) in the data 
Rates              # derives transition rates by origin state and destination state, and by age (and covariates) by dividing occurrences and exposures. Two types of rates are distinguished. 
Biograph.long      # converts Biograph object to the long format required by the survival package and other packages
Biograph.mstate    # converts Biograpgh object to the long format required by the mstate package
Biograph.msm       # converts Biograph object to the long format required by the msms package
Biograph.mvna      # conferts Biograph object to the long format required by the mvna package
Cumrates           # produces (and plots) two types of estimates of transition rates: Nelson-Aalen estimators (using the mvna package),generally used in statistics, and occurrence-exposure rates, generally used in demography. 
Lexispoints        # produces a scatter plot of the age and time of a selected transition for a subsample of subjects with selected characteristics (using the Epi package)
Lexislines.episodes  # draws lifelines for selected subjects (using the Epi package)
}
