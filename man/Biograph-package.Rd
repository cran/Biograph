\name{Biograph-package}
\alias{Biograph-package}
\alias{Biograph}
\docType{package}
\title{Explore life histories (biographies, lifepaths)}
\description{Biograph is designed to facilitate the descriptive and statistical analysis of life histories. It follows a multistate perspective on the life course and conceptualizes the life course as a sequence of states and transitions between states (events). Transitions are governed by transition rates that depend on event counts and exposure times. Transition rates also depend on covariates.  
	
Biograph produces useful life-course indicators for individuals and groups: (1) types of episodes (open, closed) and transitions (transient, absorbing), (2) states occupied at various ages, (3) sojourn times in different states and (4) state sequences (trajectories). Biograph estimates transition rates by age, origin and destination. Biograph also structures the data for multistate statistical and demographic modeling of life histories. Biograph accepts data in wide format. The user needs to create a Biograph object from original data. The object has a fixed data structure and is input to most functions. The Doc subdirectory of the package (inst/doc of the source package) contains a description of how to prepare Biograph objects. It includes illustrations and R code. It demonstrates the preparation of Biograph objects using hypothetical data and data from the German Life History Survey (GLHS) (used by Blossfeld and Rohwer in their book Techniques of Event History Modeling, 2002), the Netherlands Family and Fertility Survey (NLOG98), the Survey on Health, Ageing and Retirement in Europe (SHARE), the National Family Life Survey of India (NFHS) and data from the European Registry for Blood and Marrow Transplantation (EBMT) (included in the mstate package developed by Putter and colleagues). 


Two time scales are considered: individual time (age) and calendar time.  }
\details{
\tabular{ll}{
Package: \tab Biograph\cr
Type: \tab Package\cr
Version: \tab 2.0.3\cr
Date: \tab 2013-05-01\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}

Major functions of Biograph (for a complete list, see the INDEX file):

Parameters         :  extract information from data set

TransitionAB       : extract from the raw data information on a selected transition (age profile)

Occup              : determines state occupancies at each age and state-specific sojourn times in age interval

Sequences          : shows the different state sequences (trajectories) in the sample data 

Rates.ac              : computes transition rates by origin state and destination state, and by age (and covariates) by dividing occurrences and exposures. 

Biograph.long      : converts Biograph object to long format required by the survival and eha packages

Biograph.Lexis     : converts Biograph object to Lexis object
	
Biograph.mstate    : converts Biograpgh object to long format required by the mstate package

Biograph.msm       : converts Biograph object to long format required by the msm package

Biograph.mvna      : conferts Biograph object to long format required by the mvna package

Cumrates           : produces (and plots) two types of estimates of cumulative transition rates: Nelson-Aalen estimators (using the mvna package) and occurrence-exposure rates. The latter are generally used in demography. 

Lexispoints        : produces a scatter plot of the age and time at selected transitions for a subsample of subjects with selected characteristics 

Lexislines.episodes : draws lifelines for selected subjects 
 }
\author{Frans Willekens <willekens@demogr.mpg.de>}
\references{
Willekens, F. (2013) Biograph. Multistate analysis of life histories with R. Springer (forthcoming)}
\keyword{ package }
\keyword{ datasets }
\keyword{ survival }
