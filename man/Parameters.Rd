\name{Parameters}
\alias{Parameters}
\title{Life history parameters from data}
\description{Extracts state space and other characteristics}
\usage{Parameters(survey)}
\arguments{
  \item{survey}{survey is the data file}
}
\details{Parameters extracts useful information and stores it in (1) global variables and (2) values returned by the function. The global variables are: 
	
1.	nsample: sample size 

2.	date_in_month_or_year: the programme tries to determine whether times at events are given in months (1) or years (2). The related global variable timeunit is \sQuote{month} or \sQuote{year}.

3.	numstates: number of states in the state space  (produced by StateSpace called by Parameters)

4.	namstates: labels for the states  (determined from the character variable path) (produced by StateSpace called by Parameters)

5.	iagelow: lowest age in the (sample) population (estimated from CMCs)

6.	iagehigh: highest age in the (sample) population (idem)

7.	namage: labels for the single years of age from the lowest age (iagelow) to the highest age (iagehigh)

8.	nage: number of age groups: iagehigh - iagelow + 1 

9.	locpath: number of the column in the survey data that contains the state sequence (path variable)

10. ncmc_tr: number of different events in the observation window. It is determined by the number of dates of transitions in the Biogaph data set. It is the number of columns in the data set following the state sequence variable path: ncol(survey)-locpath. It may also be determined as the maximum number of states occupied by a subject minus one: max(nchar(survey$path)) - 1. The number of different events is also represented by maxtrans.

11. ncovariates: number of covariates
	}
\value{
  \item{ntrans}{Number of transition types}
  \item{nntrans}{Number of transitions by origin and destination}
  \item{trans_possible}{Origin-destination matrix of logical elements indicating whether transition is allowed or not. In multistate survival analysis, the matrix is known as 'transition matrix'}
  \item{transitions}{Data frame which gives for each transition origin and destination in character value and numeric value}
  \item{tmat}{Origin-destination matrix showing the line numbers of the transitions. A number is allocated to each possible transition. The matrix tmat is attached as an attribute to the data file.}
}
\author{Frans Willekens}
\note{Parameters uses the functions StateSpace and stringf}
\seealso{
	   \code{\link{StateSpace}}, \code{\link{stringf}}
      }
\examples{
# Example 1
  data(GLHS)
  z <- Parameters(GLHS)
# Example 2
  data (NLOG98)
  z <- Parameters(NLOG98)
  }

