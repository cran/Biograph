\name{Parameters}
\alias{Parameters}
\title{Life history parameters from data}
\description{Extracts state space and other characteristics from Biograph object}
\usage{Parameters(Bdata,newnamstates)}
\arguments{
  \item{Bdata}{Biograph object}
  \item{newnamstates}{Biograph detect determines the state space from the data set. The sequence of states detected may not be the most logical sequence. newnamstates allows to change the sequence. Default: sequence of labels detected by Biograph.}
}
\details{Parameters extracts useful information and stores it in values returned by the function. 
	
1.	nsample: sample size 

2.	numstates: number of states in the state space  (produced by StateSpace)

3.	namstates: labels for the states  (determined from the character variable <path>) (produced by StateSpace)

4.	absorbstates: vector of absorbing states (determined by StateSpace)

6. iagelow: lowest age in the (sample) population (determined from date at entry in observation and date of birth)

7.	iagehigh: highest age in the (sample) population (determined from date at exit from observation and date of birth)

8.	namage: labels for the single years of age from the lowest age (iagelow) to the highest age (iagehigh) 

9.  nage: number of age groups: iagehigh - iagelow + 1

10.	maxtrans: maximum number of transitions 

11. ntrans: number of transitions

12. trans_possible: transition matrix: feasible transitions 

12. tmat: matrix with transition numbers

13. transitions: identifications of transitions: number, origin, destination

14. nntrans: transition counts by origin and destination

15. locpat: column number of <path> variable in Biograph object

16. ncovariates: number of covariates

17. covariates: vector of covariate names

18. format.date: format of date variables (chronological objects) in Biograph object

19. format.born: format of date of birth

The parameters and the format of the dates (format.date) are attached to the Biograph object (Bdata) as attributes. 
	}
\value{
  \item{sample}{Sample size}
  \item{numstates}{number of states in state space}
  \item{namstates}{names of states}
  \item{absorbstates}{names of absorbing states }
  \item{iagelow}{lowest age}
  \item{iagehigh}{highest age}
  \item{namage}{names of age groups}
  \item{nage}{number of age groups}
  \item{maxtrans}{maximum number of transitions by individual}
  \item{ntrans}{number of transitions}
  \item{trans_possible}{Origin-destination matrix of logical elements indicating whether transition is allowed or not. In multistate survival analysis, the matrix is known as 'transition matrix'}
  \item{tmat}{Origin-destination matrix showing the line numbers of the transitions. A number is allocated to each possible transition. The matrix tmat is attached as an attribute to the data file.}
  \item{transitions}{Data frame which gives for each transition the following information: origin and destination in character value and numeric value}
  \item{nntrans}{Number of transitions by origin and destination}
  \item{ncovariates}{number of covariates}
  \item{covariates}{covariate labels}
  \item{format.date}{format of date variables in Biograph object}
  \item{format.born}{format of date of birth in Biograph object}
 }
\author{Frans Willekens}
\seealso{
	   \code{\link{StateSpace}}
      }
\examples{
# Example 1
  data(GLHS)
  z <- Parameters(GLHS)
# Example 2
  data (NLOG98)
  z <- Parameters(NLOG98,newnamstates=c("H","A","C","M","K"))
  }

