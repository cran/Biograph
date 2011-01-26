\name{MSLT}
\alias{MSLT}
\alias{MSLT.e}
\alias{MSLT.S}
\alias{MSLT.S.plot}
\title{Multistate life table: survival function and exposure function }
\description{The function MSLT.S estimates the survival function from the cumulative transition rates and uses the multistate life table method to estimate the sojourn times by state in the state space. }
\usage{
MSLT.S(cumrates)
MSLT.e(S, radix)
MSLT.S.plot(S, e0, title)
}
\arguments{
  \item{cumrates}{cumulative transition rates. Either the Nelson-Aalen estimator or the occurrence-exposure rates.}
  \item{S}{The multistate survival function}
  \item{radix}{The radix, which is the distribution of the population by state at the initial (reference) age}
  \item{e0}{Life expectancy at reference age (e.g. at birth)}
  \item{title}{Title for multistate survival plot}
 }
\details{The multistate survival function uses the MatrixExp function of the msm package. The multistate life table method incorporates equations that infer expected sojourn times from transition rates and the associated transition probabilities. }
\value{The function MSLT.S returns the multistate survival function and the function MSLT.e returns an object with the following components:
  \item{L }{For each state, the expected sojourn time by age. It is the number of time units (years, months) a person at the beginning of his or her career (e.g. at birth or at a reference age) may expect to spend in the given state during each age interval. }
  \item{e0 }{The total expected sojourn time in each state. If life histories are considered from birth, e0 gives the life expectancy by state. If life histories are truncated at a given age, the life expectancy is the total expected sojourn time in a state between birth and that maximum age. }
  \item{e.p}{Population-based life expectancies by age and state. Population-based measures are independent on the state occupied at the reference age.}
  \item{e.s}{Status-based life expectancies by age and state. Status-based measures depend on the state occupied at the reference age.}
}
\references{
Rogers, A. (1975) Introduction to multiregional mathematical demography. Wiley, New York

Willekens, F. (1987) The marital status life-table. In: J. Bongaarts, T. Burch and K.W. Wachter eds. Family demography: models and applications. Oxford: Clarendon Press. pages 125-149.
}
\author{Frans Willekens}
\examples{
#	a. The multistate life table based on Nelson-Aalen estimators 
#	The example obtains Nelson-Aalen estimators of the cumulative transition rates using the mavna package. It derives the multistate life survival function from the transition rates and the expected sojourn times from the survival function. The radix is the number of persons by state at the reference age (e.g. at the start of the life history).
    data(GLHS)
    z <- Parameters (GLHS)
    cr <- Cumrates (irate=3,data=GLHS,plot.cumrates=NULL)
    S <- MSLT.S(cr$NeAa[,,,1])
    radix <- c(10000,0)
    mslt <- MSLT.e (S,radix)
	
# b. The multistate life table based on occurrence-exposure rates
    cr <- Cumrates (irate=3,data=GLHS,plot.cumrates=NULL)
    S <- MSLT.S(cr$oeCum)
    radix <- c(10000,0)
    mslt <- MSLT.e (S,radix)
    
# c. Plot the multistate survival function
    MSLT.S.plot(S,mslt$e0,title="Multistate survival function")
}
