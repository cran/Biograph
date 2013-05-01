\name{MSLT}
\alias{MSLT}
\alias{MSLT.e}
\alias{MSLT.S}
\title{Multistate life table: survival function and exposure function }
\description{
The function MSLT.S estimates the multistate survival function from cumulative transition rates. 
The function MSLT.e uses the multistate life table method to estimate expected state occupation times by state. }
\usage{
MSLT.S(rates)
MSLT.e(SS, radix)
}
\arguments{
  \item{rates}{transition rates. Either the Nelson-Aalen estimators or the occurrence-exposure rates.}
  \item{SS}{The multistate survival function. An object of class 'MSLT.S'}
  \item{radix}{The radix, which is the distribution of the population by state at the initial (reference) age}
 }
\details{The multistate life table is produced by two functions: (1) MSLT.S produces the multistate survival function and (2) MSLT.e generates expected sojourn times: total and by state. MSLT.S uses the MatrixExp function of the msm package. MSLT.e applies the multistate life table method that incorporates equations that infer expected sojourn times from transition rates and the associated transition probabilities. }

\value{
The function MSLT.S returns:
  \item{S}{The multistate survival function}
  \item{P}{Age-specific transition probabilities}

The function MSLT.e returns an object with the following components:
  \item{L}{For each state, the expected sojourn time by age. It is the number of time units (years, months) a person at the beginning of his or her career (e.g. at birth or at a reference age) may expect to spend in the given state during each age interval. }
  \item{e0}{The total expected sojourn time in each state. If life histories are considered from birth, e0 gives the life expectancy by state. If life histories are truncated at a given age, the life expectancy is the total expected sojourn time in a state between birth and that maximum age. }
  \item{e.p}{Population-based life expectancies by age and state. Population-based measures are independent on the state occupied at the reference age.}
  \item{e.s}{Status-based life expectancies by age and state. Status-based measures depend on the state occupied at the reference age.}
}
\references{
Rogers, A. (1975) Introduction to multiregional mathematical demography. Wiley, New York

Willekens, F. (1987) The marital status life-table. In: J. Bongaarts, T. Burch and K.W. Wachter eds. Family demography: models and applications. Oxford: Clarendon Press. pages 125-149.

Palloni, A. (2001) Increment-decrement life tables. In: S.H. Preston, P. Heuveline and M. Guillot Demography. Measuring and modelling population processes. Blackwell, Oxford, Chapter12, pp. 256-272.

}
\author{Frans Willekens}
\seealso{\code{\link{plot.MSLT.S}}}
\examples{
#	a. The multistate life table based on Nelson-Aalen estimators 
#	The example obtains Nelson-Aalen estimators of the cumulative transition rates using the mavna package. It derives the multistate life survival function from the transition rates and the expected sojourn times from the survival function. The radix is the number of persons by state at the reference age (e.g. at the start of the life history).
    data(GLHS)
    z <- Parameters (GLHS)
    cr <- Cumrates (irate=3,Bdata=GLHS)
    S.e <- MSLT.S(cr$astr[,,,1])  # expected
    radix <- c(10000,0)
    mslt.e <- MSLT.e (S.e,radix)
    S.u <- MSLT.S(cr$astr[,,,2])  # upper
    mslt.u <- MSLT.e (S.u,radix)
    S.l <- MSLT.S(cr$astr[,,,3])  # lower
    mslt.l <- MSLT.e (S.l,radix)
    	
# b. The multistate life table based on occurrence-exposure rates
    cr <- Cumrates (irate=3,Bdata=GLHS)
    S <- MSLT.S(cr$oe)
    radix <- c(10000,0)
    mslt <- MSLT.e (S,radix)
}
