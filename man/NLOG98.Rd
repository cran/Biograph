\name{NLOG98}
\alias{NLOG98}
\docType{data}
\title{NLOG98 Netherlands Family and Fertility Survey 1998}
\description{life history data of NLOG98}
\usage{data(NLOG98)}
\format{
  A data frame with 5450 observations on the following 23 variables.
  \describe{
    \item{\code{ID}}{Personal identification number}
    \item{\code{born}}{Date of birth in Century Month Code (CMC)}
    \item{\code{start}}{Date at entry in observation in CMC}
    \item{\code{end}}{Date at exit from observation in CMC}
    \item{\code{kerk}}{Religious denomination: a factor with 5 levels:
                \code{(1) no religion}
                \code{(2) Roman Catholic} 
                \code{(3) Protestant}
               \code{(4) other}
               \code{(5) missing data}}
    \item{\code{educ}}{Level of education: a factor with 6 levels:  
              \code{(2) Primary education  }
              \code{(3) Secondary lower  }
              \code{(4) Secondary higher  }
              \code{(5) First step tertiary education  }
              \code{(6) Second step tertiary education  }
              \code{(7) Third step tertiary education  }}
    \item{\code{cohort}}{Birth cohort: a factor with 2 levels:  \code{(1) <1960  } \code{(2) >=1960}}
    \item{\code{YearInt}}{Year of interview}
    \item{\code{dim}}{A constant (value 1)}
    \item{\code{ns}}{Number of states occupied during observation period}
    \item{\code{path}}{State sequence during observation period}
    \item{\code{ev1}}{Date of first transition in CMC}
    \item{\code{ev2}}{Date of second transition in CMC}
    \item{\code{ev3}}{Date of third transition in CMC}
    \item{\code{ev4}}{Date of fourth transition in CMC}
    \item{\code{ev5}}{Date of fifth transition in CMC}
    \item{\code{ev6}}{Date of sixth transition in CMC}
    \item{\code{ev7}}{Date of seventh transition in CMC}
    \item{\code{ev8}}{Date of eighth transition in CMC}
    \item{\code{ev9}}{Date of nineth transition in CMC}
    \item{\code{ev10}}{Date of tenth transition in CMC}
    \item{\code{ev11}}{Date of eleventh transition in CMC}  
    \item{\code{ev12}}{Date of twelfth transition in CMC}  }
 }
\details{NLOG98 data}
\source{Statistics Netherlands. Life history format: Matsuo and Willekens, 2003}
\references{Matsuo, H., Willekens F. (2003) 
                 Event histories in the Netherlands Fertility and Family Survey 1998: A technical report. 
                PRC Research Report 2003-1, 
              Groningen: Population Research Centre, University of Groningen.}
\examples{
data(NLOG98)
}
\keyword{datasets}
