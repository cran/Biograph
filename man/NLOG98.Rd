\name{NLOG98}
\alias{NLOG98}
\docType{data}
\title{NLOG98 Netherlands Family and Fertility Survey 1998}
\description{life history data of NLOG98}
\usage{data(NLOG98)}
\format{
  A data frame with 5450 observations on the following 23 variables.
  \describe{
    \item{\code{ID}}{a numeric vector}
    \item{\code{born}}{a numeric vector}
    \item{\code{start}}{a numeric vector}
    \item{\code{end}}{a numeric vector}
    \item{\code{kerk}}{a factor with levels \code{no religion} \code{Roman Catholic} \code{Protestant} \code{other} \code{missing data}}
    \item{\code{educ}}{a numeric vector}
    \item{\code{cohort}}{a factor with levels \code{<1960} \code{>=1960}}
    \item{\code{YearInt}}{a numeric vector}
    \item{\code{dim}}{a numeric vector}
    \item{\code{ns}}{a numeric vector}
    \item{\code{path}}{a character vector}
    \item{\code{ev1}}{a numeric vector}
    \item{\code{ev2}}{a numeric vector}
    \item{\code{ev3}}{a numeric vector}
    \item{\code{ev4}}{a numeric vector}
    \item{\code{ev5}}{a numeric vector}
    \item{\code{ev6}}{a numeric vector}
    \item{\code{ev7}}{a numeric vector}
    \item{\code{ev8}}{a numeric vector}
    \item{\code{ev9}}{a numeric vector}
    \item{\code{ev10}}{a logical vector}
    \item{\code{ev11}}{a logical vector}
    \item{\code{ev12}}{a logical vector}
  }
}
\details{NLOG98 data}
\source{Statistics Netherlands. Life history format: Matsuo and Willekens, 200?}
\references{Matsuo and Willekens}
\examples{
data(NLOG98)
## maybe str(NLOG98) ; plot(NLOG98) ...
}
\keyword{datasets}
