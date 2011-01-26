\name{GLHS}
\alias{GLHS}
\docType{data}
\title{GLHS data}
\description{data from Blossfeld-Rohwer 2002}
\usage{data(GLHS)}
\format{
  A data frame with 201 observations on the following 24 variables. Dates are given in Century Month Code (CMC).
  \describe{
    \item{\code{ID}}{Identification number}
    \item{\code{born}}{Month of birth (CMC)}
    \item{\code{start}}{Onset of observation (CMC)}
    \item{\code{end}}{End of observation (CMC)}
    \item{\code{sex}}{Sex. A factor with levels \code{Males} \code{Females}}
    \item{\code{edu}}{Years of education.}
    \item{\code{marriage}}{Date of marriage (CMC)}
    \item{\code{TE}}{Date of entry in labour force (CMC)}
    \item{\code{cohort}}{Birth cohort. A factor with levels \code{1929-31} \code{1939-41} \code{1949-51}}
    \item{\code{idim}}{a numeric vector (not used)}
    \item{\code{ns}}{Number of states occupied during observation period.}
    \item{\code{path}}{State sequence (a character vector)}
    \item{\code{Tr1}}{Date of first transition (CMC)}
    \item{\code{Tr2}}{Date of second transition (CMC)}
    \item{\code{Tr3}}{Date of third transition (CMC)}
    \item{\code{Tr4}}{Date of fourth transition (CMC)}
    \item{\code{Tr5}}{Date of fifth transition (CMC)}
    \item{\code{Tr6}}{Date of sixth transition (CMC)}
    \item{\code{Tr7}}{Date of sevent transition (CMC)}
    \item{\code{Tr8}}{Date of eight transition (CMC)}
    \item{\code{Tr9}}{Date of ninth transition (CMC)}
    \item{\code{Tr10}}{Date of tenth transition (CMC)}
    \item{\code{Tr11}}{Date of eleventh transition (CMC)}
    \item{\code{Tr12}}{Date of twelfth transition (CMC)}
  }
}
\source{
for the original data, see http://oldsite.soziologie-blossfeld.de/eha/tda/cf_files/Data/RRDAT.1}
\references{
Blossfeld, H.P. and G. Rohwer (2002) Techniques of event history modeling. New approaches to causal analysis. Lawrence Erlbaum, Mahwah, New Jersey (2nd Edition).

Blossfeld, H.P., K. Golsh and G. Rohwer (2007) Event history analysis with Stata. Erlbaum, Mahwah, New Jersey.
}
\seealso{
  \code{\link{rrdat}}}
\keyword{datasets}
