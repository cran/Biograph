\name{cmc.as.age}
\alias{cmc.as.age}
\title{Converts date in Century Month Code (CMC) to age.}
\description{Converts date in Century Month Code (CMC) to age.}
\usage{cmc.as.age (x,born,format.born)}
\arguments{
  \item{x}{Date in Century Month Code}
  \item{born}{Date of birth}
  \item{format.born}{Format of date of birth}
  }
\value{
  \item{year}{Dates in calendar year and fraction of year}
  \item{age}{age}}
\author{Frans Willekens}
\examples{
	cmc.as.age (1260,555,"cmc")
	cmc.as.age (1260,"1946-03-10","\%Y-\%m-\%d")

}


