\name{date_b}
\alias{date_b}
\title{Converts dates in a Biograph object to dates in another format}
\description{Converts dates in a Biograph object to dates in another format. The function converts an entire Biograph object. It calls the function date_convert, which converts a vector of dates.}
\usage{date_b(Bdata,selectday,format.out,covs)}
\arguments{
  \item{Bdata}{Biograph object}
  \item{selectday}{If input date is in Century Month Code (year and month), the argument selectday gives the day of the month. Default value is one, the transition occurs on the 1st of the month. The argument may be omitted.}
  \item{format.out}{Format of output date}
  \item{covs}{Vector of covariates that are dates (that need to be converted too). The dates should be in the same format as the other dates.}
}
\value{
  \item{}{Biograph object with dates in new format (format.out)}
  }
\author{Frans Willekens}
\examples{
    data(GLHS)
    bb<- date_b (Bdata=GLHS,selectday=1,format.out="day-mon-year",covs=c("marriage","LMentry"))

}


