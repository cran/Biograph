\name{rrdat}
\alias{rrdat}
\docType{data}
\title{The Blossfeld-Rohwer subsample of the German Life History Survey (GLHS)}
\description{The data provides information on 600 job episodes. }
\usage{data(rrdat)}
\details{
ID    Identification number of subject

NOJ   Serial number of the job episode

TS    Starting time of the job episode

TF    Ending time of the job episode

SEX   Sex (1 male; 2 female)

TI    Date of interview (CMC)

TB    Date of birth (CMC)

T1    Date of entry into the labour market (CMC)

TM    Date of marriage (CMC) [0 if not married]

PRES  Prestige score of current job, i.e. of job episode in current record of data file

PRESN Prestige score of the next job (if missing: -1)

EDU   Highest educational attainment before entry into labour market
}
\source{

	 http://oldsite.soziologie-blossfeld.de/eha/tda/cf_files/Data/RRDAT.1

}
\seealso{
  \code{\link{GLHS}}
}
\references{	
	Blossfeld, H.P. and G. Rohwer (2002) Techniques of event history modeling. New approaches to causal analysis. Lawrence Erlbaum, Mahwah, New Jersey (2nd Edition).}
\examples{
data(rrdat)
}
\keyword{datasets}
