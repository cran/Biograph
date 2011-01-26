\name{Sequences}
\alias{Sequences}
\title{List of sequences}
\description{Produces a frequency table of the sequences recorded in the data}
\usage{
Sequences(survey)
}
\arguments{
  \item{survey}{Input data}
}
\details{The sequences are determined from the path variable in the data}
\value{sequences: the frequency table of sequences}
\author{Frans Willekens}
\examples{
#	Example 1: Sequences in GLHS
	data(GLHS)
	z <- Parameters (GLHS)
	seq <- Sequences (GLHS)
	
#	Example 2: Sequences in NLOG98
	data(NLOG98)
	z <- Parameters (NLOG98)
	seq <- Sequences (NLOG98)
	
#	Example 3: Sequence in NLOG98, by birth cohort
	table(NLOG98$cohort)   # cohort size
	seq.1 <- Sequences(NLOG98[NLOG98$cohort=="<1960",])
	seq.2 <- Sequences(NLOG98[NLOG98$cohort==">=1960",])
	# Note that the sum(seq.2$ncase) equals size of second cohort	
	}
