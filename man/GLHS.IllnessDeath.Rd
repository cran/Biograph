\name{GLHS.IllnessDeath}
\alias{GLHS.IllnessDeath}
\title{Illness-death model for job transitions}
\description{The function creates from the GLHS data file a Biograph object with the following states (episodes): first job, second or higher-order job and out-of-a-job.}
\usage{GLHS.IllnessDeath(GLHS)}
\arguments{
  \item{GLHS}{The GLHS data file in Biograph format }
}
\details{The illness-death model is applied to labour-market transitions. In an illness-death model, a person may reach an end-state directly or indirectly by first experiencing an intermediate transition. The illness-death model is generally used in epidemiology and public health. Biograph includes the model to illustrate the mstate package,in particular the msprep function. }
\value{The function returns a Biograph object with 3 states: First job, Second or higher-order job and No job.}
\author{Frans Willekens }
\examples{
	data (GLHS)
	ild <- GLHS.IllnessDeath (GLHS)
	}
