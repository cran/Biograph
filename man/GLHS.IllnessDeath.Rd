\name{GLHS.IllnessDeath}
\alias{GLHS.IllnessDeath}
\title{Illness-death model for job transitions}
\description{The function creates from the GLHS data file a Biograph object with the following states (episodes): first job, second or higher-order job and out-of-a-job.}
\usage{GLHS.IllnessDeath(GLHS)}
\arguments{
  \item{GLHS}{The GLHS data file in Biograph format }
}
\details{In an illness-death model, a healthy person may get a disease or become disabled, or die. A diseased or disabled person may experience death. Recovery is not possible. The model is used in epidemiology and public health. Biograph includes the model to illustrate the mstate package,in particular the msprep function. }
\value{The function returns the data set GLHS.tg, which is a Biograph object with 3 states: First job, Second or higher-order job and No job.}
\author{Frans Willekens }
\examples{
	data (GLHS)
	ild <- GLHS.IllnessDeath (GLHS)
	}
