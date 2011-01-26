\name{IllnessDeath.GLHS}
\alias{IllnessDeath.GLHS}
\title{Illness-death model for job transitions}
\description{The function creates from the GLHS data file a new file in Biograph format with the following states (episodes): first job, second or higher-order job and out-of-a-job.}
\usage{IllnessDeath.GLHS(GLHS)}
\arguments{
  \item{GLHS}{The GLHS data file in Biograph format }
}
\details{In an illness-death model, a healthy person may stay healhty, get a disease or becomes disables or may die. A diseased or disabled person may stay diseased or disabled, or may experience death. Recovery is not possible. The model is used in epidemiology and public health. Biograph includes the model to illustrate the mstate package, ,in particular the msprep function. }
\value{The function returns the data set GLHS.tg, which is the data in Biograph format for 3 states: First job, Second or higher-order job and No job.}
\author{Frans Willekens }
\examples{
	data (GLHS)
	ild <- IllnessDeath.GLHS (GLHS)
	}
