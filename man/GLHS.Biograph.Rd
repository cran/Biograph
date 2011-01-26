\name{GLHS.Biograph}
\alias{GLHS.txt_to_R}
\alias{GLHS.Biograph}
\title{Biograph object from Blossfeld-Rohwer data rrdat.1}
\description{The function GLHS.txt_to_R creates the R data file rrdat1.RData from the text file rrdat.txt. This text file is the renamed rrdat.1 file that accompanies Blossfeld-Rohwer (2002).

The function GLHS.Biograph converts the rrdat1.RData data file to a Biograph object and saves the object as the R data file GLHS.RData. }
\usage{
GLHS.txt_to_R (path,filename)
GLHS.Biograph ()
}
\arguments{
  \item{path}{Path to the location of rrdat1.txt}
  \item{filename}{The name of the text file, e.g. rrdat1.txt.}
}
\details{The function GLHS.Biograph creates the GLHS data file in Biograph format and adds the "trans" attribute. The function returns the Biograph object. 

The Biograph package includes two data files: rrdat and GLH. The first is the original data and the second is the Biograph object. 
}
\value{The Biograph object (data file in Biograph format)}
\references{For a description of the original data, see Blossfeld and Rohwer (2002)}
\author{Frans Willekens}
\examples{
	# Move to folder with the Blossfeld-Rohwer data set.
	path <-  getwd()  # replace by the path on your computer
	# not run
	# rrdat <- GLHS.txt_to_R (path,filename="rrdat.txt")
	data (rrdat)
	head(rrdat)
	tail(rrdat)
    GLHS <- GLHS.Biograph ()
}
\keyword{ data }
