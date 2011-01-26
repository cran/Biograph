\name{p.table}
\alias{p.tableBiograph}
\title{Print a table}
\description{Prints a table of up to three dimensions}
\usage{
p.tableBiograph(table,filename,w,append)
}
\arguments{
  \item{table}{An object in table format. }
  \item{filename}{The name of the output file that contains the table.}
  \item{w}{Column width}
  \item{append}{A logical variable which is set to TRUE if table should be appendend to filename and to FALSE if new table should be created. }
}
\author{Frans Willekens}
\examples{
	# Print the table of transitions and exposure times that is the basis for the occurrence-exposure rates.
  data (GLHS)
  z<- Parameters (GLHS)
  occup <- Occup(GLHS)
  ist <- statesequence.ind (GLHS)
  trans <- Trans (GLHS,seq.ind=ist)
  y <- RateTable(GLHS, occup, trans)
  p.tableBiograph (table=y$Stable, filename="Stable.out",w=8,append=FALSE)
	}
