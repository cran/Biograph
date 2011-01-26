\name{StackGraph}
\alias{StackGraph}
\title{Plot state occupancies by age}
\description{Plots counts or proportion of people in each state, by age.}
\usage{
StackGraph(x, xlabel, ylabel, xlegend, ylegend, title_main, title_sub, namst)
}
\arguments{
  \item{x}{An object with state occupancies, produced by the Occup function or the multistate life table. 
}
  \item{xlabel}{Label of the x-axis.
}
  \item{ylabel}{Label of the y-axis.
}
  \item{xlegend}{The position of the legend on the x-axis.
}
  \item{ylegend}{The position of the legend on the y-axis.
}
  \item{title_main}{The main title.
}
  \item{title_sub}{A subtitle. The subtitle is shown at the bottom of the graph.
}
  \item{namst}{The legend. A vector of labels for each category. 
}
}
\details{The function uses the barplot function of the R graphics package.}
\author{Frans Willekens
}
\examples{
	data (GLHS)
	occup <- Occup (GLHS)
	tit1 <- "State occupancies by age. GLHS"
	tit2<- "Data from cohorts 1929-31, 1939-41 and 1949-51"
	namstates2 <- c(namstates,"censored")
	z <- StackGraph (x=occup$state_occup[,1:(numstates+1)],
         	xlabel="Age",
			ylabel="State Occupancies",
			xlegen=0,
			ylegend=nsample,
			title_main=tit1,
			title_sub=tit2,
			namst=namstates2)

}
