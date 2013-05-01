\name{Lexis.points}
\alias{Lexis.points}
\title{Plots ages and calendar years at transitions in Lexis diagram }
\description{Plots observations in age-time diagram for each category of categorical covariate (using ggplot2)}
\usage{
Lexis.points(Bdata, transition, title, cov,group,legend.pos,pdf)
}
\arguments{
  \item{Bdata}{Biograph object}
  \item{transition}{The transition selected, e.g. "NJ" in GLHS data}
  \item{title}{Title of Lexis diagram}
  \item{cov}{Covariate. Observations are plotted for each category of this covariate. If observations should be plotted for the total sample population, cov = NA}
  \item{group}{Covariate. Graphs for groups are in separate panels. }
  \item{legend.pos}{position of legend}
  \item{pdf}{If graph should be stored in pdf file: pdf=TRUE, else pdf=FALSE}
  }
\value{graph}
\author{Frans Willekens. }
\note{requires ggplot2}
\seealso{Lexispoints (requires Epi)}
\examples{
#  Example 1: ages at labour market entry, by sex
  data(GLHS)
  #y <- Parameters(GLHS)
  z <- Lexis.points (Bdata=GLHS,transition="NJ",title="Date and age at labour market entry by sex and cohort",cov="sex",group="cohort",legend.pos="right",pdf=FALSE)

#  Example 2: ages at first birth among married women, by religious denomination
  data(NLOG98)
  y <- Parameters (NLOG98)
  z <- Lexis.points (Bdata=NLOG98,transition="MK",title="Date and age at first birth (married women) by religion and cohort",cov="kerk",group="cohort",legend.pos="right",pdf=FALSE)
#  Example 3: ages at leaving home, by birth cohort (any destination)
  z <- Lexis.points (Bdata=NLOG98,transition="H*",title="Date and age at leaving parental home by religion and education",cov="kerk",group="educ",legend.pos=c(0.7,0.2),pdf=FALSE)  
  }
