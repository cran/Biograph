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
  t <- "Date and age at labour market entry by sex and cohort"
  z <- Lexis.points (GLHS,"NJ",title=t,cov="sex",group="cohort",legend.pos="right",pdf=FALSE)

#  Example 2: ages at first birth among married women, by religious denomination
  data(NLOG98)
  y <- Parameters (NLOG98)
  t <- "Date and age at first birth (married women) by religion and cohort"
  z <- Lexis.points (NLOG98,"MK",title=t,cov="kerk",group="cohort",legend.pos="right",pdf=FALSE)
#  Example 3: ages at leaving home, by birth cohort (any destination)
  t <- "Date and age at leaving parental home by religion and education"
  z <- c(0.7,0.2)
  z <- Lexis.points (NLOG98,"H*",title=t,cov="kerk",group="educ",legend.pos=z,pdf=FALSE)  
  }
