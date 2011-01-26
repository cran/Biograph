\name{Lexispoints}
\alias{Lexispoints}
\title{Plot observations in Lexis diagram}
\description{Plots observations in age-time diagram for each category of categorical covariate}
\usage{
Lexispoints(data, transition, title1, cov)
}
\arguments{
  \item{data}{Biograph object}
  \item{transition}{The transition selected, e.g. "NJ" in GLHS data}
  \item{title1}{Title of Lexis diagram}
  \item{cov}{Covariate. Observations are plotted for each category of this covariate. If observations should be plotted for the total sample population, cov = NA}
}
\value{
  \item{Lcoh}{Lexis object created to plot the observations}
}
\references{For Lexis diagram, see Carstensen (2009) and the Epi pacakge}
\author{Frans Willekens. The Epi package used to plot the Lexis diagram was developed by Carstensen.}
\note{requires Epi}
\seealso{Lexislines.episodes}
\examples{
#  Example 1: ages at labour market entry, by sex
  library (Epi)
  data(GLHS)
  y <- Parameters(GLHS)
  z <- Lexispoints (GLHS,"NJ","Calendar time and age at labour market entry","sex")

#  Example 2: ages at first birth among married women, by religious denomination
  data(NLOG98)
  y <- Parameters (NLOG98)
  z <- Lexispoints (NLOG98,"MK","Calendar time and age at first birth (married women)","kerk")
#  Example 3: ages at leaving home, by birth cohort (any destination)
  z <- Lexispoints (NLOG98,"H*","Calendar time and age at leaving parental home","cohort")  
  }
