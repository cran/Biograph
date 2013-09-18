\name{Lexispoints}
\alias{Lexispoints}
\title{Plot event dates and ages in Lexis diagram}
\description{Plots observations in age-time diagram for each category of categorical covariate (using the Lexis function of the Epi package)}
\usage{
Lexispoints(Bdata, transition, title, cov,legend)
}
\arguments{
  \item{Bdata}{Biograph object}
  \item{transition}{The transition selected, e.g. "NJ" in GLHS data}
  \item{title}{Title of Lexis diagram}
  \item{cov}{Covariate. Observations are plotted for each category of this covariate. If observations should be plotted for the total sample population, cov = NA}
  \item{legend}{legend}
}
\value{
  \item{Lcoh}{Lexis object created to plot the observations}
}
\references{Plummer, M. and B. Carstensen (2011) Lexis: an R class for epidemiolgoical studies with long-term follow-up. Journal of Statistical Software, 38(5):1-12 
	
Carstensen, B. (2013) The Epi package. 
}
\author{Frans Willekens. The Epi package used to plot the Lexis diagram was developed by Carstensen.}
\note{requires Epi}
\seealso{Lexislines.episodes}
\examples{
#  Example 1: ages at labour market entry, by sex
  library (Epi)
  data(GLHS)
  #y <- Parameters(GLHS)
  t <- "Calendar time and age at labour market entry"
  z <- Lexispoints (Bdata=GLHS,transition="NJ",title=t,cov="sex",legend="topleft")

#  Example 2: ages at first birth among married women, by religious denomination
  data(NLOG98)
  y <- Parameters (NLOG98)
  t <- "Calendar time and age at first birth (married women)"
  z <- Lexispoints (NLOG98,"MK",t,"kerk")
#  Example 3: ages at leaving home, by birth cohort (any destination)
  t <- "Calendar time and age at leaving parental home"
  z <- Lexispoints (NLOG98,"H*",t,"cohort","topleft")  
  }
