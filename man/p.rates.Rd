\name{p.rates}
\alias{p.rates}
\title{Print transition rates}
\description{Tabulates transition rates in output file}
\usage{
p.rates(pc_ac, rates, path, filename)
}
\arguments{
  \item{pc_ac}{Option age-cohort rates or period-cohort rates. Onluy age-cohort rates implemented.}
  \item{rates}{Occurrence-exposure rates}
  \item{path}{path to folder where rates should be saved}
  \item{filename}{Name of file to save rates}
}
\author{Frans Willekens}
\examples{
   data(GLHS)
   ex_survey <- Parameters (GLHS)
   occup <- Occup (GLHS)
   ist <- statesequence.ind (GLHS)
   trans <- Trans (GLHS,seq.ind=ist)
   ratetable <- RateTable (GLHS,occup,trans)
   rates <- Rates (pc_ac=2,Stable=ratetable$Stable)
   path1 <- getwd() # save output file in current directory
   p.rates (pc_ac=2,rates=rates$M,path=path1,filename="rates.out")
}
