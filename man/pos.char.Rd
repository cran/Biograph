\name{pos.char}
\alias{pos.char}
\title{Position of a character in a string variable (first match)}
\description{Determines the positions of a given character in a string variable and returns the first position. If the character is absent, NA is returned.}
\usage{
pos.char(string, char)
}
\arguments{
  \item{string}{Character string}
  \item{char}{The character to position of which needs to be determined}
}
\value{The position is returned}
\author{Frans Willekens}
\seealso{printf}
\examples{
pos.char("test","s")
}
