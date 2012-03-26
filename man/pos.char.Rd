\name{pos.char}
\alias{pos.char}
\title{Position of a given character in a string variable (first match)}
\description{Determines the position(s) of a given character in a string variable. If the character is absent, NA is returned.}
\usage{
pos.char(string, char)
}
\arguments{
  \item{string}{Character string}
  \item{char}{The character to position of which needs to be determined}
}
\details{The function converts the string to a vector of characters. An alternative is to use which(strsplit(string, '')[[1]]=='a'), where a is the character. That code determines all positions of the character in the character string. A similar code is uses in the function TransitionAB. }
\value{The position is returned}
\author{Frans Willekens}
\seealso{printf}
\examples{
string  <- "tests"
pos.char(string,"s")
}
