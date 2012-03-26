\name{string.blank.omit}
\alias{string.blank.omit}
\title{Removes blanks in character string, including the leading and trailing white space.}
\description{Removes blanks in character string and returns string without blanks.}
\usage{
string.blank.omit(string)
}
\arguments{
  \item{string}{Character string}
}
\value{The function returns the string with blanks removed.}
\author{Frans Willekens}
\examples{
z <- string.blank.omit ("This is a test   ")
y <- string.blank.omit (" A B   C  ") 
}
