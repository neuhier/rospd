% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doeFactor.R
\name{getRandomLevel}
\alias{getRandomLevel}
\title{Function that returns a random value that could be chosen for the given factor.}
\usage{
getRandomLevel(object, skip = NA)
}
\arguments{
\item{object}{An object of class doeFactor}

\item{skip}{A list of values that should not be used for the random selection of one factor level.}
}
\value{
The function returns either a single value (numeric or character) for hard- and easy-to-change factors.
        For semi-hard-to-change-factors a list of different values is generated. The length of the list is
        the number of possible values a shtc-factor can take in a given block (=whole plot).
}
\description{
Function that returns a random value that could be chosen for the given factor.
}
\examples{
etc.factor <- new("doeFactor",
                     name="Temperature",
                     levels=c(180, 210),
                     number.levels=as.integer(5),
                     type="continuous",
                     changes="easy")
getRandomLevel(etc.factor)

semi.htc.factor <- new("doeFactor",
                     name="Temperature",
                     levels=c(180, 210),
                     number.levels=as.integer(5),
                     type="continuous",
                     changes="semi.hard",
                     semi.htc.group.size=as.integer(3))
getRandomLevel(semi.htc.factor)

}
\keyword{internal}
