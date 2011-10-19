\name{birdstate}
\alias{birdstate}
\title{
 Transformation of condition into state
}
\description{
Transforms the numeric code of the variable condition into a factor defining the 
state of the bird (dead, alive, sick).
}
\usage{
birdstate(x)
}
\arguments{
  \item{x}{
  variable condition with numeric code for the condition of the bird as given in the original EURING data
}
}
\value{
a factor with the levels "dead", "alive", "sick" and "unknown"
}
\references{
du Feu et al. (2012) EURING Exchange Code 2000+. www.euring.org
}
\author{
F. Korner-Nievergelt
}

\seealso{
  \code{\link{codes2names}}
}
\examples{
  condition <- 0:9
  birdstate(condition)
}
\keyword{transformation}
\keyword{code}
