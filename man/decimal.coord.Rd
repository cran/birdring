\name{decimal.coord}
\alias{decimal.coord}
\title{decimal coordinates
}
\description{
transforms degrees and minutes into decimal coordinates
}
\usage{
decimal.coord(x)
}
\arguments{
  \item{x}{
scalar or vector containing degrees and minutes, e.g. for 45 degrees and 30 minutes = 45.3
}
}
\value{
a scalar or a vector containing the decimal coordinates
}
\references{
This function just multiplies the decimals by 5/3... (no reference)
}
\author{
Fraenzi Korner-Nievergelt
}

\examples{
decimal.coord(45.3)
}
\keyword{ coordinates}
\keyword{ geographic location }
