\name{mercatorlat}
\alias{mercatorlat}
\title{
gives the streching factor of the latitude to draw a mercator map
}
\description{
this function is only used internally by the function draw.map
}
\usage{
mercatorlat(x)
}

\arguments{
  \item{x}{
latitude in decimal coordinates
}
}

\value{
numeric: latitude to draw a mercator map
}
\references{
Rummler, H. (2002) Mercatorkarte und hyperbolische Geometrie. Elem. Math., 57, 168-173.
}
\author{
Fraenzi Korner-Nievergelt
}
\examples{
mercatorlat(41.6)
}

\keyword{misc}

