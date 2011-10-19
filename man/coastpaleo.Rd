\name{coastpaleo}
\alias{coastpaleo}
\docType{data}
\title{
coordinates of the palearctic coastline
}
\description{
coordinates of the palearctic coastline  used by the function draw.map. The coordinates were downloaded from the sources indicated below and then cleaned by hand from too small details
}
\usage{data(coastpaleo)}
\format{
  A data frame with 25042 observations on the following 5 variables.
  \describe{
    \item{\code{x}}{a numeric vector of longitude}
    \item{\code{y}}{a numeric vector of latitude}
    \item{\code{name}}{name of the island}
    \item{\code{entry}}{degree of detail to be entered in the map}
    \item{\code{kategorie}}{a factor with levels \code{i} \code{m} \code{s}}
  }
}

\source{
http://rimmer.ngdc.noaa.gov/mgg/coast/getcoast.html
}
\references{
http://rimmer.ngdc.noaa.gov/mgg/coast/getcoast.html
}
\examples{
data(coastpaleo)
head(coastpaleo)

}
\keyword{datasets}
