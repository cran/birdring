\name{coastEU}
\alias{coastEU}
\docType{data}
\title{
coordinates of the coastline of Europe
}
\description{
coordinates of the coastline of Europe used by the function draw.map
}
\usage{data(coastEU)}
\format{
  A data frame with 7528 observations on the following 4 variables.
  \describe{
    \item{\code{x}}{a numeric vector, longitude}
    \item{\code{y}}{a numeric vector, latitude}
    \item{\code{name}}{name of the island} 
    \item{\code{abstract}}{a numeric vector}
  }
}
\details{
the data is used by the function draw.map
}
\source{
http://rimmer.ngdc.noaa.gov/mgg/coast/getcoast.html
}
\references{
http://rimmer.ngdc.noaa.gov/mgg/coast/getcoast.html
}
\examples{
data(coastEU)
head(coastEU)

}
\keyword{datasets}
