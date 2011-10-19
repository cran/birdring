\name{orthodrom.dist}
\alias{orthodrom.dist}
\title{
orthodromic distance and direction
}
\description{
calculates the orthodromic (great circle, shortest distance) distance between two points on the earth
}
\usage{

orthodrom.dist(x1, y1, x2, y2)
}

\arguments{
  \item{x1}{
x-coordinate of the first point (in decimal coordinates), can be a scalar or a vector
}
  \item{y1}{
y-coordinate of the first point (in decimal coordinates), can be a scalar or a vector
}
  \item{x2}{
x-coordinate of the second point (in decimal coordinates), can be a scalar or a vector
}
  \item{y2}{
y-coordinate of the second point (in decimal coordinates), can be a scalar or a vector
}
}
\details{
The function is a wrapper of the distMeeus function of the geosphere package.
}
\value{
gives back a number or a vector with the distances in km
between the two points on earth.
}
\references{
Imboden, C., Imboden D. (1972) Orthodromic and loxodromic formula for the calculation of distance and direction between ringing and finding place. Vogelwarte 26: 336-346.
}
\author{
Fraenzi Korner-Nievergelt
}
\section{Warning}{
see details
}

\seealso{
\code{\link{decimal.coord}}
}
\examples{

ringingx<-7.30
ringingy<-47.41
findingx<-5.1
findingy<-32.01
rxdec<-decimal.coord(ringingx)
rydec<-decimal.coord(ringingy)
fxdec<-decimal.coord(findingx)
fydec<-decimal.coord(findingy)     

orthodrom.dist(rxdec, rydec, fxdec, fydec) 
}
\keyword{math}
\keyword{misc}
\keyword{spatial}
