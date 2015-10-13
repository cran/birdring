\name{loxodrom}
\alias{loxodrom.dir}
\alias{loxodrom.dist}
\title{
loxodromic distance and direction
}
\description{
calculates the loxodromic distance and direction between two points on the earth
}
\usage{
loxodrom.dir(x1, y1, x2, y2, epsilon = 1e-06)

loxodrom.dist(x1, y1, x2, y2, epsilon = 1e-04, package="geosphere")
}

\arguments{
  \item{x1}{
x-coordinate/longitude of the first point (in decimal coordinates), can be a scalar or a vector
}
  \item{y1}{
y-coordinate/latitude of the first point (in decimal coordinates), can be a scalar or a vector
}
  \item{x2}{
x-coordinate/longitude of the second point (in decimal coordinates), can be a scalar or a vector
}
  \item{y2}{
y-coordinate/latitude of the second point (in decimal coordinates), can be a scalar or a vector
}
  \item{epsilon}{
a threshold value for considering a number as zero. See details.
}
\item{package}{
if "geosphere" (default) the function is based on the geosphere package, if "birdring" the function written by F. Korner is used. The latter is less reliable.
}
}
\details{
If you use the birdring package, please, check the results carefully, especially when vectors instead of scalars are
given as arguments. If some distances or directions are obviously wrong (such cases occurred
predominantly when the bird moved exactly into one of the four directions 0, 90, 180 or
270 degrees) then it might help to increase the value of epsilon.
}
\value{
The function loxodrom.dist() gives back a number or a vector with the distances in km
between the two points on earth.
The function loxodrom.dir() gives back a number or a vector with the directions in degees from North (clockwise) between the two points on earth.
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

loxodrom.dist(rxdec, rydec, fxdec, fydec) 

loxodrom.dir(rxdec, rydec, fxdec, fydec)  
}
\keyword{math}
\keyword{misc}
\keyword{spatial}
