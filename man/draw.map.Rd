\name{draw.map}
\alias{draw.map}
\title{
Draws a map of the palearctic region
}
\description{
Draws a map of the palearctic region that can be used as basis for showing ring 
reencounter data.
}
\usage{
draw.map(a=-10, b=30, c=23, d=65, bbox=NULL, fill = TRUE, col.land = grey(0.5), 
  col.water = "white", border = NA, detail = FALSE, box = TRUE, axes = FALSE, 
  las = 1, cex.axis = 0.8, dist.axislab = 0.2, whichaxis = c(1:4), tck = -0.005, 
  mercator = FALSE, mar = rep(0.5, 4), asp=NA)
}

\arguments{
  \item{a}{
left edge of the map (longitude), can be used alternatively to the argument bbox
}
  \item{b}{
right edge of the map (longitude), can be used alternatively to the argument bbox
}
  \item{c}{
lower edge of the map (latitude), can be used alternatively to the argument bbox
}
  \item{d}{
upper edge of the map (latitude), can be used alternatively to the argument bbox
}
  \item{bbox}{
a numeric vector giving the map boundary in decimal degrees as 
c(long_min, long_max, lat_min, lat_max),
can be used alternatively to the arguments a, b, c, and d.
}

  \item{fill}{
logical value, TRUE = land and water masses are filled with a color
}
  \item{col.land}{
color of the land
}
  \item{col.water}{
color of the water
}
  \item{border}{
color of the coastline, NA = no coastline is drawn
}
  \item{detail}{
if TRUE: an alternative way to draw coastlines
}
  \item{box}{
logical, TRUE = a box is drawn around the map
}
  \item{axes}{
logical, indicates whether axis should be drawn
}
  \item{las}{
see par
}
  \item{cex.axis}{
see par
}
  \item{dist.axislab}{
distance from the axis to the axis labels
}
  \item{whichaxis}{
vector of length 1 to 4 containing the axis that should be drawn, default is 
c(1,2,3,4), i.e. all axes
}
  \item{tck}{
see par
}
  \item{mercator}{
logical, if TRUE a mercator projection is plotted. see details below
}
  \item{mar}{
see par
}
  \item{asp}{ if set to 1 x and y axes are scaled equally (not recommended if 
  mercator=TRUE), see par
}
}
\details{
If a mercator projection is used, the values for a, b, c and d will be adjusted.
}
\value{
this is a plotting function only
}
\references{
Rummler, H. 2002. Mercatorkarte und hyperbolische Geometrie. Elem. Math., 57, 168-173.
}
\author{
Fraenzi Korner-Nievergelt
}
\note{
some times it does not do what we expect....
}


\examples{

draw.map(-18, 50, -5, 56, col.land="white", col.water=grey(0.5), 
  detail=FALSE, axes=TRUE)

draw.map(a=0, b=30, c=35, d=60, col.land="white", col.water=grey(0.5), 
  detail=FALSE, axes=TRUE, mercator=TRUE)
}


\keyword{misc}
\keyword{graphics}

