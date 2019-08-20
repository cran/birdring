\name{draw.recmap}
\alias{draw.recmap}
\title{
Plot ringing data
}
\description{
Draws a map of ringing data, representing locations as points or lines linking 
recoveries. Optionally a heat map of the locations can also be created
}
\usage{
draw.recmap(data, file='screen', subset=NULL, group=NULL, bbox=NULL, 
  projection='longlat', zoom=2, margin=1, points=0, pcol='red', lines=0, 
  lcol='red', gcircle=FALSE, density=FALSE, grid.size=300, mask='sea', 
  all.data=FALSE, height=1000, width=0, units='px', col='lemonchiffon', 
  bg='lightblue1', border=NA, lwd=1, legend='none', title = NULL, labels=NULL, 
  lcex=0, alpha=0.2, ...)
}
\arguments{
  \item{data}{
a dataframe with, minimally, columns labelled 'lat' and 'lon' giving the co-ordinates 
of locations (in decimal degrees). Note there should be one row per encounter 
(i.e. a recovery will appear as 2+ rows).
}
  \item{file}{
a valid filename to which the map should be plotted. Currently supports png, jpg 
(or jpeg), or tif (tiff) extensions.
}
  \item{subset}{
a character string indicating subset of rows to use, any logical expression involving 
column names (or numbers) should work.
}
  \item{group}{
a character string, the variable name (within data) indicating the grouping variable 
to be displayed in the legend.
}
  \item{bbox}{
a numeric vector giving the map boundary in decimal degrees as c(long_min, long_max, 
lat_min, lat_max). If some data are outwith this box a warning is issued.
}
  \item{projection}{
a character string indicating the projection to be used, currently only 'longlat' or 
'mercator' work.
}
  \item{zoom}{
a number indicating resolution of map to plot: 1 - low resolution suitable for 
global maps; 2 - fine for most purposes (the default); and 3 - for large maps or 
small areas where a higher resolution is needed. 
}
  \item{margin}{
a number (in decimal degrees) to pad the map by around the extreme points.
}
  \item{points}{
an integer, if >0 then points will be plotted at that size, relative to par('cex').
}
  \item{pcol}{
colour to plot the points.
}
  \item{lines}{
an integer, if >0 then lines will be plotted at that width.
}
  \item{lcol}{
colour to plot the lines.
}
  \item{gcircle}{
logical, should the lines be plotted as great circle routes.
}
  \item{density}{
logical, if TRUE overlay kernel density smooth (heat map) generated from data, 
see details.
}
  \item{grid.size}{
numeric, indicating the number of points over which to calculate the density grid, 
increase from the default 300 for a smoother pattern, decrease for quicker computation.
}
  \item{mask}{
if density=TRUE, mask could be either 'land' or 'sea' (the default); this is purely 
presentational, it doesn't alter the kernel calculation.
}
  \item{all.data}{
logical, if bbox is given, should all the data be used in creating the heat map, 
or only those displayed.
}
  \item{height}{
the height of the map canvas (in pixels unless units is given). 
Specify only one of height or width; height will be ignored of both given.
}
  \item{width}{
the width of the map canvas (in pixels unless units is given). Specify only one 
of height or width.
}
  \item{units}{
the units in which height and width are given. Can be px (pixels, the default), 
in (inches), cm or mm.
}
  \item{col}{
colour for land areas, the default colours are designed to also print well in 
greyscale.
}
  \item{bg}{
colour for ocean areas.
}

  \item{border}{
colour to draw the country borders (and coastal outline), use NA to suppress these.
}
  \item{lwd}{
numeric, width of border lines.
}
  \item{legend}{
keyword indicating position from the list "none", "bottomright", "bottom", 
"bottomleft", "left", "topleft", "top", "topright", "right" and "center". Use 
inset=c(x,y) for finer control of placement, where x, y between 0 and 1.
}
  \item{lcex}{
if a legend is included, size of the legend labels relative to par('cex').
}

  \item{alpha}{
if a legend is included, transparency of the legend rectangle, as a proportional 
opacity from 0 (fully transparent) to 1 (opaque).
}
  \item{title}{
if a legend is included, the title to be added in the legend box.
}

  \item{labels}{
vector of character labels to be used in identifying the group levels. If this 
does not match the number of group levels, then it will be ignored.
}
  \item{...}{
other arguments to pass to \code{legend()}.
}
}
\details{
If density=TRUE, additionally overlays a simple bivariate kernel density estimator 
using the \code{\link{kde}} function from package ks. This does not respect 
geography so may give biologically unrealistic results and, in particular, will 
predict effort over the sea or land inappropriately in most cases, this behaviour 
can be hidden by using the 'mask' argument. 
}
\value{
invisibly, a SpatialPolygons object containing the map outline
}
\references{
The country border dataset uses Natural Earth data as presented in the 
\code{\link{rworldmap}} package.
The following was useful in working out the default land and sea colours used \cr
http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf
}
\author{
Rob Robinson}
\note{
all co-ordinates in data should be given as decimal degrees, they will be transformed 
as appropriate. If plotting recovery lines by group, check that each record has the 
same group identifier, since only that attached to the first encounter will be used.
}
\examples{
data(lancol)
if (requireNamespace("rgeos", quietly=TRUE) && packageVersion("rgeos") >= "0.5.1") {
  if (rgeos::get_RGEOS_CheckValidity() == 1L) rgeos::set_RGEOS_CheckValidity(2L)
}
draw.recmap(lancol,lines=2,border='gray',subset="ring=='13B...6919'",gcircle=TRUE,zoom=3)

\dontrun{
draw.recmap(lancol,points=0.5,pcol='blue',density=TRUE, bbox=c(-5,20, 47,58),
  projection='mercator',all.data=TRUE)
draw.recmap(lancol,'d:/example.png',points=1,pcol=c('red','blue','black'),
  group='sex.byringer',legend='bottomleft',labels=c('Female','Male','Unknown'),title='Sex')
}


}
\keyword{graphics}
