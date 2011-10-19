\name{places}
\alias{places}
\docType{data}
\title{
EURING-code for finding locations
}
\description{
EURING-code for places as given in Speek et al. (2001)
}
\usage{data(places)}
\format{
  A data frame with 1852 rows of the following 5 variables.
  \describe{
    \item{\code{country}}{name of country. non-ASCII characters have been exchanged, please change back for Cote d Ivoire and Suqutra}
    \item{\code{region}}{name of region. non-ASCII characters have been exchanged, please check an change back!}
    \item{\code{code}}{the code used by EDB}
    \item{\code{current}}{}
    \item{\code{date.updated}}{date of update}
}
}
\source{
http://www.euring.org
}
\references{
Speek, G., Clark, J.A., Rhode, Z., Wassenaar, R.D. & van Noordwijk, A.J. (2001) The EURING exchange-code 2000. Dutch Ringing Scheme, Heteren.
}
\examples{
data(places)
}
\keyword{datasets}
