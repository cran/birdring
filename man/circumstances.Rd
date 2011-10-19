\name{circumstances}
\alias{circumstances}
\docType{data}
\title{
EURING-code for finding circumstances
}
\description{
EURING-code for finding circumstances as given in Speek et al. (2001)
}
\usage{data(circumstances)}
\format{
  A data frame with 92 observations on the following 4 variables.
  \describe{
    \item{\code{Code}}{a numeric vector containing the code given in column 78 and 79 in EURING data of the format 2000}
    \item{\code{Name}}{a factor with the names of the finding circumstances}
    \item{\code{BTO}}{a factor with the shorter names of the finding circumstances 
        as defined by the BTO (, http://www.bto.org/volunteer-surveys/ringing/publications/online-ringing-reports) )}
    \item{\code{Description}}{a factor with the description of the finding circumstances}
    \item{\code{Date.Updated}}{a numeric vector}
  }
}
\source{
http://www.euring.org
}
\references{
Speek, G., Clark, J.A., Rhode, Z., Wassenaar, R.D. & van Noordwijk, A.J. (2001) The EURING exchange-code 2000. Dutch Ringing Scheme, Heteren.
}
\examples{
data(circumstances)
circumstances
}
\keyword{datasets}
