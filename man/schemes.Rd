\name{schemes}
\alias{schemes}
\docType{data}
\title{
EURING-code for ringing scheme
}
\description{
EURING-code for ringing scheme as given in Speek et al. (2001)
}
\usage{data(schemes)}
\format{
  A data frame with 39 observations on the following 7 variables.
  \describe{
    \item{\code{Code}}{a factor with the EURING code}
    \item{\code{Country}}{a factor with the country of the scheme}
    \item{\code{Centre}}{a factor with the city of the place of the scheme}
    \item{\code{EURING}}{a factor with levels \code{Y}}
    \item{\code{Current}}{a factor with levels \code{Y}}
    \item{\code{Date.Updated}}{a factor}
    \item{\code{Notes}}{a factor with additional remarks}
  }
}
\source{
http://www.euring.org
}
\references{
Speek, G., Clark, J.A., Rhode, Z., Wassenaar, R.D. & van Noordwijk, A.J. (2001) The EURING exchange-code 2000. Dutch Ringing Scheme, Heteren.
}
\examples{
data(schemes)
}
\keyword{datasets}
