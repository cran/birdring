\name{conditions}
\alias{conditions}
\docType{data}
\title{
EURING-code for finding conditions
}
\description{
EURING-code for finding conditions as given in Speek et al. (2001)
}
\usage{data(conditions)}
\format{
  A data frame with 10 observations on the following 2 variables.
  \describe{
    \item{\code{Code}}{a numeric vector containing the code given in column 77 in EURING data of the format 2000}
    \item{\code{Description}}{a factor with the finding conditions}
  }
}
\source{
http://www.euring.org
}
\references{
Speek, G., Clark, J.A., Rhode, Z., Wassenaar, R.D. & van Noordwijk, A.J. (2001) The EURING exchange-code 2000. Dutch Ringing Scheme, Heteren.
}
\examples{
data(conditions)
conditions
}
\keyword{datasets}
