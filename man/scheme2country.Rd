\name{scheme2country}
\alias{scheme2country}
\title{
Gives the country name from the scheme code
}
\description{
Transforms the scheme codes into country name.
}
\usage{
scheme2country(x)
}
\arguments{
  \item{x}{
variable scheme from the data frame produced by read.EURING2000 or \cr
read.EURING2000plus
}
}
\value{
a factor with levels coresponding to the country names
}
\references{
www.euring.org
}
\author{
F. Korner-Nievergelt
}
\seealso{
\code{\link{codes2names}}
}
\examples{
schemecode <- "HES"
scheme2country(schemecode)

}

\keyword{code}
\keyword{transformation}
