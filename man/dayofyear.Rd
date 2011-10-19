\name{dayofyear}
\alias{dayofyear}
\title{
The day of the year
}
\description{
Gives the day of the year, i.e. the number of days since the 31 December of the previous year.
}
\usage{
dayofyear(m, d, y = 1960)
}

\arguments{
  \item{m}{
month (integer between 1 and 12)
}
  \item{d}{
day (integer between 1 and 31)
}
  \item{y}{
year (integer such as 2013)
}
}
\value{
numeric value, day of the year
}
\references{
no references
}
\author{
Fraenzi Korner-Nievergelt
}

\seealso{
\code{\link{mdy.date}}
}
\examples{
dayofyear(3,1,2013)
dayofyear(3,1,2012)
}
\keyword{ date }
