\name{dmvnorm}
\alias{dmvnorm}
\title{
Multivariate Normal Density and Random Deviates - this function is a copy of the 
function in the package mvtnorm
}
\description{
These functions provide the density function and a random number generator for 
the multivariate normal distribution with mean equal to mean and covariance matrix sigma.
}
\usage{
dmvnorm(x, mean, sigma, log = FALSE)
}
\arguments{
  \item{x}{
Vector or matrix of quantiles. If x is a matrix, each row is taken to be a quantile.
}
  \item{mean}{
Mean vector, default is rep(0, length = ncol(x)).
}
  \item{sigma}{
Covariance matrix, default is diag(ncol(x)).
}
  \item{log}{
Logical; if TRUE, densities d are given as log(d).
}
}
\details{
This function is copied to birdring from mvtnorm. If you use this function,
please use and cite the original function from the package mvtnorm!
}

\references{
 Alan Genz, Frank Bretz, Tetsuhisa Miwa, Xuefei Mi,
  Friedrich Leisch, Fabian Scheipl, Torsten Hothorn
  (2013). mvtnorm: Multivariate Normal and t
  Distributions. R package version 0.9-9995. URL
  http://CRAN.R-project.org/package=mvtnorm

  Alan Genz, Frank Bretz (2009), Computation of
  Multivariate Normal and t Probabilities. Lecture Notes
  in Statistics, Vol. 195., Springer-Verlage, Heidelberg.
  ISBN 978-3-642-01688-2
}
\author{
Friedrich Leisch and Fabian Scheipl
}

\seealso{
see functions and documentations in the package mvtnorm
}
\examples{
dmvnorm(x=c(0,0))
}

\keyword{misc}
