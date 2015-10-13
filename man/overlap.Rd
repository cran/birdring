\name{overlap}
\alias{overlap}
\title{
Overlap between the prior and posterior distribution
}
\description{
Gives the overlap of two distributions (such as a prior and a posterior distribution) based on one sample of simulated values from each distribution
}
\usage{
overlap(posterior, prior, from = 0, to = 1, nsim = 1e+05, edge.of.parameter.space=FALSE)
}
\arguments{
  \item{posterior}{
A numeric vector, a sample of simulated random values from the posterior distribution
}
  \item{prior}{
A numeric vector, a sample of simulated random values from the prior distribution
}
  \item{from}{
Lower limit of the parameter space over which the posterior and prior distributions are compared.
}
  \item{to}{
Upper limit of the parameter space over which the posterior and prior distributions are compared.
}
  \item{nsim}{
Number of simulated values used for the Monte Carlo simulation to measure the overlap.
}
  \item{edge.of.parameter.space}{
logical value; Two different methods are implemented to 
calculate the overlap.  First (edge.of.parameter.space=FALSE), smoothers are 
fitted to the histograms of the 
simulated values from the posterior and prior distributions, and the overlap is
calculated based on this smoothed density functions. This has the advantage to 
be more exact when the number of simulated values from the posterior distribution is small.
However, it can be unreliable when the mean of the posterior distribution is 
close to the edge of the parameter space. In such cases (edge.of.parameter.space=TRUE), 
it is more reliable to 
calculate the overlap directly from histograms of the simulated values from the
posterior and prior distributions. See also details. 
}

}
\details{
If edge.of.parameter.space=FALSE, the function first uses the function density 
to obtain density functions of the 
prior and posterior distributions and then the overlap is measured by a 
Monte Carlo simulation.
If edge.of.parameter.space=TRUE, two histrograms of the simulated values from the
posterior and prior distributions are drawn with 999 classes and breaks 1000 
equally spaced values between from and to. The overlap is then calculated directly 
from these histograms.
}
\value{
a numeric value which is an approximation of the proportion of the overlap 
of the posterior with the prior distribution.
}
\references{
Gimenez, O., S. P. Brooks, et al. (2009). Weak identiability in models for mark-recapture-recovery data. 
Modelling Demographic Processes in Marked Populations.
Series: Environmental and Ecological Statistics. D. L. Thomson, E. G. Cooch and M. J. Conroy.
}
\author{
Fraenzi Korner-Nievergelt
}

\seealso{
\code{\link{density}}
}
\examples{
prior <- rbeta(2000, 1,1)
posterior <- rbeta(2000, 14, 35)
overlap(posterior, prior)
}
\keyword{ Bayesian data analysis }
\keyword{ distributions }
