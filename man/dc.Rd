\name{dc}
\alias{dc}
\alias{bootci.dc}

\title{Division coefficient

}
\description{The function dc() calculates from the number of ringed birds per group and the number of reencountered
birds per group in each destination area the estimated division coefficients per
group and the estimated re-encounter probabilities per destination area as described in Kania
and Busse (1987). The function bootci.dc() gives the bootstrap confidence intervals for the division coefficients
and the re-encounter probabilities estimated by dc(). For details see Korner-Nievergelt et al.
(2010).
}
\usage{
dc(N, recmatrix, group.names = NA, area.names = NA, start = NA)

bootci.dc(N, recmatrix, interval = 0.95, R = 1000, 
                             group.names = NA, area.names = NA)
}

\arguments{
  \item{N}{a vector containing the number of ringed birds per group
}
  \item{recmatrix}{a matrix containing the number of re-encountered birds per group and area.
The rows of the matrix represent the bird groups; the columns represent the
destination areas.
}
  \item{interval}{proportion of the confidence interval (default = 0.95)
}
  \item{R}{number of bootstrap replicates (default = 1000)
}

  \item{group.names}{facultative vector of group names
}
  \item{area.names}{facultative vector of area names
}
  \item{start}{facultative vector of starting values for the least-square iteration for estimating
the inverse of the recovery probability per destination area. The vector has the
length of the number of areas and contains approximate inverse values of the
recovery probabilities (default = N[1]/recmatrix[1,]/2).
}
}
\value{
  \item{rec.probs}{estimated re-encounter probabilities per destination area}
  \item{division.coef}{estimated division coefficient per group and destination area}
  \item{div.coef.lower}{lower limits of the confidence intervals of the estimated division
coefficients}
  \item{div.coef.upper}{upper limits of the confidence intervals of the estimated division
coefficients}

}
\references{
Kania W,  Busse P (1987) An analysis of the recovery distribution based on finding probabilities. Acta Ornithologica 23: 121-128.
Korner-Nievergelt F, Schaub M, Thorup K, Vock M, Kania W (2010) Estimation of bird distribution based on ring re-encounters: precision and bias of the division coefficient and its relation to multi-state models. Bird Study 57: 56-68.

Example data are from:
Bauthian I, Gossmann F, Ferrand Y, Julliard R. (2007) Quantifying the origin of Woodcock wintering in France. Journal of Wildlife Management 71: 701-705.
}
\author{Fraenzi Korner-Nievergelt
}


\examples{

N<-c(7125, 9661, 5266, 3240, 3643, 3192, 3227)

recmatrix<-matrix(c(22,25, 6, 2, 4, 1, 0, 47, 78, 49, 28, 38,27, 34), ncol=2)
colnames(recmatrix) <- c("Scandinavian", "Eastern")
rownames(recmatrix) <- c("North", "West", "Southwest",
    "Central North", "Central South", "Northeast","Southeast")

dc(N, recmatrix, group.names= c("North", "West", "Southwest",
    "Central North", "Central South", "Northeast","Southeast"),
    area.names= c("Scandinavian", "Eastern"))

}
\keyword{ring recovery}
\keyword{migratory connectivity}
