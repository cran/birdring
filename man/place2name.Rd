\name{place2name}
\alias{place2name}

\title{
Decode Euring place codes
}
\description{
Converts Euring country and region codes into their text names.
}
\usage{
place2name(x)
}

\arguments{
  \item{x}{
a variable containing four character Euring place code.
}
}
\details{
A warning is given if any place codes are not matched, these will be returned as NA.
}
\value{
a dataframe with three columns: 'country.name', 'region.name', 'current', the last of which indicates whether the code is currently use or deprecated.
}

\author{
Rob Robinson
}



\examples{
examplecodes <- c('BL20', 'GBTR')
place2name(examplecodes)

}

\keyword{misc}
\keyword{transformation}
