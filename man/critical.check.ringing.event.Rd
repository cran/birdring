\name{critical.check.ringing.event}
\alias{critical.check.ringing.event}

\title{
Checking whether Ringing Event Exists for each Individual Bird.
}
\description{
Adds a variable ringing.event which is TRUE if exactly one ringing event exists 
and FALSE otherwise.
}
\usage{
critical.check.ringing.event(dat, id = "birdID")
}
\arguments{
  \item{dat}{
a EURING data, read in using read.euring2000plus function
}
  \item{id}{
  The name of the variable that contains the individual identifier, 
  normally a combination of scheme and ring number
}
}

\value{
A new variable called "ringing.event" is added to the data frame.
}

\author{
Kasper Thorup and Fraenzi Korner-Nievergelt
}



\examples{
data(lancol)
lancol$birdID <- paste(lancol$scheme, lancol$ring)
critical.check.ringing.event(lancol)

}

\keyword{misc}
