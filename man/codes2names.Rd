\name{codes2names}
\alias{codes2names}

\title{
Transforms codes of circumstances, condition, species and sheme into string
}
\description{
The function gives for the numeric codes for circumstances, condition and species the interpretable string name as given in the EURING code.
The alphabetic scheme code is transformed into the town and country name of the scheme. 
}
\usage{
codes2names(x, variable = "circumstances", type="euring")
}
\arguments{
  \item{x}{
variable circumstances, condition, species or scheme from the output of \cr
read.EURING2000plus.
}
  \item{variable}{
character that declares the name of the variable x, should be one of c("circumstances" [default], 
"conditions", "schemes", "species").
}
  \item{type}{
character that declares which names should be used, the one defined in the EURING 
manual, "euring" (default), or the ones used in the BTO ringing reports, "bto", 
(http://www.bto.org/volunteer-surveys/ringing/publications/online-ringing-reports).  
}
}

\value{
a factor with levels corresponding to the names of the EURING code
}
\references{
http://www.euring.org/data_and_codes/euring_code_list/index.html
}
\author{
F. Korner-Nievergelt
}

\seealso{
\code{\link{scheme2country}}
}
\examples{
# circumstances
examplecode <- c(20, 21, 35, 80)
codes2names(examplecode) 

# conditions
examplecodes <- c(0:9)
codes2names(examplecodes, variable="conditions")

# schemes
codes2names("BGS", variable="schemes")
}

\keyword{transformation}
\keyword{codes}
