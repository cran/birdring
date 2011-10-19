\name{mdy.date}
\alias{mdy.date}

\title{Convert to Julian Dates - this is a copy of the function mdy.date from the package date
}
\description{
Given a month, day, and year, returns the number of days since January 1, 1960.
}
\usage{
mdy.date(month, day, year, nineteen = TRUE, fillday = FALSE, fillmonth = FALSE)
}

\arguments{
  \item{month}{
vector of months.
}
  \item{day}{
vector of days.
}
  \item{year}{
vector of years.
}
  \item{nineteen}{
if TRUE, year values between 0 and 99 are assumed to be in the 20th century A.D.; 
otherwise, if FALSE, they are assumed to be in the 1st century A.D.
}
  \item{fillday}{
if TRUE, then missing days are replaced with 15.
}
  \item{fillmonth}{
if TRUE, then a missing month causes the month and day to be set to 7/1.
}
}
\details{
The date functions are particularly useful in computing time spans, such as number 
of days on test, and similar functions can be found in other statistical packages. 
The baseline date of Jan 1, 1960 is, of course, completely arbitrary (it is the 
same one used by SAS).

The fillday and fillmonth options are perhaps useful only to the author and a 
very few others: we sometimes deal with patients whose birth date was in the 1800's, 
and only the month or even only the year is known. When the interval is greater 
than 80 years, a filler seems defensible.
}
\value{
a vector of Julian dates.
}
\references{
Press, W. H., Teukolsky, S. A., Vetterling, W. T., and Flannery, B. P. (1992). 
Numerical Recipes: The Art of Scientific Computing (Second Edition). 
Cambridge University Press.

Terry Therneau and Thomas Lumley and Kjetil Halvorsen
  and Kurt Hornik (2012). date: Functions for handling
  dates. R package version 1.2-33.
}

\seealso{
see documentation and other functions in the package date
}
\examples{
mdy.date(3, 10, 53)
}
\keyword{transformation}

