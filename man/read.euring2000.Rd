\name{read.euring2000}
\alias{read.euring2000}
\title{
Reads EURING data  into R
}
\description{
Reads EURING data (format EURING exchange code 2000) into R and creates a data.frame
}
\usage{
read.euring2000(filename)
}
\arguments{
  \item{filename}{
character string of the data file obtained from EURING (format: exchange code 2000)
}
}
\value{
A data.frame containing the following variables from the EURING 2000 code:\cr
\tabular{rll}{
scheme \tab Factor \tab ringing scheme (code)\cr
id.method \tab Factor \tab identification method\cr
ring \tab Factor \tab ring number\cr
ring.verif \tab character \tab verification of the metal ring\cr
metal.ring.info \tab character \tab metal ring information\cr
marks.info \tab character \tab other marks information\cr
spec.byringer \tab character \tab species + subsp., mentioned by person\cr
spec.byscheme \tab character \tab species + subsp., concluded by scheme\cr 
manipulated \tab character \tab manipulated\cr
moved \tab character \tab moved before the (re)capture/recovery\cr 
catching.method \tab character \tab catching method\cr
catching.lures \tab character \tab catching lures\cr
sex.byringer \tab character \tab sex, by the person who handled the bird\cr      
sex.byscheme \tab character \tab sex, as concluded by scheme\cr
age.byringer \tab character \tab age, by the person who handled the bird\cr
age.byscheme \tab character \tab age, as concluded by scheme\cr   
status \tab character \tab status\cr 
broodsize \tab character \tab broodsize\cr 
pullus.age \tab character \tab pullus age\cr 
pullus.age.acc \tab character \tab accuracy of pullus age\cr 
day \tab numeric \tab day of record, derived from date\cr
month \tab numeric \tab month of record, derived from date\cr
year \tab numeric \tab year of record, derived from date\cr
date.acc \tab character \tab accuracy of date\cr
time \tab character \tab time\cr
place.code \tab character \tab place code\cr
country \tab character \tab country derived from place code\cr
region \tab character \tab region (county) derived from place code\cr        
lat \tab numeric \tab latitude in decimal coordinates\cr
lon \tab numeric \tab longitude in decimal coordinates\cr
coord.acc \tab character \tab accuracy of coordinates\cr     
condition \tab character \tab condition\cr     
circumstances \tab character \tab finding circumstances\cr     
circumstances.presumed \tab character \tab finding circumstances presumed\cr   
euring.codeid \tab character \tab EURING-code identifier\cr   
distance \tab numeric \tab distance to place of ringing (km)\cr 
direction \tab numeric \tab direction to place of ringing (km)\cr 
time.elapsed  \tab numeric \tab time since ringing (days)\cr 
}
}
\references{
Speek et al. 2007: The EURING exchange-code 2000. www.euring.org
}
\author{
Fraenzi Korner-Nievergelt
}


\examples{
filename <- system.file("extdata", "RBShrike2000.TXT", package = "birdring")
dat <-  read.euring2000(filename)
str(dat)
}

\keyword{data}
\keyword{manip}
