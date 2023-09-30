\name{read.euring2000plus}
\alias{read.euring2000plus}
\title{
Reads EURING data of the format 2000\+ into R
}
\description{
Reads EURING data (format EURING exchange code 2000\+) into R and creates a data.frame
}
\usage{
read.euring2000plus(filename, quote="", ...)
}
\arguments{
  \item{filename}{
character string of the psv-file obtained from EURING (format: exchange code 2000\+)
}
  \item{quote}{
default is to ignore quotemarks as much as possible, see notes.
}
  \item{...}{
extra arguments to pass to read.table
}
}
\value{
A data.frame containing the following variables from the EURING 2000\+ code:\cr
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
country \tab character \tab country extracted from place code\cr
region \tab character \tab region (county) extracted from place code\cr        
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
wing.length  \tab numeric \tab maximum chord measurement in mm, Svensson (1992)\cr
third.primary \tab numeric \tab  Length of the third primary feather, in mm\cr
state.of.wing.point \tab character \tab condition of the longest primary feather\cr
mass \tab numeric \tab body, in grams, measured to a maximum precision of 0.1 g. \cr
moult \tab character \tab  code for main, clearly identifiable moult states\cr
plumage.code \tab character \tab  extra information to enhance age code\cr
hind.claw \tab numeric \tab  in mm, for details of method see Svensson (1992)\cr
bill.length \tab numeric \tab  in mm, see du Feu (2012).\cr
bill.method \tab character \tab  a single letter code, for details see Svensson (1992).\cr
total.head.length \tab numeric \tab  in mm, see du Feu (2012)\cr
tarsus \tab numeric \tab  in mm, see du Feu (2012)\cr
tarsus.method \tab character \tab tarsus method used, see du Feu (2012)\cr
tail.length \tab numeric \tab in mm, details of the method see Svensson (1992).\cr
tail.difference \tab numeric \tab according to Svensson (1992)\cr
fat.score \tab numeric \tab  fat score, see du Feu (2012) for details.\cr
fat.score.method \tab character \tab fat score method used, see du Feu (2012)\cr
pectoral.muscle \tab numeric \tab state of pectoral muscle\cr
brood.patch \tab character \tab  state of the brood patch in the breeding season\cr
primary.score \tab numeric \tab  the sum of the primary moult scores\cr
primary.moult \tab character \tab state of the 10 primary feathers\cr
old.greater.coverts \tab numeric \tab the number of unmoulted greater coverts\cr
alula  \tab character \tab state of the three alula feathers\cr
carpal.covert \tab numeric \tab state of moult of the carpal covert: 0=old, 1=new.\cr
sexing.method \tab character \tab for code description see du Feu (2012).\cr
}
For three additional optional variables, see du Feu (2012).
}
\note{
Quotemarks, apostrophes and backticks can cause havoc when reading in files; if things do not work as you expect, check for their presence of these and consider removing!
}
\references{
du Feu et al. 2012: EURING exchange-code 2000\+. www.euring.org
}
\author{
Fraenzi Korner-Nievergelt
}


\examples{
filename <- system.file("extdata", "REEWAExport_50.psv", package = "birdring")
dat <-  read.euring2000plus(filename)
str(dat)
}

\keyword{data}
\keyword{manip}
