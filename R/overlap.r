# calculate the overlap between prior and posterior distribution as described in 
#Gimenez, O., S. P. Brooks, et al. (2009). Weak identiability in models for mark-recapture-recovery data. Modelling Demographic Processes in Marked Populations:
#Series: Environmental and Ecological Statistics. D. L. Thomson, E. G. Cooch and M. J. Conroy.

#--------------------------------------------------------------------
overlap <- function(posterior,prior, from=0, to=1, nsim=100000){
# posterior: simulated posterior distribution
# prior: simulated prior distribution or a character with the distribution name. currently implemented: "unif01"
# from: lower and upper limit of the parameter space. This space is sliced into 1000 slices for the Monte Carlo Integration, therefore should be chosen large enough but not too large
# nsim: number of Monte Carlo simulations to be used for the Monte-Carlo integration (note, that this number is multiplied by 1000)
denspost <- density(posterior, from=from, to=to, n=1000)
if(!is.character(prior)){
 densprior <- density(prior, from=from, to=to, n=1000)
 yran <- runif(nsim, 0,max(c(denspost$y, densprior$y)))
 areatotal <- max(c(denspost$y, densprior$y)) * (to-from)
 }
if(is.character(prior)){
 yran <- runif(nsim, 0,max(c(denspost$y, 1)))
 areatotal <- max(c(denspost$y, 1)) * (to-from)
 }
kk <- 0
for(ii in 1:1000){ # count the numbers of points being in the overlap area
  if(is.numeric(prior))  ninoverlap <- sum(yran < min(c(denspost$y[ii], densprior$y[ii])))
  if(prior[1] == "unif01") ninoverlap <- sum(yran < min(c(denspost$y[ii], 1)))
  kk <- kk+ninoverlap
  }
ol <- kk/(nsim*1000) * areatotal
return(ol)
}# close function
