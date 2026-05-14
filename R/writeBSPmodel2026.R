#' Title
#'
#' @param rprior 
#' @param split 
#'
#' @returns
#' @export
#'
#' @examples
writeBSPmodel2026 <- function(rprior=0.2000379,split=FALSE){
  
  rval=paste0('r ~ dlnorm(',round(log(rprior)-(0.5*(1/3.252)),4),',3.252)') 
  
  part1 =  "model
{

#prior for r based on info from swain"
  
  part2= "# prior distribution of K based on EPP 100,30 
K~dlnorm(4.562,11.6)



# prior distribution of q's
pq.splate~dgamma(1,1)
q.splate<-1/pq.splate

pq.fallcam ~ dgamma(1,1)
q.fallcam<-1/pq.fallcam

pq.spearly~dgamma(1,1)
q.spearly<-1/pq.spearly

pq.cabs~dgamma(1,1)
q.cabs<-1/pq.cabs

pq.cabf~dgamma(1,1)
q.cabf<-1/pq.cabf


# Prior for process noise, sigma
sigma ~ dunif(0,10)
isigma2 <- pow(sigma, -2)
sigmadev <-sigma+1
isigmadev2<- pow(sigmadev, -2)


# Prior for observation errors, tau. 
a0<-1
b0<-1
tau.splate~dgamma(a0,b0)
itau2.splate <- 1/tau.splate

tau.fallcam~dgamma(a0,b0)
itau2.fallcam <- 1/tau.fallcam

tau.spearly~dgamma(a0,b0)
itau2.spearly <- 1/tau.spearly

tau.cabs~dgamma(a0,b0)
itau2.cabs <- 1/tau.cabs

tau.cabf~dgamma(a0,b0)
itau2.cabf <- 1/tau.cabf


# Prior for initial population size as proportion of K, P[1]. Limited between 0.001 and 5.
# In jags, can't things be limited by using T instead of I?  Added that below.
Pin~dunif(0.5, 1)
Pm[1] <- log(Pin)
P[1] ~ dlnorm(Pm[1], isigma2) T(0.001,5)
P.res[1]<-log(P[1])-Pm[1]


# State equation - SP Model. 
# This is the sigma (process area) section, where it was permitted to vary from 2014-2016.
# Added Ts in to replace Is below to restrict range. 
for (t in 2:(54))   { 
Pm[t] <- log(max(P[t-1] + r*P[t-1]*(1-P[t-1]) - L[t-1]/K, 0.0001))
P[t] ~ dlnorm(Pm[t], isigma2) T(0.001,5)
P.res[t]<-log(P[t])-Pm[t]
}
for (t in 55:(57))   { 
Pm[t] <- log(max(P[t-1] + r*P[t-1]*(1-P[t-1]) - L[t-1]/K, 0.0001))  
P[t] ~ dlnorm(Pm[t], isigmadev2) T(0.001,5)
P.res[t]<-log(P[t])-Pm[t]
}
for (t in 58:(N))   { 
Pm[t] <- log(max(P[t-1] + r*P[t-1]*(1-P[t-1]) - L[t-1]/K, 0.0001))
P[t] ~ dlnorm(Pm[t], isigma2) T(0.001,5)
P.res[t]<-log(P[t])-Pm[t]
}


# Observation equations 
#any new data series need to be added below (in 2024, spring cab added, in 2026 fall cab added)
for (t in 32:(N)) {
Isplatem[t] <- log(q.splate*K * P[t])
Isplate[t] ~ dlnorm(Isplatem[t], itau2.splate)
}
for (t in 31:(N)) {
Ifallcamm[t] <- log(q.fallcam*K * P[t])
Ifallcam[t] ~ dlnorm(Ifallcamm[t], itau2.fallcam)
}
for (t in 25:(31)) {
Ispearlym[t] <- log(q.spearly*K * P[t])
Ispearly[t] ~ dlnorm(Ispearlym[t], itau2.spearly)
}
for (t in 55:(N)) {
Icabsm[t] <- log(q.cabs*K * P[t])
Icabs[t] ~ dlnorm(Icabsm[t], itau2.cabs)
}
for (t in 64:(N)) {
Icabfm[t] <- log(q.cabf*K * P[t])
Icabf[t] ~ dlnorm(Icabfm[t], itau2.cabf)
}


# Output. Using the proportion and K to estimate biomass, B. 
for(t in 1:N) {
B[t] <- P[t] * K

#Zp[t] <- (L[t]/K+M[t]/K)
#Z[t]<-Zp[t]*K
F[t]<-L[t]/B[t]
#F[t]<- Z[t]-M[t]/K

#M[t]~dunif(0.0001,1000)


#Biomass Ratio: Showing what percent the stock would be at if fished at MSY for a given year, t
Bratio[t] <- B[t]/BMSY
}

#F Ratio: indicates the ratio of fishing mortality to that estimated for FMSY. 
#e.g. 1.65=65% higher than that estimated for FMSY
for(t in 1:N) {
Fratio[t] <- F[t]/FMSY
}


# further management parameters and predictions:
MSP <- r*K/4;

#MSP<-FMSY*BMSY

#FMSY<-r/(pow((shape+1),(1/shape)))
FMSY<-r/2

#EFMSY.f.cam<-r/2*q.f.cam
BMSY<-K/2
#BMSY<-K/(pow((shape+1),(1/shape)))


#generate replicate data sets
#spring late
for (i in 32:N){
	Isplate.rep[i] ~ dlnorm(Isplatem[i],itau2.splate)
p.smaller.splate[i] <- step(log(Isplate[i])-log(Isplate.rep[i]))
#residuals of log values of replicate data
	res.Isplate.rep[i] <- log(Isplate[i])-log(Isplate.rep[i])
}
#fall campelen
for (i in 31:N){
	Ifallcam.rep[i] ~ dlnorm(Ifallcamm[i],itau2.fallcam)
p.smaller.fallcam[i] <- step(log(Ifallcam[i])-log(Ifallcam.rep[i]))
#residuals of log values of replicate data
	res.Ifallcam.rep[i] <- log(Ifallcam[i])-log(Ifallcam.rep[i])
}
#spring early
for (i in 25:31){
	Ispearly.rep[i] ~ dlnorm(Ispearlym[i],itau2.spearly)
p.smaller.spearly[i] <- step(log(Ispearly[i])-log(Ispearly.rep[i]))
#residuals of log values of replicate data
	res.Ispearly.rep[i] <- log(Ispearly[i])-log(Ispearly.rep[i])
}
#spring modified campelen and teleost 
for (i in 55:N){
	Icabs.rep[i] ~ dlnorm(Icabsm[i],itau2.cabs)
p.smaller.cabs[i] <- step(log(Icabs[i])-log(Icabs.rep[i]))
#residuals of log values of replicate data
	res.Icabs.rep[i] <- log(Icabs[i])-log(Icabs.rep[i])
}
#fall modified campelen
for (i in 64:N){
	Icabf.rep[i] ~ dlnorm(Icabfm[i],itau2.cabf)
p.smaller.cabf[i] <- step(log(Icabf[i])-log(Icabf.rep[i]))
#residuals of log values of replicate data
	res.Icabf.rep[i] <- log(Icabf[i])-log(Icabf.rep[i])
}

} 
    "

WF_model=paste (part1,rval,part2,sep='\n')

return(WF_model)
}