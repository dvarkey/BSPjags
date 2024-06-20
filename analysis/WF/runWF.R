

data <- WFdata$data
inits <- WFdata$inits
parameters <- WFdata$parameters

mod1=writeBSPmodel(rprior=0.2000379)
mod2=writeBSPmodel(rprior=0.1)

vv=runModel(modeltxt=textConnection(mod1), data=data,inits=inits, parameters=parameters, n.chains=3, n.adapt=100, n.burnin=1000, n.iter=2000,n.thin=10)
vp=runProjections(nY=3,output=vv,Yield)
