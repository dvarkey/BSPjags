

data <- WFdata$data
inits <- WFdata$inits
parameters <- WFdata$parameters

mod1=writeBSPmodel(rprior=0.2000379)
writeLines(mod1, "analysis/WF/mod1.txt")

mod1Run=runModel(modeltxt=textConnection(mod1), data=data,inits=inits, parameters=parameters, n.chains=3, n.adapt=100, n.burnin=1000, n.iter=2000,n.thin=10)
mod1Out=getOutputs(mod1Run)


nYproj=4 #number of projection years
mod1Proj1=runProjections(nY=nYproj,output=mod1Run,Yield=c(0.4,0,0,0)) #output dimension (Year,n.iter/n.thin,n.chains)
mod1Proj2=runProjections(nY=nYproj,output=mod1Run,Yield=c(0.4,0.5,0.6,0.6)) #output dimension (Year,n.iter/n.thin,n.chains)

Byplus1=as.numeric(mod1Proj1$Bproj[1,1])

#RMSE::fnCall(fnName='Flinear',Bt=Byplus1,Blim=mod1Out$Blim,Btrigger=mod1Out$Btrigger,Ftarget=mod1Out$Ftarget,x50=x50)
# 
# WF_PAleaf <- RMSE::plotLeafStock(
#   Blim = mod1Out$Blim,
#   Btrigger = mod1Out$Btrigger,
#   Ftarget = mod1Out$Ftarget,
#   x50UL = 0.25,
#   x50ML = 0.5,
#   x50LL = 0.75,
#   Fmin = 0.0001,
#   stockval=mod1Out$Bterminal,
#   stockF=mod1Out$Fterminal); WF_PAleaf
# 


