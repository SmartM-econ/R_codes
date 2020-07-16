# JAGS is better than OpenBUGS!!
# Normal distr with creating new random parameter which is impossible in Openbugs
library(rjags)
library(coda)
modelString="
model
{
  mu ~ dnorm(10,1/25)  # prior, norm(mu,prec) is Jags manner 
  prec ~ dgamma(0.5,1) # prior, norm(mu,sig) is R gramma
  sigsq<-1/prec
  for(i in 1:10){
    
    x[i] ~ dnorm(mu,prec) # likelihood 
  }
}
"
writeLines(modelString,"NormalDistrJagsFirstTime.txt")

dataList=list(x = c(10,13,15,11,9,18,20,17,23,21) )

initsList=list(mu=10,prec=1/25)

JagsModel=jags.model(file="NormalDistrJagsFirstTime.txt",data=dataList,
                     inits=initsList, n.chains=3,n.adapt=500)
update(JagsModel,n.iter=1000)
codaSamples=coda.samples(JagsModel,variable.names=c("mu","sigsq"),
                         n.iter=5000)
coda::traceplot( codaSamples[,"mu"] , main="" , ylab="mu" )
acf(codaSamples[,"mu"][[1]],plot=T, main="")

coda::traceplot( codaSamples[,"sigsq"] , main="" , ylab="sigsq" )
acf(codaSamples[,"sigsq"][[1]],plot=T, main="")

MuSamples=as.matrix(codaSamples[,"mu"])
SigSamples=as.matrix(codaSamples[,"sigsq"])

par(mfrow=c(1,2))
plot(density(MuSamples), xlab=expression(mu), ylab="posterior density",main="")
plot(density(SigSamples), xlab=expression(sigma^2), ylab="posterior density",main="")

AcceptRate=1-rejectionRate(codaSamples)
AcceptRate
