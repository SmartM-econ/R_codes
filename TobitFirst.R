r = system.file(package="psidR")
cwf = openxlsx::read.xlsx(file.path(r,"psid-lists","psid.xlsx"))
getNamesPSID("ER17013", cwf, years = 2001)
getNamesPSID("ER17013", cwf, years = 2003)
getNamesPSID("ER17013", cwf, years = NULL)
getNamesPSID("ER17013", cwf, years = c(2005, 2007, 2009))

z0<-z1<-z2<-Tobacco
z0$nkids2<-0
z1$nkids2<-1
z2$nkids2<-2

model<-stobacco~nkids+nkids2+nadults+lnx+age
ols<-lm(model,data=Tobacco)
summary(ols)

trunc<-lm(model,data=Tobacco,subset=stobacco>0)
summary(trunc)

tobinfit<-tobit(model,data=Tobacco,left=0)
summary(tobinfit)

Tobacco$xb <- predict(tobinfit,type='lp')
Tobacco$xb1 <- Tobacco$xb / tobinfit$scale
Tobacco$imr <- with(Tobacco, dnorm(xb1)/pnorm(xb1))
b1 <- tobinfit$coef['lnx']
b1*mean(1-with(Tobacco,imr*(xb1+imr)))

library(sampleSelection)
data("Mroz87")
seleq<-lfp~nwifeinc+educ+exper+I(exper^2)+age+kids5+kids618
outcomeeq<-log(wage) ~educ+exper+I(exper^2)
twostep<-heckit(seleq,outcomeeq,data=Mroz87) #option : method="ml"(maximumlikely..)
summary(twostep)