sitetree<-read.csv("sitetree11.26.19.csv", header=T, sep=",", stringsAsFactors = F)

model1<-lm(sitetree$DIA~sitetree$TEMP+sitetree$CN_RATIO+sitetree$AGEDIA+sitetree$SPCD+sitetree$SOIL_MOIS)
summary(model1)
plot(model1)


model2<-lm(sitetree$DIA~sitetree$TEMP+sitetree$CN_RATIO+sitetree$AGEDIA)
summary(model2)
plot(model2)

plot(sitetree$CN_RATIO~sitetree$TEMP)

model3<-lm(sitetree$DIA~sitetree$TEMP+sitetree$CN_RATIO+sitetree$AGEDIA+sitetree$SPCD+sitetree$SOIL_MOIS)
summary(model3)
plot(model3)

logmodel1<-lm(sitetree$logDIA~sitetree$TEMP+sitetree$CN_RATIO+sitetree$AGEDIA+sitetree$SPCD+sitetree$SOIL_MOIS)
plot(logmodel1)
summary(logmodel1)

sqrtmodel1<-lm(sitetree$sqrtDIA~sitetree$TEMP+sitetree$CN_RATIO+sitetree$AGEDIA+sitetree$SPCD+sitetree$SOIL_MOIS)
plot(sqrtmodel1)
summary(sqrtmodel1)

sitetree$CN_RATIO<-as.numeric(sitetree$CN_RATIO)
sitetree$SPCD<-as.factor(sitetree$SPCD)

normali1 <- function (x)
{mnT <- mean(x)
sdT <- sd(x)
h<-hist(x,breaks=10 , col="orangered1")
xfit<-seq(min(x),max(x)) 
yfit<-dnorm(xfit,mean=mnT,sd=sdT) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="purple3", lwd=2)
qqnorm(x)
qqline(x)
shapiro.test(x)}

normali1(sitetree$DIA[sitetree$SPCD=="110"])
normali1(sitetree$DIA[sitetree$SPCD=="111"])
normali1(sitetree$DIA[sitetree$SPCD=="121"])
normali1(sitetree$DIA[sitetree$SPCD=="128"])


normali1(sitetree$logDIA[sitetree$SPCD=="110"])
normali1(sitetree$logDIA[sitetree$SPCD=="111"])
normali1(sitetree$logDIA[sitetree$SPCD=="121"])
normali1(sitetree$logDIA[sitetree$SPCD=="128"])

sitetree<-mutate(sitetree, logDIA=log10(sitetree$DIA))
sitetree<-mutate(sitetree, sqrtDIA=sqrt(sitetree$DIA))
