sitetree<-read.csv("sitetree11.26.19.csv", header=T, sep=",", stringsAsFactors = F)

interaction.plot(sitetree$TEMP,sitetree$SOIL_MOIS,sitetree$DIA,type="b")


model1<-lm(sitetree$DIA~sitetree$TEMP+sitetree$CN_RATIO+sitetree$AGEDIA+sitetree$SPCD+sitetree$SOIL_MOIS)
summary(model1)
plot(model1)


model2<-lm(sitetree$DIA~sitetree$TEMP+sitetree$CN_RATIO+sitetree$AGEDIA+sitetree$SOIL_MOIS)
summary(model2)
plot(model2)

plot(sitetree$CN_RATIO~sitetree$TEMP)

model3<-lm(sitetree$DIA~sitetree$TEMP+sitetree$CN_RATIO+sitetree$AGEDIA+sitetree$SPCD+sitetree$SOIL_MOIS+sitetree$HT)
summary(model3)
plot(model3)

model4<-lm(sitetree$DIA~sitetree$TEMP+sitetree$CN_RATIO+sitetree$SPCD+sitetree$SOIL_MOIS+sitetree$AGEDIA*sitetree$HT)
summary(model4)

model5<-lm(sitetree$DIA~sitetree$TEMP+sitetree$AGEDIA+sitetree$SPCD+sitetree$CN_RATIO*sitetree$SOIL_MOIS+sitetree$HT)

model6<-lm(sitetree$DIA~sitetree$AGEDIA+sitetree$SPCD+sitetree$CN_RATIO+sitetree$TEMP*sitetree$SOIL_MOIS+sitetree$HT)

model7<-lm(sitetree$DIA~sitetree$AGEDIA+sitetree$SPCD+sitetree$CN_RATIO*sitetree$TEMP*sitetree$SOIL_MOIS+sitetree$HT)

AICctab(model1, model2, model3, model4, model5, model6, model7,  base=T, delta=T, weights=T)

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

plot(sitetree$CN_RATIO~sitetree$TEMP)
plot(sitetree$DIA~sitetree$SPCD)

pairs(sitetree$DIA~sitetree$TEMP+sitetree$CN_RATIO+sitetree$AGEDIA+sitetree$SPCD+sitetree$SOIL_MOIS)

shortleaf<-subset(sitetree, sitetree$SPCD=="110")
slashpine<-subset(sitetree, sitetree$SPCD=="111")
longleaf<-subset(sitetree, sitetree$SPCD=="121")
pondpine<-subset(sitetree, sitetree$SPCD=="128")
loblolly<-subset(sitetree, sitetree$SPCD=="131")

slashmodel<-lm(slashpine$DIA~slashpine$TEMP+slashpine$CN_RATIO+slashpine$AGEDIA+slashpine$SOIL_MOIS)
summary(slashmodel)
plot(slashmodel)

slashpine<-mutate(slashpine, logDIA=log10(slashpine$DIA))

slashmodel2<-lm(slashpine$logDIA~slashpine$TEMP+slashpine$CN_RATIO+slashpine$AGEDIA+slashpine$SOIL_MOIS)
summary(slashmodel2)
plot(slashmodel2)

vif(slashmodel)
vif(model1)

slashmodel3<-lm(slashpine$logDIA~slashpine$HT+slashpine$TEMP+slashpine$CN_RATIO+slashpine$AGEDIA+slashpine$SOIL_MOIS)
summary(slashmodel3)
plot(slashmodel3)

lm(slashpine$logDIA~slashpine$HT+slashpine$CN_RATIO+slashpine$AGEDIA+slashpine$TEMP*slashpine$SOIL_MOIS)

