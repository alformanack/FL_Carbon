sitetree<-read.csv("soilCNcomparison.csv", header=T, sep=",", stringsAsFactors = F)

sitetree$CN_RATIO_ORNL<-as.numeric(sitetree$CN_RATIO_ORNL)
sitetree$SPCD<-as.factor(sitetree$SPCD)

attach(sitetree)

pairs(DIA~AVG_TEMP_bioclim+CN_RATIO_ORNL+AGEDIA+HT+SPCD+SOIL_MOIS)


model1<-lm(sitetree$DIA~sitetree$AVG_TEMP_bioclim+sitetree$CN_RATIO_ORNL+sitetree$AGEDIA+sitetree$SPCD+sitetree$SOIL_MOIS)
summary(model1)
plot(model1)

sitetree<-mutate(sitetree, logDIA=log10(sitetree$DIA))

logmodel1<-lm(sitetree$logDIA~sitetree$AVG_TEMP_bioclim+sitetree$CN_RATIO_ORNL+sitetree$AGEDIA+sitetree$SPCD+sitetree$SOIL_MOIS)
plot(logmodel1)
summary(logmodel1)

model2<-lm(sitetree$logDIA~sitetree$AVG_TEMP_bioclim+sitetree$CN_RATIO_ORNL+sitetree$AGEDIA+sitetree$SPCD+sitetree$HT)


model3<-lm(sitetree$logDIA~sitetree$AVG_TEMP_bioclim+sitetree$CN_RATIO_ORNL+sitetree$AGEDIA+sitetree$SPCD+sitetree$SOIL_MOIS+sitetree$HT)
summary(model3)
plot(model3)

model4<-lm(sitetree$logDIA~sitetree$AVG_TEMP_bioclim+sitetree$CN_RATIO_ORNL+sitetree$SPCD+sitetree$SOIL_MOIS+sitetree$AGEDIA*sitetree$HT)
summary(model4)

model5<-lm(sitetree$logDIA~sitetree$AVG_TEMP_bioclim+sitetree$AGEDIA+sitetree$SPCD+sitetree$CN_RATIO_ORNL*sitetree$SOIL_MOIS+sitetree$HT)

model6<-lm(sitetree$logDIA~sitetree$AGEDIA+sitetree$SPCD+sitetree$CN_RATIO_ORNL+sitetree$AVG_TEMP_bioclim*sitetree$SOIL_MOIS+sitetree$HT)

model7<-lm(sitetree$logDIA~sitetree$AGEDIA+sitetree$SPCD+sitetree$CN_RATIO_ORNL*sitetree$AVG_TEMP_bioclim+sitetree$AVG_TEMP_bioclim*sitetree$SOIL_MOIS+sitetree$HT)

model8<-lm(sitetree$logDIA~sitetree$AVG_TEMP_bioclim+sitetree$CN_RATIO_ORNL+sitetree$SPCD+sitetree$AGEDIA*sitetree$HT)
summary(model8)

AICctab(logmodel1, model2, model3, model4, model5, model6, model7, model8,  base=T, delta=T, weights=T)

scalemodel8<-lm(sitetree$logDIA~scale(sitetree$AVG_TEMP_bioclim)+scale(sitetree$CN_RATIO_ORNL)+sitetree$SPCD+scale(sitetree$AGEDIA)*scale(sitetree$HT))
summary(scalemodel8)

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

shortleaf<-subset(sitetree, sitetree$SPCD=="110")
slashpine<-subset(sitetree, sitetree$SPCD=="111")
longleaf<-subset(sitetree, sitetree$SPCD=="121")
pondpine<-subset(sitetree, sitetree$SPCD=="128")
loblolly<-subset(sitetree, sitetree$SPCD=="131")

slashmodel<-lm(slashpine$logDIA~slashpine$AVG_TEMP_bioclim+slashpine$CN_RATIO_ORNL+slashpine$AGEDIA*slashpine$HT)
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

