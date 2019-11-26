sitetree<-read.csv("sitetree11.26.19.csv", header=T, sep=",", stringsAsFactors = F)

model1<-glm(sitetree$DIA~sitetree$TEMP+sitetree$CN_RATIO+sitetree$AGEDIA+sitetree$SPCD+sitetree$SOIL_MOIS)
summary(model1)
plot(model1)

model2<-lm(sitetree$DIA~sitetree$TEMP+sitetree$CN_RATIO+sitetree$AGEDIA)
summary(model2)
plot(model2)

plot(sitetree$CN_RATIO~sitetree$TEMP)

model3<-glm(sitetree$DIA~sitetree$TEMP+sitetree$CN_RATIO+sitetree$AGEDIA+sitetree$SPCD+sitetree$SOIL_MOIS)
summary(model3)
plot(model3)

sitetree$CN_RATIO<-as.numeric(sitetree$CN_RATIO)
sitetree$SPCD<-as.factor(sitetree$SPCD)
