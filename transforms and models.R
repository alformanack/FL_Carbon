sitetree<-read.csv("sitetree12.17.19.csv", header=T, sep=",", stringsAsFactors = F)

attach(sitetree)

sitetree$SPCD<-as.factor(sitetree$SPCD)

# calculate soil C:N for ORNL

sitetree<-mutate(sitetree, ORNL_CN=(SOC_ORNL_kgperm2*1000)/N_ORNL_gperm2)

# calculate weighted averages and soil C:N for Soilgrids data

sitetree<-mutate(sitetree, SOILGRIDS_C_AVG=((Soilgrids_C_0.5/10)*(1/3))+((Soilgrids_C_5.15/10)*(2/3)))
                 
sitetree<-mutate(sitetree, SOILGRIDS_N_AVG=((Soilgrids_N_0.5/100)*(1/3))+((Soilgrids_N_5.15/100)*(2/3)))                 

sitetree<-mutate(sitetree, SOILGRIDS_CN=SOILGRIDS_C_AVG/SOILGRIDS_N_AVG)  

# comparing soils data with FIA

plot(ORNL_CN, FIA_CN_RATIO)                 

plot(SOILGRIDS_CN, FIA_CN_RATIO)

# checking variable normality

boxcox(model3)

normali1 <- function (x)
{mnT <- mean(x)
sdT <- sd(x)
h<-hist(x, breaks=10, col="orangered1")
xfit<-seq(min(x),max(x), length=100) 
yfit<-dnorm(xfit,mean=mnT,sd=sdT) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="purple3", lwd=2)
qqnorm(x)
qqline(x)
shapiro.test(x)}


normali1(DIA)
sitetree<-mutate(sitetree, logDIA=log10(DIA))
normali1(sitetree$logDIA)
sitetree<-mutate(sitetree, lnDIA=log(DIA))
normali1(sitetree$lnDIA)

normali1(AVG_TEMP_bioclim)
sitetree<-mutate(sitetree, logAVG_TEMP=log10(AVG_TEMP_bioclim))
normali1(sitetree$logAVG_TEMP)
sitetree<-mutate(sitetree, lnAVG_TEMP=log(AVG_TEMP_bioclim))
normali1(sitetree$lnAVG_TEMP)

normali1(SOILGRIDS_CN)
sitetree<-mutate(sitetree, logSOILGRIDS_CN=log10(SOILGRIDS_CN))
normali1(na.omit(sitetree$logSOILGRIDS_CN))

normali1(AGEDIA)
sitetree<-mutate(sitetree, logAGEDIA=log10(AGEDIA))
normali1(sitetree$logAGEDIA)

normali1(SOIL_MOIS)
sitetree<-mutate(sitetree, logSOIL_MOIS=log10(SOIL_MOIS))
normali1(sitetree$logSOIL_MOIS)

# looking at trends

pairs(DIA~AVG_TEMP_bioclim+AGEDIA+SOILGRIDS_CN+SOIL_MOIS+SPCD)

pairs(DIA~AVG_TEMP_bioclim+AGEDIA+ORNL_CN+SOIL_MOIS+SPCD)

# building models

model1<-lm(logDIA~AVG_TEMP_bioclim+AGEDIA+SOILGRIDS_CN+SOIL_MOIS+SPCD)
summary(model1)
plot(model1)

model2<-lm(na.omit(logDIA~AVG_TEMP_bioclim+AGEDIA+logSOILGRIDS_CN+SOIL_MOIS+SPCD))
summary(model2)
plot(model2)

model3<-lm(logDIA~logAVG_TEMP+AGEDIA+logSOILGRIDS_CN+SOIL_MOIS+SPCD)
summary(model3)

model4<-lm(logDIA~logAVG_TEMP+logAGEDIA+logSOILGRIDS_CN+SOIL_MOIS+SPCD)
summary(model4)
plot(model4)

model5<-lm(logDIA~logAVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN+SOIL_MOIS+SPCD)

AICctab(model1, model2, model3, model4, base=T, delta=T, weights=T)

# subsetting species

shortleaf<-subset(sitetree, sitetree$SPCD=="110")
slashpine<-subset(sitetree, sitetree$SPCD=="111")
longleaf<-subset(sitetree, sitetree$SPCD=="121")
pondpine<-subset(sitetree, sitetree$SPCD=="128")
loblolly<-subset(sitetree, sitetree$SPCD=="131")

model6<-lm(na.omit(slashpine$logDIA~slashpine$logAVG_TEMP+slashpine$logAGEDIA+slashpine$logSOILGRIDS_CN+slashpine$SOIL_MOIS))
summary(model6)
plot(model4)