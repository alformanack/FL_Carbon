setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
sitetree<-read.csv("sitetree12.17.19.csv", header=T, sep=",", stringsAsFactors = F)

attach(sitetree)

sitetree$SPCD<-as.factor(sitetree$SPCD)

# calculate soil C:N for ORNL

#sitetree<-mutate(sitetree, ORNL_CN=(SOC_ORNL_kgperm2*1000)/N_ORNL_gperm2)
sitetree$ORNL_CN<-sitetree$SOC_ORNL_kgperm2*1000/sitetree$N_ORNL_gperm2

# calculate weighted averages and soil C:N for Soilgrids data

#sitetree<-mutate(sitetree, SOILGRIDS_C_AVG=((Soilgrids_C_0.5/10)*(1/3))+((Soilgrids_C_5.15/10)*(2/3)))
sitetree$SOILGRIDS_C_AVG<-(sitetree$Soilgrids_C_0.5/10)*(1/3)+((sitetree$Soilgrids_C_5.15/10)*(2/3))
                 
#sitetree<-mutate(sitetree, SOILGRIDS_N_AVG=((Soilgrids_N_0.5/100)*(1/3))+((Soilgrids_N_5.15/100)*(2/3)))                 
sitetree$SOILGRIDS_N_AVG<-(sitetree$Soilgrids_N_0.5/100)*(1/3)+((sitetree$Soilgrids_N_5.15/100)*(2/3))                 

#sitetree<-mutate(sitetree, SOILGRIDS_CN=SOILGRIDS_C_AVG/SOILGRIDS_N_AVG)  
sitetree$SOILGRIDS_CN<-sitetree$SOILGRIDS_C_AVG/sitetree$SOILGRIDS_N_AVG 

sitetree$FIA_CN_RATIO<-as.numeric(sitetree$FIA_CN_RATIO)
sitetree[which(is.finite(sitetree$SOILGRIDS_CN)==F),"SOILGRIDS_CN"]<-NA
#sitetree[which(is.finite(sitetree$FIA_CN_RATIO)==F),"FIA_CN_RATIO"]<-NA
model<-lm(FIA_CN_RATIO~SOILGRIDS_CN, data=sitetree)
summary(model)

sitetree$SOILGRIDS_CN_SCALE<-(sitetree$SOILGRIDS_CN*6.045)+19.775

# comparing soils data with FIA

plot(sitetree$ORNL_CN, sitetree$FIA_CN_RATIO)                 

plot(sitetree$SOILGRIDS_CN, sitetree$FIA_CN_RATIO)

# checking variable normality


normali1 <- function (x)
{mnT <- mean(x,na.rm=T)
sdT <- sd(x,na.rm=T)
h<-hist(x, breaks=10, col="orangered1")
xfit<-seq(min(x,na.rm=T),max(x,na.rm=T), length=100) 
yfit<-dnorm(xfit,mean=mnT,sd=sdT) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="purple3", lwd=2)
qqnorm(x)
qqline(x)
shapiro.test(x)}

par(mfrow=c(1,2))

normali1(DIA)
sitetree$logDIA<-log10(sitetree$DIA)
normali1(sitetree$logDIA)




normali1(AVG_TEMP_bioclim)
sitetree$logAVG_TEMP<-log10(sitetree$AVG_TEMP_bioclim)
normali1(sitetree$logAVG_TEMP)
sitetree<-dplyr::mutate(sitetree, lnAVG_TEMP=log(AVG_TEMP_bioclim))
normali1(sitetree$lnAVG_TEMP)

normali1(sitetree$SOILGRIDS_CN_SCALE)
sitetree<-mutate(sitetree, logSOILGRIDS_CN_SCALE=log10(SOILGRIDS_CN_SCALE))
normali1(na.omit(sitetree$logSOILGRIDS_CN_SCALE))

normali1(AGEDIA)
sitetree<-mutate(sitetree, sqrtAGEDIA=sqrt(AGEDIA))
normali1(sitetree$sqrtAGEDIA)

normali1(SOIL_MOIS)
sitetree<-mutate(sitetree, logSOIL_MOIS=log10(SOIL_MOIS))
normali1(sitetree$logSOIL_MOIS)

# looking at trends

pairs(DIA~AVG_TEMP_bioclim+AGEDIA+SOILGRIDS_CN+SOIL_MOIS+SPCD)

pairs(DIA~AVG_TEMP_bioclim+AGEDIA+ORNL_CN+SOIL_MOIS+SPCD)

# building models

model1<-lm(logDIA~AVG_TEMP_bioclim+AGEDIA+SOILGRIDS_CN+SOIL_MOIS, data=sitetree)
summary(model1)
plot(model1)

model2<-lm(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+SOIL_MOIS, data=sitetree)
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