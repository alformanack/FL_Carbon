rm(list=ls())
setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
sitetree<-read.csv("sitetree12.17.19.csv", header=T, sep=",", stringsAsFactors = F)
sitetree<-read.csv("precip.csv", header=T, sep=",", stringsAsFactors = F)
sitetree<-read.csv("aridity3.csv", header=T, sep=",", stringsAsFactors = F)



sitetree$SPCD<-as.factor(sitetree$SPCD)


# Calculate soil C:N for ORNL ---------------------------------------------

sitetree$ORNL_CN<-sitetree$SOC_ORNL_kgperm2*1000/sitetree$N_ORNL_gperm2

# Calculate weighted averages and soil C:N for Soilgrids data -------------

is.na(sitetree$SOC_0_5_1) <- !sitetree$SOC_0_5_1
is.na(sitetree$SOC_5_15_1) <- !sitetree$SOC_5_15_1
is.na(sitetree$Nitrogen_0_5_1) <- !sitetree$Nitrogen_0_5_1
is.na(sitetree$Nitrogen_5_15_1) <- !sitetree$Nitrogen_5_15_1

# sitetree <- sitetree[which(!is.na(sitetree$SOC_0_5_1) &
#                  !is.na(sitetree$SOC_5_15_1) &
#                  !is.na(sitetree$Nitrogen_0_5_1) &
#                  !is.na(sitetree$Nitrogen_5_15_1)),]

sitetree$SOILGRIDS_C_AVG<-((sitetree$SOC_0_5_1/10)*(1/3))+((sitetree$SOC_5_15_1/10)*(2/3))
                 
sitetree$SOILGRIDS_N_AVG<-((sitetree$Nitrogen_0_5_1/100)*(1/3))+((sitetree$Nitrogen_5_15_1/100)*(2/3))                 

sitetree$SOILGRIDS_CN<-sitetree$SOILGRIDS_C_AVG/sitetree$SOILGRIDS_N_AVG 


sitetree[which(is.finite(sitetree$SOILGRIDS_CN)==F),"SOILGRIDS_CN"]<-NA 


sitetree$FIA_CN_RATIO<-as.numeric(sitetree$FIA_CN_RATIO)
sitetree[which(is.finite(sitetree$FIA_CN_RATIO)==F),"FIA_CN_RATIO"]<-NA


# Comparing soils data with FIA -------------------------------------------

plot(sitetree$ORNL_CN, sitetree$FIA_CN_RATIO)                 

plot(sitetree$SOILGRIDS_CN, sitetree$FIA_CN_RATIO)

model<-lm(FIA_CN_RATIO~SOILGRIDS_CN, data=sitetree)
summary(model)

#scale CN from FIA data
sitetree$SOILGRIDS_CN_SCALE<-(sitetree$SOILGRIDS_CN*4.941)+29.777
#sitetree$SOILGRIDS_CN_SCALE<-(sitetree$SOILGRIDS_CN*6.045)+19.775

plot(sitetree$SOILGRIDS_CN_SCALE, sitetree$FIA_CN_RATIO)
model<-lm(FIA_CN_RATIO~SOILGRIDS_CN_SCALE, data=sitetree)
summary(model)

# Checking variable normality ---------------------------------------------

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

par(mfrow=c(1,1))

normali1(sitetree$DIA)
sitetree$logDIA<-log10(sitetree$DIA)
normali1(sitetree$logDIA)
hist(sitetree$AVG_TEMP_bioclim)
pairs(sitetree$DIA~sitetree$AVG_TEMP_bioclim)
normali1(sitetree$AVG_TEMP_bioclim)
sitetree$logAVG_TEMP<-log10(sitetree$AVG_TEMP_bioclim)
normali1(sitetree$logAVG_TEMP)
#sitetree<-dplyr::mutate(sitetree, lnAVG_TEMP=log(AVG_TEMP_bioclim))
#normali1(sitetree$lnAVG_TEMP)

normali1(sitetree$SOILGRIDS_CN_SCALE)
sitetree$logSOILGRIDS_CN_SCALE<-log10(sitetree$SOILGRIDS_CN_SCALE)
normali1(sitetree$logSOILGRIDS_CN_SCALE)

normali1(sitetree$AGEDIA)
sitetree$logAGEDIA<-log10(sitetree$AGEDIA)
normali1(sitetree$logAGEDIA)

#normali1(SOIL_MOIS)
#sitetree<-mutate(sitetree, logSOIL_MOIS=log10(SOIL_MOIS))
#normali1(sitetree$logSOIL_MOIS)

# looking at trends

#pairs(DIA~AVG_TEMP_bioclim+AGEDIA+SOILGRIDS_CN+SOIL_MOIS+SPCD)

#plot(DIA~AVG_TEMP_bioclim)
plot(sitetree$logDIA~sitetree$AVG_TEMP_bioclim) #USED
#plot(log10(DIA)~log10(AVG_TEMP_bioclim))

#plot(DIA~AGEDIA)
#plot(log10(DIA)~AGEDIA)
plot(sitetree$logDIA~sitetree$logAGEDIA) #USED

#plot(DIA~SOILGRIDS_CN_SCALE, data=sitetree)
#plot(log10(DIA)~SOILGRIDS_CN_SCALE, data=sitetree)
plot(log10(DIA)~log10(SOILGRIDS_CN_SCALE), data=sitetree) #USED

plot(logDIA~sitetree$precip_1, data=sitetree)
normali1(sitetree$precip_1)
sitetree$logprecip<-log10(sitetree$precip)
normali1(sitetree$logprecip)
#plot(log10(DIA)~SOIL_MOIS)
sitetree<-sitetree[-c(1336), ]

sitetree$aridity_1<-sitetree$aridity_1*.0001
summary(sitetree$aridity_1)
hist(sitetree$aridity_1)

plot(logDIA~sitetree$aridity_1, data=sitetree)
normali1(sitetree$aridity_1)
#sitetree$logaridity<-log10(sitetree$aridity_1)
normali1(sitetree$logaridity)
plot(logDIA~sitetree$logaridity, data=sitetree)
# Subsetting species ------------------------------------------------------

slashpine<-subset(sitetree, sitetree$SPCD=="111")
longleaf<-subset(sitetree, sitetree$SPCD=="121")
loblolly<-subset(sitetree, sitetree$SPCD=="131")

slashpine$temp2<-slashpine$AVG_TEMP_bioclim^2

SlashSMA <- smatr::sma(data=slashpine, logDIA ~ logAGEDIA, method = "SMA")
slashresidualssma<-residuals(SlashSMA)

other2.slash<-lm(data=slashpine, slashresidualssma ~ aridity_1*SOILGRIDS_CN_SCALE+AVG_TEMP_bioclim)
summary(other2.slash)

write.csv(slashpine, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/slashpine_data.csv")
write.csv(longleaf, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/longleaf_data.csv")

s1<-lm(data=slashpine, logDIA ~ AVG_TEMP_bioclim + temp2 + logAGEDIA + aridity_1*SOILGRIDS_CN_SCALE)
#confint(s1, level=0.95)
s2<-lm(data=slashpine, logDIA ~ logAGEDIA + AVG_TEMP_bioclim + aridity_1*SOILGRIDS_CN_SCALE)
#confint(s2, level=0.95)
s3<-lm(data=slashpine, logDIA ~ AVG_TEMP_bioclim + temp2 + logAGEDIA + aridity_1 + SOILGRIDS_CN_SCALE)
s7<-lm(data=slashpine, logDIA ~ AVG_TEMP_bioclim + temp2 + logAGEDIA + aridity_1)
 
s4<-lm(data=slashpine, logDIA ~ AVG_TEMP_bioclim + logAGEDIA + aridity_1*SOILGRIDS_CN_SCALE)
s5<-lm(data=slashpine, logDIA ~ AVG_TEMP_bioclim*aridity_1 +logAGEDIA +SOILGRIDS_CN_SCALE)

#WINNER WINNER CHICKEN DINNER
s6<-lm(data=slashpine, logDIA ~ SOILGRIDS_CN_SCALE*aridity_1 +logAGEDIA)

#alometric equation to predict height frfom dbh

height<-lm(data = slashpine, log10(HT)~log10(DIA))
summary(height)

heightSMA <- smatr::sma(data=slashpine, log10(HT) ~ log10(DIA), method = "SMA")

height2<-lm(data = slashpine, log10(HT)~log10(DIA) + AVG_TEMP_bioclim + temp2 + logAGEDIA + aridity_1)
summary(height2)


bbmle::AICctab(s1, s2, s5, s3, s4, s6, base=T, delta=T, weights=T)
bbmle::BICtab(s1, s2, s5, s3, s4, s6, base=T, delta=T, weights=T)

# glm1<-glm(data=slashpine, family = poisson, slashresiduals.sma ~ AVG_TEMP_bioclim + temp2 + aridity_1)
# glm2<-glm(data=slashpine, family = Gamma, slashresiduals.sma ~ AVG_TEMP_bioclim + temp2 + aridity_1)
# glm3<-glm.nb(data=slashpine, slashresiduals.sma ~ AVG_TEMP_bioclim + temp2 + aridity_1)

# AICctab(s1, glm1, glm2, glm3, base=T, delta=T, weights=T)

car::vif(s2)


ggplot(data=slashpine, aes(other.slash$residuals)) +
  geom_histogram(color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

ggplot(data = slashpine, aes(x = AGEDIA, y = DIA)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

AICctab(s1, s2, s3, base=T, delta=T, weights=T)

# # sdlogDIA <- sd(slashpine$logDIA)
# sdslashresid <- sd(slashresidualssma)
# # 
# model2 <- map2stan(
#   alist(
#     slashresidualssma ~ dnorm(mu,sigma),
#     mu <- a + b*AVG_TEMP_bioclim +c*aridity_1 + d*SOILGRIDS_CN_SCALE + e*SOILGRIDS_CN_SCALE*aridity_1,
#     a ~ dnorm(0,10),
#     b ~ dnorm(0,10),
#     c ~ dnorm(0,10),
#     d ~ dnorm(0,10),
#     e ~ dnorm(0,10),
#     sigma ~ dunif(0,50)
#   ),
#   data = slashpine,chains =3,
#   start <- list(a = 0, b= 0, c=0, sigma=sdslashresid)
# )
# 
# precis(model2,digits=3)
# plot(model2)
# pairs(model2)

# slashpine$logAGEDIA.s <- (slashpine$logAGEDIA - mean(slashpine$logAGEDIA))/sd(slashpine$logAGEDIA)
# slashpine$AVG_TEMP_bioclim.s <- (slashpine$AVG_TEMP_bioclim - mean(slashpine$AVG_TEMP_bioclim))/sd(slashpine$AVG_TEMP_bioclim)
# slashpine$temp2.s <- (slashpine$temp2 - mean(slashpine$temp2))/sd(slashpine$temp2)
# slashpine$aridity_1.s <- (slashpine$aridity_1 - mean(slashpine$aridity_1))/sd(slashpine$aridity_1)
# 
# model2 <- map2stan(
#   alist(
#     logDIA ~ dnorm(mu,sigma),
#     mu <- a + b*logAGEDIA.s + c*AVG_TEMP_bioclim.s + d*temp2.s + e*aridity_1.s,
#     a ~ dnorm(0.9986263,0.117853),
#     b ~ dnorm(0,10),
#     c ~ dnorm(0,10),
#     d ~ dnorm(0,10),
#     e ~ dnorm(0,10),
#     sigma ~ dunif(0,50)
#   ),
#   data = slashpine,chains =3,
#   start <- list(a = 0, b= 0, c=0, d=0, e=0, sigma=sdlogDIA)
# )
# 
# precis(model2,digits=3)
# plot(model2)
# pairs(model2)
# 
# model3 <- map2stan(
#   alist(
#     DIA ~ dnorm(mu,sigma),
#     mu <- 10^(a + c*AVG_TEMP_bioclim + d*temp2 + e*aridity_1) * AGEDIA^b,
#     a ~ dnorm(0,10),
#     b ~ dnorm(0,10),
#     c ~ dnorm(0,10),
#     d ~ dnorm(0,10),
#     e ~ dnorm(0,10),
#     sigma ~ dunif(0,100)
#   ),
#   data = slashpine,chains =3,
#   start <- list(a = 0, b= 0, c=0, d=0, e=0, sigma=sdDIA)
# )
# 
# precis(model3,digits=3)
# plot(model3)
# pairs(model3)
# 
# par(mfrow=c (1,1))
# plot(dt$height,dt$rep_structures, main="",xlab="height (cm)",
#      ylab="Number of fruits",pch=16,cex=0.55, xlim=c(0,80), ylim= c(0,1000) ) 
# lnhgt <- seq(0, log(max(dt$ht_init)), 0.01)
# mu <- link(model2,data=data.frame(lnht=lnhgt))
# mu.mean <- apply(mu,2,mean)
# mu.PI <- apply(mu,2,PI,prob=0.95)
# lines(exp(lnhgt),exp(mu.mean),col="red")
# shade(exp(mu.PI),exp(lnhgt))

slashpin<-slashpine[na.exclude(slashpine$SOILGRIDS_CN_SCALE),]

SlashSMA <- smatr::sma(data=slashpine, logDIA ~ logAGEDIA, method = "SMA")
slashresidualssma<-residuals(SlashSMA)
plot(SlashSMA)

plot(slashresidualssma~slashpine$AVG_TEMP_bioclim)
plot(slashresidualssma~slashpine$temp2)
plot(slashresidualssma~slashpine$aridity_1)
plot(slashresidualssma~slashpine$SOILGRIDS_CN_SCALE)
plot(slashresidualssma~slashpine$logSOILGRIDS_CN_SCALE)
#SlashMA <- ma(data=slashpine, logDIA ~ logAGEDIA)
#plot(SlashMA)
# plot(slashresiduals.sma)

car::vif(new.slash)

new.slash<-lm(data=slashpine, slashresidualssma ~ aridity_1+AVG_TEMP_bioclim)
other.slash<-lm(data=slashpine, slashresidualssma ~ aridity_1+AVG_TEMP_bioclim+temp2)
other2.slash<-lm(data=slashpine, slashresidualssma ~ aridity_1*AVG_TEMP_bioclim)
bbmle::BICtab( new.slash, other2.slash, other.slash,  base=T, delta=T, weights=T)
bbmle::AICctab( new.slash, other2.slash, other.slash,  base=T, delta=T, weights=T)



# summary(other2.slash)
# old.slash<-lm(data=slashpine, slashresidualssma ~ aridity_1+AVG_TEMP_bioclim+temp2)
# old.slash2<-lm(data=slashpine, slashresidualssma ~ aridity_1+AVG_TEMP_bioclim*AVG_TEMP_bioclim+SOILGRIDS_CN_SCALE)
# 
# new.slash<-lm(data=slashpine, slashresidualssma ~ aridity_1*SOILGRIDS_CN_SCALE)
# other.slash<-lm(data=slashpine, slashresidualssma ~ aridity_1*SOILGRIDS_CN_SCALE+AVG_TEMP_bioclim+temp2)
# other2.slash<-lm(data=slashpine, slashresidualssma ~ aridity_1*SOILGRIDS_CN_SCALE+AVG_TEMP_bioclim)
# old.slash<-lm(data=slashpine, slashresidualssma ~ aridity_1+AVG_TEMP_bioclim+temp2)
# 
# 
# bbmle::AICctab( new.slash, other2.slash, other.slash, old.slash2,  base=T, delta=T, weights=T)
# bbmle::BICtab( new.slash, other2.slash, other.slash,  base=T, delta=T, weights=T)
# 
# bbmle::AICctab(   base=T, delta=T, weights=T)
# bbmle::BICtab(  base=T, delta=T, weights=T)
# 
# slashgrowth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
# age<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[1,3]^(1.728844)))
# 
# 
# 
# 
# errors <- otherslashresid[!is.na(otherslashresid)]
# otherslashresid<-residuals(other.slash)
# 
# ggplot(data=na.exclude(slashpine), aes(other.slash$residuals)) +
#   geom_histogram(color = "black", fill = "purple4") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.line.x=element_line(),
#         axis.line.y=element_line()) +
#   ggtitle("Histogram for Model Residuals")
# 
# slashgrowth<-(10^(0.7217104 -0.007283*CN + 0.007855*CN*aridity -0.638287*aridity))*(0.5784213*age[i,j]^(-0.4215787))
# age<-round((10^(-1.247724 +0.01259117*CN -0.01358007*CN*aridity +1.103498*aridity))*(envdata[1,3]^(1.728844)))
# 
# 
# par(mfrow=c (2,2))
# plot(new.slash)
# #this is for MA (didn't use)(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*age[i,j]^(-0.5667225))
# 
# normali1(slashpine$aridity_1)
# normali1(slashpine$AVG_TEMP_bioclim) #not normal
# normali1(slashpine$logAGEDIA)
# normali1(slashpine$temp2) #not normal
# 
# # slashpinemodel<-lm(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+slashpine$precip_1, data=slashpine)
# # slashpinemodel2<-lm(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+slashpine$SOIL_MOIS, data=slashpine)
# slashpinemodel2<-lm(logDIA~logAGEDIA+logSOILGRIDS_CN_SCALE+slashpine$aridity_1, data=slashpine)
# summary(slashpinemodel2)
# slashpinemodel3<-lm(na.omit(data=slashpine, logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+slashpine$aridity_1))
# summary(slashpinemodel3)
# slashpinemodel4<-lm(logDIA~AVG_TEMP_bioclim+aridity_1+logAGEDIA, data=slashpine)
# summary(slashpinemodel4)
# res.slash<-resid(slashpinemodel4)
# plot(res.slash~slashpine$aridity_1)
# plot(res.slash~slashpine$AVG_TEMP_bioclim)
# plot(res.slash~slashpine$logAGEDIA)
# plot(res.slash~slashpine$logSOILGRIDS_CN_SCALE)
# # summary(slashpinemodel2)
# # summary(slashpinemodel)
# 
# pairs(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+slashpine$aridity_1+temp2, data=slashpine)
# car::vif(slashpinemodel3)


# Longleaf Data -----------------------------------------------------------

normali1(longleaf$aridity_1) #not normal
normali1(longleaf$AVG_TEMP_bioclim)
normali1(longleaf$logSOILGRIDS_CN_SCALE)
normali1(longleaf$SOILGRIDS_CN_SCALE)
normali1(longleaf$logAGEDIA)


pairs(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+aridity_1, data=longleaf)

car::vif(longleafmodel1) #temperature is too correlated with aridity

longleaf$aridity2<-longleaf$aridity_1^2

longleafmodel1<-lm(resid.LLSMA~SOILGRIDS_CN_SCALE+AVG_TEMP_bioclim+aridity_1, data=longleaf)
longleafmodel2<-lm(resid.LLSMA~SOILGRIDS_CN_SCALE+AVG_TEMP_bioclim, data=longleaf)

#WINNER WINNER CHICKEN DINNER
longleafmodel3<-lm(resid.LLSMA~aridity_1*SOILGRIDS_CN_SCALE, data=longleaf)
longleafmodel5<-lm(resid.LLSMA~aridity_1, data=longleaf)
longleafmodel6<-lm(resid.LLSMA~aridity_1+SOILGRIDS_CN_SCALE, data=longleaf)

bbmle::BICtab(longleafmodel1, longleafmodel2, longleafmodel3, base=T, delta=T, weights=T)
bbmle::AICctab(longleafmodel1, longleafmodel2, longleafmodel3, base=T, delta=T, weights=T)

# longleafmodel4<-lm(logDIA~logAGEDIA+logSOILGRIDS_CN_SCALE*aridity_1, data=longleaf) 
# longleafmodel5<-lm(logDIA~logAGEDIA+SOILGRIDS_CN_SCALE+aridity_1+AVG_TEMP_bioclim, data=longleaf)
# plot(longleafmodel4)
# 
# summary(longleafmodel4)
# resid.L3<-resid(longleafmodel3)
# plot(data = longleaf, resid.L3~ logSOILGRIDS_CN_SCALE)
# 
# ggplot(data=na.exclude(longleaf), aes(newLongleaf$residuals)) +
#   geom_histogram(color = "black", fill = "purple4") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.line.x=element_line(),
#         axis.line.y=element_line()) +
#   ggtitle("Histogram for Model Residuals")
# 
# bbmle::AICctab(longleafmodel1, longleafmodel2, longleafmodel3, longleafmodel4, longleafmodel5, base=T, delta=T, weights=T)
# bbmle::BICtab(longleafmodel1, longleafmodel2, longleafmodel3, longleafmodel4, longleafmodel5, base=T, delta=T, weights=T)
# 
# ggplot2::interact_plot(longleafmodel3, pred = logSOILGRIDS_CN_SCALE, modx = AVG_TEMP_bioclim)

res.longleaf<-resid(longleafmodel1)
plot(data=longleaf, logDIA~longleaf$aridity_1)
plot(data=longleaf, logDIA~longleaf$logSOILGRIDS_CN_SCALE)
plot(data=longleaf, logDIA~longleaf$logAGEDIA)

plot(res.longleaf~longleaf$logAGEDIA)
plot(res.slash~slashpine$logSOILGRIDS_CN_SCALE)

par(mfrow=c (2,2))

plot(longleafmodel1)

par(mfrow=c (1,1))
plot(LongleafSMA)
LongleafSMA <- smatr::sma(data=longleaf, logDIA ~ logAGEDIA, method = "SMA")
resid.LLSMA<- residuals(LongleafSMA)
newLongleaf<- lm(resid.LLSMA~SOILGRIDS_CN_SCALE*aridity_1, data=longleaf)
plot(newLongleaf)
plot(resid.LLSMA~longleaf$SOILGRIDS_CN_SCALE)
plot(resid.LLSMA~longleaf$aridity_1)
plot(resid.LLSMA~longleaf$aridity_1)
plot(resid.LLSMA~longleaf$AVG_TEMP_bioclim)

longleafgrowth<-(10^(0.9333281 - 0.009259*CN + 0.011340*CN*aridity - 0.977477*aridity))*(0.5718692*age[i,j]^(-0.4281308))

#longleafmodel2<-lm(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+SOIL_MOIS, data=longleaf)
#longleafmodel<-lm(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+longleaf$precip_1, data=longleaf)
summary(longleafmodel3)
#summary(longleafmodel2)
#summary(longleafmodel)


# Loblolly ----------------------------------------------------------------



#loblolly$temp2<-loblolly$AVG_TEMP_bioclim^2
loblolly$aridity2<-loblolly$aridity_1^2

par(mfrow=c (2,2))
LoblollySMA <- smatr::sma(data=loblolly, logDIA ~ logAGEDIA, method = "SMA")
# LoblollyMA <- smatr::sma(data=loblolly, logDIA ~ logAGEDIA, method = "MA")
plot(LoblollySMA)
plot(LoblollyMA)
residloblollysma<-residuals(LoblollySMA)
plot(residloblollysma)
loblollymodel3<-lm(residloblollysma~SOILGRIDS_CN_SCALE+aridity_1+aridity2, data=(loblolly))
loblollymodel3<-lm(residloblollysma~SOILGRIDS_CN_SCALE+aridity2, data=(loblolly))
plot(loblollymodel3)

car::vif(loblollymodel1)

par(mfrow=c (1,1))
ggplot(data=na.exclude(loblolly), aes(loblollymodel3$residuals)) +
  geom_histogram(color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

loblollymodel4<-lm(residloblollysma~aridity_1+SOILGRIDS_CN_SCALE, data=(loblolly))
loblollymodel3<-lm(residloblollysma~SOILGRIDS_CN_SCALE+aridity_1+aridity2+AVG_TEMP_bioclim, data=(loblolly))
loblollymodel5<-lm(residloblollysma~SOILGRIDS_CN_SCALE+aridity_1+aridity2, data=(loblolly))
loblollymodel2<-lm(residloblollysma~aridity_1*SOILGRIDS_CN_SCALE+AVG_TEMP_bioclim, data=(loblolly))
loblollymodel1<-lm(residloblollysma~aridity_1+AVG_TEMP_bioclim+SOILGRIDS_CN_SCALE, data=(loblolly))

# loblollymodel4<-lm(logDIA~logAGEDIA+aridity_1+SOILGRIDS_CN_SCALE, data=(loblolly))

#WINNER WINNER CHICKEN DINNER
loblollymodel3<-lm(logDIA~logAGEDIA+SOILGRIDS_CN_SCALE+aridity_1+aridity2, data=(loblolly))
plot(residloblollysma~aridity_1, data=loblolly)

# loblollymodel2<-lm(logDIA~logAGEDIA+aridity_1*SOILGRIDS_CN_SCALE, data=(loblolly))
# loblollymodel1<-lm(logDIA~logAGEDIA+aridity_1+AVG_TEMP_bioclim+logSOILGRIDS_CN_SCALE, data=(loblolly))

bbmle::AICctab(loblollymodel1, loblollymodel2, loblollymodel3, loblollymodel4,loblollymodel5, base=T, delta=T, weights=T)
bbmle::BICtab(loblollymodel1, loblollymodel2, loblollymodel3, loblollymodel4, loblollymodel5, base=T, delta=T, weights=T)


sdDIA<-sd(residloblollysma)
model3 <- map2stan(
  alist(
    residloblollysma ~ dnorm(mu,sigma),
    mu <- a + c*AVG_TEMP_bioclim + d*SOILGRIDS_CN_SCALE + e*aridity_1,
    a ~ dnorm(0,10),
    b ~ dnorm(0,10),
    c ~ dnorm(0,10),
    d ~ dnorm(0,10),
    e ~ dnorm(0,10),
    sigma ~ dunif(0,100)
  ),
  data = loblolly,chains =3,
  start <- list(a = 0, b= 0, c=0, d=0, e=0, sigma=sdDIA)
)

precis(model3,digits=3)
plot(model3)
pairs(model3)

pairs(logDIA~aridity_1+logSOILGRIDS_CN_SCALE+AVG_TEMP_bioclim+logAGEDIA+aridity2, data=(loblolly))
car::vif(loblollymodel1)
A<-seq(.4,1.2,.05)

loblollygrowth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
loblollyage<-(10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(env[i,j]^(1.350321))

diameter<-(10^(-1.21+3.2*A-(1.97*A^2)-.001*57)*28^(.74))
plot(diameter~A)

#loblollymodel2<-lm(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+SOIL_MOIS, data=loblolly)
#loblollymodel<-lm(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+loblolly$precip_1, data=loblolly)
summary(loblollymodel3)
plot(loblollymodel3)
summary(loblollymodel4)
#summary(loblollymodel2)
#summary(loblollymodel)
slash<-slashpinemodel4$coefficients
long<-longleafmodel3$coefficients
lob<-loblollymodel4$coefficients


# slashgrowth<-(10^.381)*(10^(.012*22))*(.346*slashpine$AGEDIA^-.654)*(60^-0.069)*(10^(-.0001*250))
# longleafgrowth<-(10^-.043)*(10^(.023*22))*(.386*longleaf$AGEDIA^-.61)*(10^(-.00033*250))
# loblollygrowth<-(10^(-.56))*10^(.053*22)*(.45*loblolly$AGEDIA^-.55)  
slashgrowth<-(10^slash[1])*(10^(slash[2]*20))*(slash[4]*slashpine$AGEDIA^-.649)*(10^(slash[3]*.8753))
#slashgrowth<-(10^2.972)*(10^(-.00723*20))*(.351*slashpine$AGEDIA^-.649)*(8753^(-.584))
#longleafgrowth<-(10^4.720)*(10^(-.0264*20))*(.392*longleaf$AGEDIA^-.608)*(8753^(-.979))
longleafgrowth<-(10^long[1])*(10^(long[2]*20))*(long[3]*longleaf$AGEDIA^-.608)*(10^(long[5]*.8753))*(51^long[4])
loblollygrowth<-(10^(lob[1]))*(10^(lob[3]*.8753))*(lob[2]*loblolly$AGEDIA^-.535)  
#loblollygrowth<-(10^(4.833))*(8753^(-1.058))*(.465*loblolly$AGEDIA^-.535)  
#par(mfrow=c(1,1), mgp = c(5, 1, 0))
par(mfrow=c(1,1))
plot(slashpine$AGEDIA, slashgrowth, cex.axis=1.5, col.axis="#027368", ann=FALSE, col="#75BFBF", pch=16, type="p", font=2)
mtext(side = 1, text = "Age (yr)", line = 3, cex=1.5)
mtext(side = 3, text = "Growth Rates", line = 1, cex=1.5)
mtext(side = 2, text = "Growth (in/yr)", line = 2, cex=1.5)
lines(longleaf$AGEDIA, longleafgrowth, type="p", pch=16, col="#048ABF")
lines(loblolly$AGEDIA, loblollygrowth, type="p", col="#A7C8F2", pch=16)
legend(90,.2, legend=c("Loblolly", "Longleaf","Slashpine"), col=c("#A7C8F2", "#048ABF","#75BFBF"), pch=16, cex=1.5)
age<-seq(1,100,1)
lines(age, long[3]*age^.9, type="p", pch=16, col="#048ABF")

plot(sitetree$DIA ~ sitetree$AGEDIA , cex.axis=1.5, col.axis="#027368", ann=FALSE, col="#75BFBF", pch=16, type="p", font=2)
mtext(side = 1, text = "Age (yr)", line = 3, cex=1.5)
mtext(side = 2, text = "Diameter (in)", line = 3, cex=1.5)
lines(longleaf$AGEDIA, longleafgrowth, type="p", pch=16, col="#048ABF")
lines(loblolly$AGEDIA, loblollygrowth, type="p", col="#A7C8F2", pch=16)
legend(90,.2, legend=c("Loblolly", "Longleaf","Slashpine"), col=c("#A7C8F2", "#048ABF","#75BFBF"), pch=16, cex=1.5)


height<-lm(log(sitetree$DIA)~log(sitetree$HT))
summary(height)
plot(log(sitetree$DIA)~log(sitetree$HT))

predict_age<-lm(data = slashpine, logAGEDIA ~ logDIA + AVG_TEMP_bioclim + temp2 + aridity_1)
summary(predict_age)           
