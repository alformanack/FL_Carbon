rm(list=ls())
setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
# sitetree<-read.csv("sitetree12.17.19.csv", header=T, sep=",", stringsAsFactors = F)
# sitetree<-read.csv("precip.csv", header=T, sep=",", stringsAsFactors = F)
# sitetree<-read.csv("aridity3.csv", header=T, sep=",", stringsAsFactors = F)
# sitetree<-read.csv("aridity3.csv", header=T, sep=",", stringsAsFactors = F)
sitetree<-read.csv("sitetree_NAD83_envData.csv", header=T, sep=",", stringsAsFactors = F)



sitetree$SPCD<-as.factor(sitetree$SPCD)


# Calculate soil C:N for ORNL ---------------------------------------------

# sitetree$ORNL_CN<-sitetree$SOC_ORNL_kgperm2*1000/sitetree$N_ORNL_gperm2

# Calculate weighted averages and soil C:N for Soilgrids data -------------


is.na(sitetree$SOC0_5_NAD) <- sitetree$SOC0_5_NAD==-9999
is.na(sitetree$SOC5_15_NA) <- sitetree$SOC5_15_NA==-9999

is.na(sitetree$N0_5_NAD) <- sitetree$N0_5_NAD==-9999
is.na(sitetree$N5_15_NAD) <- sitetree$N5_15_NAD==-9999

is.na(sitetree$X30s_NAD) <- sitetree$X30s_NAD==-9999
is.na(sitetree$ai_et0_NAD) <- sitetree$ai_et0_NAD==-9999

# sitetree <- sitetree[which(!is.na(sitetree$SOC0_5_NAD) &
#                  !is.na(sitetree$SOC5_15_NA) &
#                  !is.na(sitetree$N0_5_NAD) &
#                  !is.na(sitetree$X30s_NAD) &
#                  !is.na(sitetree$ai_et0_NAD) &
#                  !is.na(sitetree$N5_15_NAD)),]



sitetree$SOILGRIDS_C_AVG<-((sitetree$SOC0_5_NAD/10)*(1/3))+((sitetree$SOC5_15_NA/10)*(2/3))
                 

sitetree$SOILGRIDS_N_AVG<-((sitetree$N0_5_NAD/100)*(1/3))+((sitetree$N5_15_NAD/100)*(2/3))                 

sitetree$SOILGRIDS_CN<-sitetree$SOILGRIDS_C_AVG/sitetree$SOILGRIDS_N_AVG 


sitetree[which(is.finite(sitetree$SOILGRIDS_CN)==F),"SOILGRIDS_CN"]<-NA 


sitetree$FIA_CN_RAT<-as.numeric(sitetree$FIA_CN_RAT)
sitetree[which(is.finite(sitetree$FIA_CN_RAT)==F),"FIA_CN_RAT"]<-NA


# Comparing soils data with FIA -------------------------------------------

# plot(sitetree$ORNL_CN, sitetree$FIA_CN_RATIO)                 

plot(sitetree$SOILGRIDS_CN, sitetree$FIA_CN_RAT)

model<-lm(na.omit(sitetree$FIA_CN_RAT~sitetree$SOILGRIDS_CN, data=sitetree))
summary(model)

#scale CN from FIA data
sitetree$SOILGRIDS_CN_SCALE<-(sitetree$SOILGRIDS_CN*6.212)+24.634


# Checking variable normality ---------------------------------------------

# normali1 <- function (x)
# {mnT <- mean(x,na.rm=T)
# sdT <- sd(x,na.rm=T)
# h<-hist(x, breaks=10, col="orangered1")
# xfit<-seq(min(x,na.rm=T),max(x,na.rm=T), length=100) 
# yfit<-dnorm(xfit,mean=mnT,sd=sdT) 
# yfit <- yfit*diff(h$mids[1:2])*length(x) 
# lines(xfit, yfit, col="purple3", lwd=2)
# qqnorm(x)
# qqline(x)
# shapiro.test(x)}

sitetree$ai_et0_NAD<-sitetree$ai_et0_NAD*.0001

# Subsetting species ------------------------------------------------------

slashpine<-subset(sitetree, sitetree$SPCD=="111")
longleaf<-subset(sitetree, sitetree$SPCD=="121")
loblolly<-subset(sitetree, sitetree$SPCD=="131")


slashpine$temp2<-slashpine$X30s_NAD^2

SlashMA <- smatr::sma(data=slashpine, log(slashpine$DIA)~log(slashpine$AGEDIA), method = "MA")
slashresidualsma<-residuals(SlashMA)
plot(SlashMA)
summary(SlashMA)

# Call: smatr::sma(formula = log(slashpine$DIA) ~ log(slashpine$AGEDIA), 
#                  data = slashpine, method = "MA") 
# 
# Fit using Major Axis 
# 
# ------------------------------------------------------------
#   Coefficients:
#   elevation     slope
# estimate    0.8340824 0.4334511
# lower limit 0.7562790 0.4107720
# upper limit 0.9118858 0.4565120
# 
# H0 : variables uncorrelated
# R-squared : 0.3770741 
# P-value : < 2.22e-16 

cor.test(slashpine$X30s_NAD, slashresidualsma, method = c("pearson")) #0.19
cor.test(slashpine$ai_et0_NAD, slashresidualsma, method = c("pearson")) #-0.27
cor.test(slashpine$SOILGRIDS_CN_SCALE, slashresidualsma, method = c("pearson")) #-0.10
cor.test(log10(slashpine$DIA), log10(slashpine$AGEDIA), method = c("pearson")) #0.61

MPV::PRESS(full.model) #95.62
MPV::PRESS(add.model) #99.94
MPV::PRESS(notemp.model) #100.24
MPV::PRESS(int.model2) #100.01
MPV::PRESS(int.model3) #94.73 ******model3
MPV::PRESS(int.model4) #94.80
MPV::PRESS(.model) #99.90

#adj_r_squared*******model3

BIC(full.model) #-632.9722
BIC(add.model) #-625.0302
BIC(notemp.model) #-628.8401
BIC(int.model2) #-617.7074
BIC(int.model3) #-647.9377
BIC(int.model4) #-651.7585 *****model4


full.model<-lm(slashresidualsma~slashpine$X30s_NAD + slashpine$ai_et0_NAD + slashpine$SOILGRIDS_CN_SCALE) #CN not significant
summary(full.model) #.0685
plot(full.model)
car::vif(full.model)

# X30s_NAD         ai_et0_NAD SOILGRIDS_CN_SCALE 
# 3.587173           3.812656           1.122901 

pairs(slashresidualssma~X30s_NAD + ai_et0_NAD + SOILGRIDS_CN_SCALE, data=slashpine)

.model<-lm(slashresidualsma~X30s_NAD + temp2 +  ai_et0_NAD, data=slashpine) #temp and temp2 significant at .1
summary(.model) #.07518

add.model<-lm(slashresidualsma~slashpine$X30s_NAD + slashpine$ai_et0_NAD, data=slashpine)
summary(int.model) #.07426

notemp.model<-lm(slashresidualsma~slashpine$ai_et0_NA)
summary(notemp.model) #.07107

int.model2<-lm(slashresidualsma~slashpine$X30s_NAD * slashpine$ai_et0_NAD, data=slashpine) #no significant predictors
summary(int.model2) #.07401

int.model3<-lm(slashresidualsma~ ai_et0_NAD*SOILGRIDS_CN_SCALE + slashpine$X30s_NAD, data=slashpine)
summary(int.model3) #.07762


int.model4<-lm(slashresidualsma~ ai_et0_NAD*SOILGRIDS_CN_SCALE, data=slashpine)
summary(int.model4) #.07641

# Call:
#   lm(formula = slashresidualsma ~ ai_et0_NAD * SOILGRIDS_CN_SCALE, 
#      data = slashpine)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.73974 -0.14481  0.00549  0.13457  1.07212 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    1.269207   0.155936   8.139 6.58e-16 ***
#   ai_et0_NAD                    -1.435603   0.178935  -8.023 1.66e-15 ***
#   SOILGRIDS_CN_SCALE            -0.015004   0.002730  -5.496 4.33e-08 ***
#   ai_et0_NAD:SOILGRIDS_CN_SCALE  0.016463   0.003075   5.354 9.51e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2071 on 2202 degrees of freedom
# (80 observations deleted due to missingness)
# Multiple R-squared:  0.07767,	Adjusted R-squared:  0.07641 
# F-statistic: 61.81 on 3 and 2202 DF,  p-value: < 2.2e-16


# write.csv(slashpine, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/slashpine_data.csv")
# write.csv(longleaf, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/longleaf_data.csv")


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


# Longleaf Data -----------------------------------------------------------

cor(longleaf$X30s_NAD, resid.LLSMA, method = c("pearson")) #0.17
cor.test(longleaf$X30s_NAD, resid.LLSMA, method = c("pearson"))
cor.test(longleaf$ai_et0_NAD, resid.LLSMA, method = c("pearson")) #-0.26
cor.test(longleaf$SOILGRIDS_CN_SCALE, resid.LLSMA, method = c("pearson")) #not significant
cor.test(log10(longleaf$DIA), log10(longleaf$AGEDIA), method = c("pearson")) #0.65

MPV::PRESS(full.model) #30.7
MPV::PRESS(int.model) #31.42602
MPV::PRESS(notemp.model) #32.18844
MPV::PRESS(int.model2) #30.77593
MPV::PRESS(int.model3) #30.11378 *****model3

#adj_rsquared*****model3

BIC(full.model) #-234.2603
BIC(int.model) #-232.4288
BIC(notemp.model) #-219.6586
BIC(int.model2) #-243.6327
BIC(int.model3) #-243.7491*****model3


pairs(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+aridity_1, data=longleaf)

par(mfrow=c (1,1))
plot(LongleafSMA)
LongleafSMA <- smatr::sma(data=longleaf, log(DIA) ~ log(AGEDIA), method = "MA")
resid.LLSMA<- residuals(LongleafSMA)
# 
# Call: smatr::sma(formula = log(DIA) ~ log(AGEDIA), data = longleaf, 
#                  method = "MA") 
# 
# Fit using Major Axis 
# 
# ------------------------------------------------------------
#   Coefficients:
#   elevation     slope
# estimate    0.7578183 0.4430331
# lower limit 0.6172557 0.4062444
# upper limit 0.8983809 0.4808524
# 
# H0 : variables uncorrelated
# R-squared : 0.4207783 
# P-value : < 2.22e-16 

full.model<-lm(resid.LLSMA~X30s_NAD + ai_et0_NAD + SOILGRIDS_CN_SCALE, data=longleaf) #all significant
summary(full.model) #.1039
plot(full.model)

car::vif(full.model)

# X30s_NAD         ai_et0_NAD SOILGRIDS_CN_SCALE 
# 6.255180           6.211489           1.683972

notemp.model<-lm(resid.LLSMA~ai_et0_NAD, data=longleaf)
summary(notemp.model) #0.0645

int.model<-lm(resid.LLSMA~ai_et0_NAD + X30s_NAD, data=longleaf)
summary(int.model) #.08712
plot(int.model)

int.model2<-lm(resid.LLSMA~ai_et0_NAD * X30s_NAD, data=longleaf)
summary(int.model2) #.1073
plot(int.model2)

int.model3<-lm(resid.LLSMA~ X30s_NAD + ai_et0_NAD * SOILGRIDS_CN_SCALE, data=longleaf)
summary(int.model3) #.1219
plot(int.model2)

bbmle::BICtab(full.model, notemp.model, int.model, int.model2, base=T, delta=T, weights=T)
bbmle::AICctab(full.model, notemp.model, int.model, int.model2, base=T, delta=T, weights=T)

# Loblolly ----------------------------------------------------------------

#loblolly$temp2<-loblolly$AVG_TEMP_bioclim^2
loblolly$aridity2<-loblolly$ai_et0_NAD^2

par(mfrow=c (1,1))
LoblollySMA <- smatr::sma(data=loblolly, log(DIA) ~ log(AGEDIA), method = "SMA")
LoblollyMA <- smatr::sma(data=loblolly, log(DIA) ~ log(AGEDIA), method = "MA")
# method = "SMA") 
# 
# Fit using Standardized Major Axis 
# 
# ------------------------------------------------------------
#   Coefficients:
#   elevation     slope
# estimate     0.005106407 0.7405648
# lower limit -0.149203086 0.6945966
# upper limit  0.159415900 0.7895752
# 
# H0 : variables uncorrelated
# R-squared : 0.4149081 
# P-value : < 2.22e-16

residloblollysma<-residuals(LoblollySMA)
residloblollyma<-residuals(LoblollyMA)
plot(residloblollysma)

cor(loblolly$X30s_NAD, residloblollyma, method = c("pearson")) #0.3
cor.test(loblolly$X30s_NAD, residloblollyma, method = c("pearson"))
cor.test(loblolly$ai_et0_NAD, residloblollyma, method = c("pearson")) #-0.366
cor.test(loblolly$SOILGRIDS_CN_SCALE, residloblollyma, method = c("pearson")) #-.315
cor.test(log10(loblolly$DIA), log10(loblolly$AGEDIA), method = c("pearson")) #0.644

MPV::PRESS(loblollymodel1) #31.03 ***fullmodel
MPV::PRESS(loblollymodel1.5) #31.07
MPV::PRESS(model1) #31.06
MPV::PRESS(model1.5) #31.10

#adj_rsquared****model1

BIC(loblollymodel1) #7.228564
BIC(loblollymodel1.5) #3.705593
BIC(model1) #3.216038****model1
BIC(model1.5) #3.606744


olsrr::ols_mallows_cp(loblollymodel1.5, loblollymodel1) # should be 3, 4.7
olsrr::ols_mallows_cp(model1, loblollymodel1) # should be 4, .16
olsrr::ols_mallows_cp(model1.5, loblollymodel1) # should be 4, 5.3
olsrr::ols_mallows_cp(notemp.model, full.model) # should be 2, 16.1


car::vif(loblollymodel1)

# ai_et0_NAD  loblolly$X30s_NAD SOILGRIDS_CN_SCALE 
# 2.582804           4.037075           2.552541 

loblollymodel1<-lm(residloblollyma~ai_et0_NAD + loblolly$X30s_NAD + SOILGRIDS_CN_SCALE, data=(loblolly))
summary(loblollymodel1) #.1528

loblollymodel1.5<-lm(residloblollyma~ai_et0_NAD+SOILGRIDS_CN_SCALE, data=(loblolly))
summary(loblollymodel1.5) #.15

model1<-lm(residloblollyma~ai_et0_NAD*loblolly$X30s_NAD, data=(loblolly))
summary(model1) #.1645

# Call:
#   lm(formula = residloblollyma ~ ai_et0_NAD * loblolly$X30s_NAD, 
#      data = (loblolly))
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.6891 -0.1689  0.0199  0.1539  0.7863 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   15.0739     3.0971   4.867 1.48e-06 ***
#   ai_et0_NAD                   -18.5794     3.6969  -5.026 6.80e-07 ***
#   loblolly$X30s_NAD             -0.7341     0.1596  -4.601 5.23e-06 ***
#   ai_et0_NAD:loblolly$X30s_NAD   0.9113     0.1920   4.748 2.63e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2367 on 547 degrees of freedom
# Multiple R-squared:  0.1691,	Adjusted R-squared:  0.1645 
# F-statistic: 37.09 on 3 and 547 DF,  p-value: < 2.2e-16

model1.5<-lm(residloblollyma~ai_et0_NAD*loblolly$X30s_NAD + SOILGRIDS_CN_SCALE, data=(loblolly)) #CN not significant
summary(model1.5) #.1507




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
