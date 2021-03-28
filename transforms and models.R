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

save(longleaf, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/LongleafData.Rdata")
save(loblolly, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/LoblollyData.Rdata")
save(slashpine, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/SlashData.Rdata")

SlashMA <- smatr::sma(data=slashpine, log10(slashpine$DIA)~log10(slashpine$AGEDIA), method = "SMA")
slashresidualsma<-residuals(SlashMA)

int.model3<-lm(slashresidualsma~ SOILGRIDS_CN_SCALE*ai_et0_NAD + slashpine$X30s_NAD, data=slashpine)

#Calculating var.cov matrix for age
cov<-matrix(data=NA, 2,2)

mu.y<-mean(log10(slashpine$DIA))
mu.x<-mean(log10(slashpine$AGEDIA))

xi<-as.vector(log10(slashpine$AGEDIA))
sq.xi<-(xi-mu.x)^2
cov[1,1]<-sum(sq.xi)/2285
yi<-as.vector(log10(slashpine$DIA))
sq.yi<-(yi-mu.y)^2
cov[2,2]<-sum(sq.yi)/2285

covars<-(xi-mu.x)*(yi-mu.x)
cov[1,2]<-sum(covars)/2285
cov[2,1]<-sum(covars)/2285

age.par<-MASS::mvrnorm(n=300, mu=mu.age, Sigma = cov)

# smaSlope <- function(x,y) {
#   sign <- ifelse(cor(x,y) >= 0, 1, -1)
#   b1 <- sign * sd(y)/sd(x)
#   b1
# }
# 
# smaSlope(log10(slashpine$AGEDIA),log10(slashpine$DIA))
# 
# smaIntercept <- function(x, y) {
#   b1 <- smaSlope(x, y)
#   b0 <- mean(y) - mean(x)*b1
#   b0
# }



mu<-int.model3$coefficients
cov.intmodel<-as.matrix(vcov(int.model3))
growth.par<-MASS::mvrnorm(n=300, mu=mu, Sigma = cov.intmodel)



slashpine$temp2<-slashpine$X30s_NAD^2



# Call: smatr::sma(formula = log10(slashpine$DIA) ~ log10(slashpine$AGEDIA), 
#                 data = slashpine, method = "SMA") 
# 
# Fit using Standardized Major Axis 
# 
# ------------------------------------------------------------
#   Coefficients:
#   elevation     slope
# estimate    0.1494024 0.5783973
# lower limit 0.1215712 0.5599690
# upper limit 0.1772336 0.5974321
# 
# H0 : variables uncorrelated
# R-squared : 0.3770741 
# P-value : < 2.22e-16 


cor.test(slashpine$X30s_NAD, slashresidualsma, method = c("pearson")) #0.19
cor.test(slashpine$ai_et0_NAD, slashresidualsma, method = c("pearson")) #-0.27
cor.test(slashpine$SOILGRIDS_CN_SCALE, slashresidualsma, method = c("pearson")) #-0.10
cor.test(log10(slashpine$DIA), log10(slashpine$AGEDIA), method = c("pearson")) #0.61

my_data <- slashpine[, c("DIA","X30s_NAD","ai_et0_NAD","SOILGRIDS_CN_SCALE")]
res <- cor(my_data, use = "complete.obs")
round(res, 2)

modelnames <- c("full.model", "add.model", "notemp.model", "int.model2", "int.model3", "int.model4")

PRESS <- c(117.2473, 121.8596, 122.7548, 121.7957, 116.8553, 117.4792)

Adj.r <- c(0.06016, 0.06254, 0.05519, 0.06339, 0.06364, 0.05821)

BIC <- c(-183.0376, -174.4199, -167.7221, -169.7606, -184.5131, --178.4704)



df<-cbind(modelnames, PRESS, Adj.r, BIC)

df<-data.frame(df)

colnames(df)<-c("Model","PRESS","Adjusted R-squared", "BIC")


MPV::PRESS(full.model) #117.2473, MA 95.62182
MPV::PRESS(add.model) #121.8596, MA 99.94328
MPV::PRESS(notemp.model) #122.7548, MA 100.2449
MPV::PRESS(int.model2) #121.7957, MA 100.2449
MPV::PRESS(int.model3) #116.8553, MA 94.72511  
MPV::PRESS(int.model4) #117.4792, MA 94.80305
MPV::PRESS(.model) #121.8121, MA 99.89651

#adj_r_squared*******model3

BIC(full.model) #-183.0376, MA -632.9722
BIC(add.model) #-174.4199, MA -625.0302
BIC(notemp.model) #-167.7221, MA -628.8401
BIC(int.model2) #-169.7606, MA  -617.7074
BIC(int.model3) #-184.5131, MA -647.9377
BIC(int.model4) #-178.4704, MA -651.7585


full.model<-lm(slashresidualsma~slashpine$X30s_NAD + slashpine$ai_et0_NAD + slashpine$SOILGRIDS_CN_SCALE) #CN not significant
summary(full.model) #0.06016, MA0.0681
plot(full.model)
car::vif(full.model)

# X30s_NAD         ai_et0_NAD SOILGRIDS_CN_SCALE 
# 3.587173           3.812656           1.122901 

pairs(slashresidualssma~X30s_NAD + ai_et0_NAD + SOILGRIDS_CN_SCALE, data=slashpine)

.model<-lm(slashresidualsma~X30s_NAD + temp2 +  ai_et0_NAD, data=slashpine) #temp and temp2 significant at .1
summary(.model) #0.06338, MA 0.07518 

add.model<-lm(slashresidualsma~slashpine$X30s_NAD + slashpine$ai_et0_NAD, data=slashpine)
summary(add.model) #0.06254, MA 0.07426

notemp.model<-lm(slashresidualsma~slashpine$ai_et0_NA)
summary(notemp.model) #0.05519, MA 0.07107

int.model2<-lm(slashresidualsma~slashpine$X30s_NAD * slashpine$ai_et0_NAD, data=slashpine) #no significant predictors
summary(int.model2) # 0.06339, MA 0.07401

int.model3<-lm(slashresidualsma~ SOILGRIDS_CN_SCALE*ai_et0_NAD + slashpine$X30s_NAD, data=slashpine)
summary(int.model3) #0.06364, MA 0.07762
plot(int.model3)

##Creating an interaction plot
int<-lm(slashresidualsma~ SOILGRIDS_CN_SCALE*ai_et0_NAD , data = slashpine)
summary(int)
# library(effects)
Inter.HandPick <- effects::effect('SOILGRIDS_CN_SCALE*ai_et0_NAD', int,
                         xlevels=list(SOILGRIDS_CN_SCALE = c(40, 80, 120),
                                      ai_et0_NAD = c(.7, .8, .9)),
                         se=TRUE, confidence.level=.95, typical=mean)

#Put data in data frame 
Inter.HandPick <- as.data.frame(Inter.HandPick)

#Check out what the "head" (first 6 rows) of your data looks like
head(Inter.HandPick)
#Create a factor of the IQ variable used in the interaction                   
Inter.HandPick$SOILGRIDS_CN_SCALE <- factor(Inter.HandPick$SOILGRIDS_CN_SCALE,
                            levels=c(40, 80, 120),
                            labels=c("High N", "Avg N", "Low N"))

#Create a factor of the Work Ethic variable used in the interaction 
Inter.HandPick$ai_et0_NAD <- factor(Inter.HandPick$ai_et0_NAD,
                                    levels=c(.7, .8, .9),
                                    labels=c("Drier", "Wetter", "Wettest"))

Inter.HandPick[,"fit"]<-10^(Inter.HandPick[,"fit"])
Inter.HandPick[,"DBH"]<-dbh

dbh<-10^((0.764+0.149)-(0.011*22)-(0.597*Inter.HandPick$ai_et0_NAD)-
           (0.004*Inter.HandPick$SOILGRIDS_CN_SCALE)+(0.005*Inter.HandPick$ai_et0_NAD*Inter.HandPick$SOILGRIDS_CN_SCALE))*0.578*(10^0.578-1)




library(ggplot2)                
ggplot2::ggplot(data=Inter.HandPick, aes(x=ai_et0_NAD, y=DBH, group=SOILGRIDS_CN_SCALE))+
  geom_line(size=2, aes(color=SOILGRIDS_CN_SCALE))+
  # ylim(0,4)+
  ylab("DBH")+
  xlab("Aridity")+
  labs(color = "Soil C:N")+
  ggtitle("Slash pine interaction plot") +
theme_Publication() +
  # scale_color_manual(values=c("#9ACFDD","#668C4A","#B29577"))
  scale_color_manual(values=c("#9ACFDD","#FFF447","#D90B1C"))
# scale_colour_Publication()
#   scale_fill_Publication() +


Plot.HandPick 
# Call:
#   lm(formula = slashresidualsma ~ SOILGRIDS_CN_SCALE * ai_et0_NAD + 
#        slashpine$X30s_NAD, data = slashpine)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.35391 -0.06433  0.00175  0.06732  0.46605 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.763963   0.105641   7.232 6.55e-13 ***
#   SOILGRIDS_CN_SCALE            -0.004317   0.001359  -3.176 0.001512 ** 
#   ai_et0_NAD                    -0.596886   0.087543  -6.818 1.19e-11 ***
#   slashpine$X30s_NAD            -0.011454   0.003088  -3.709 0.000214 ***
#   SOILGRIDS_CN_SCALE:ai_et0_NAD  0.004611   0.001523   3.029 0.002485 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.09985 on 2201 degrees of freedom
# (80 observations deleted due to missingness)
# Multiple R-squared:  0.06534,	Adjusted R-squared:  0.06364 
# F-statistic: 38.46 on 4 and 2201 DF,  p-value: < 2.2e-16


int.model4<-lm(slashresidualsma~ ai_et0_NAD*SOILGRIDS_CN_SCALE, data=slashpine)
summary(int.model4) #0.05821, MA 0.07641



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

MPV::PRESS(full.model) # 35.60895
MPV::PRESS(add.model) #36.59297
MPV::PRESS(notemp.model) #37.60718
MPV::PRESS(int.model2) #35.76112
MPV::PRESS(int.model3) #34.99162 *****model3
MPV::PRESS(int.model4) #35.77264 
MPV::PRESS(int.model5) #36.35493 

#adj_rsquared*****model3

BIC(full.model) #-124.0053
BIC(add.model) #-117.941
BIC(notemp.model) #-102.5656
BIC(int.model2) #-130.626
BIC(int.model3) #-132.0229*****model3
BIC(int.model4) #-120.7723
BIC(int.model5) # -113.8396

pairs(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+aridity_1, data=longleaf)

par(mfrow=c (1,1))
plot(LongleafSMA)
LongleafSMA <- smatr::sma(data=longleaf, log10(DIA) ~ log10(AGEDIA), method = "SMA")
resid.LLSMA<- residuals(LongleafSMA)
plot(resid.LLSMA)
summary(LongleafSMA)

# Call: smatr::sma(formula = log10(DIA) ~ log10(AGEDIA), data = longleaf, 
#                  method = "SMA") 
# 
# Fit using Standardized Major Axis 
# 
# ------------------------------------------------------------
#   Coefficients:
#   elevation     slope
# estimate    0.11929406 0.5718692
# lower limit 0.06797308 0.5415208
# upper limit 0.17061505 0.6039184
# 
# H0 : variables uncorrelated
# R-squared : 0.4207783 
# P-value : < 2.22e-16 

full.model<-lm(resid.LLSMA~X30s_NAD + ai_et0_NAD + SOILGRIDS_CN_SCALE, data=longleaf) #all significant
summary(full.model) #0.1145 
plot(full.model)

car::vif(full.model)

# X30s_NAD         ai_et0_NAD SOILGRIDS_CN_SCALE 
# 6.255180           6.211489           1.683972

notemp.model<-lm(resid.LLSMA~ai_et0_NAD, data=longleaf)
summary(notemp.model) #0.07421 

add.model<-lm(resid.LLSMA~ai_et0_NAD + X30s_NAD, data=longleaf)
summary(add.model) #0.0997
# plot(int.model)

int.model2<-lm(resid.LLSMA~ai_et0_NAD * X30s_NAD, data=longleaf)
summary(int.model2) #0.1213
# plot(int.model2)

int.model3<-lm(resid.LLSMA~ SOILGRIDS_CN_SCALE*ai_et0_NAD + X30s_NAD, data=longleaf)
summary(int.model3) #0.1306


int<-lm(resid.LLSMA~ SOILGRIDS_CN_SCALE*ai_et0_NAD , data = slashpine)
summary(int)
# library(effects)
Inter.HandPick <- effects::effect('SOILGRIDS_CN_SCALE*ai_et0_NAD', int,
                                  xlevels=list(SOILGRIDS_CN_SCALE = c(40, 80, 120),
                                               ai_et0_NAD = c(.7, .8, .9)),
                                  se=TRUE, confidence.level=.95, typical=mean)

#Put data in data frame 
Inter.HandPick <- as.data.frame(Inter.HandPick)

dbh<-10^((1.655+0.119)-(0.034*22)-(1.128*Inter.HandPick$ai_et0_NAD)-
           (0.007*Inter.HandPick$SOILGRIDS_CN_SCALE)+(0.009*Inter.HandPick$ai_et0_NAD*Inter.HandPick$SOILGRIDS_CN_SCALE))*0.572*(10^0.572-1)

Inter.HandPick[,"DBH"]<-dbh
#Check out what the "head" (first 6 rows) of your data looks like
head(Inter.HandPick)
#Create a factor of the IQ variable used in the interaction                   
Inter.HandPick$SOILGRIDS_CN_SCALE <- factor(Inter.HandPick$SOILGRIDS_CN_SCALE,
                                            levels=c(40, 80, 120),
                                            labels=c("High N", "Avg N", "Low N"))

#Create a factor of the Work Ethic variable used in the interaction 
Inter.HandPick$ai_et0_NAD <- factor(Inter.HandPick$ai_et0_NAD,
                                    levels=c(.7, .8, .9),
                                    labels=c("Drier", "Wetter", "Wettest"))



# library(ggplot2)                
ggplot2::ggplot(data=Inter.HandPick, aes(x=ai_et0_NAD, y=DBH, group=SOILGRIDS_CN_SCALE))+
  geom_line(size=2, aes(color=SOILGRIDS_CN_SCALE))+
  # ylim(0,4)+
  ylab("DBH")+
  xlab("Aridity")+
  labs(color = "Soil C:N")+
  ggtitle("Longleaf pine interaction plot") +
  theme_Publication() +
  # scale_color_manual(values=c("#9ACFDD","#668C4A","#B29577"))
  # scale_color_manual(values=c("#9ACFDD","#FFF447","#D90B1C"))
  scale_color_manual(values=c("#023E73","#067354","#A60303"))
# scale_colour_Publication()
#   scale_fill_Publication() +



# Call:
#   lm(formula = resid.LLSMA ~ SOILGRIDS_CN_SCALE * ai_et0_NAD + 
#        X30s_NAD, data = longleaf)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.37833 -0.06697  0.00078  0.07075  0.22704 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    1.654958   0.256073   6.463 1.86e-10 ***
#   SOILGRIDS_CN_SCALE            -0.007376   0.002164  -3.408 0.000690 ***
#   ai_et0_NAD                    -1.127642   0.149500  -7.543 1.35e-13 ***
#   X30s_NAD                      -0.034125   0.008053  -4.238 2.55e-05 ***
#   SOILGRIDS_CN_SCALE:ai_et0_NAD  0.008805   0.002299   3.831 0.000139 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.09395 on 739 degrees of freedom
# (8 observations deleted due to missingness)
# Multiple R-squared:  0.1352,	Adjusted R-squared:  0.1306 
# F-statistic: 28.89 on 4 and 739 DF,  p-value: < 2.2e-16


mu<-int.model3$coefficients
cov.intmodel<-as.matrix(vcov(int.model3))
growth.par<-MASS::mvrnorm(n=300, mu=mu, Sigma = cov.intmodel)

mu.age<-coef(LongleafSMA)
cov.age<-vcov(LongleafSMA)
age.par<-MASS::mvrnorm(n=300, mu=mu.age, Sigma = cov.age)

x<-c(5, 15, 25, 35, 45, 55, 65, 75)
x2<-x^2
y<-c((.162/8), (.026/8), (.006/8), (.013/8), (.024/8), (.047/8), (.060/8), (0.129/8))

mod<-lm(y ~ x+x2)
summary(mod)
mort.mu<-mod$coefficients
cov.mortmodel<-vcov(mod)
mort.par<-MASS::mvrnorm(n=300, mu=mort.mu, Sigma = cov.mortmodel)

initial.par<-cbind(growth.par,mort.par)
initial.par[1:300,1]<-initial.par[1:300,1]+ mu.age[1]

save(initial.par, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/initialpar300.Rdata")

plot(int.model3)

# Call:
#   lm(formula = resid.LLSMA ~ X30s_NAD + ai_et0_NAD * SOILGRIDS_CN_SCALE, 
#      data = longleaf)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.87113 -0.15420  0.00179  0.16292  0.52278 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    3.810682   0.589630   6.463 1.86e-10 ***
#   X30s_NAD                      -0.078576   0.018543  -4.238 2.55e-05 ***
#   ai_et0_NAD                    -2.596493   0.344235  -7.543 1.35e-13 ***
#   SOILGRIDS_CN_SCALE            -0.016985   0.004984  -3.408 0.000690 ***
#   ai_et0_NAD:SOILGRIDS_CN_SCALE  0.020275   0.005293   3.831 0.000139 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2163 on 739 degrees of freedom
# (8 observations deleted due to missingness)
# Multiple R-squared:  0.1352,	Adjusted R-squared:  0.1306 
# F-statistic: 28.89 on 4 and 739 DF,  p-value: < 2.2e-16


int.model4<-lm(resid.LLSMA~ ai_et0_NAD * SOILGRIDS_CN_SCALE, data=longleaf)
summary(int.model4) #0.1106
plot(int.model3)

int.model5<-lm(resid.LLSMA~ ai_et0_NAD + SOILGRIDS_CN_SCALE, data=longleaf)
summary(int.model5) # 0.09551
# plot(int.model3)



growth<-(10^(4.085367 - 0.016985*CN + 0.020275*CN*aridity - 2.596493*aridity - 0.078576*temp))*(0.5718692*age[i,j]^(-0.4281308))
age=round((10^(-7.143884 + 0.02970085*CN_scale - 0.03545391*CN_scale*aridity + 4.540362*aridity + 0.137402*temp))*(DIA^1.748652)))

bbmle::BICtab(full.model, notemp.model, int.model, int.model2, base=T, delta=T, weights=T)
bbmle::AICctab(full.model, notemp.model, int.model, int.model2, base=T, delta=T, weights=T)

# Loblolly ----------------------------------------------------------------

#loblolly$temp2<-loblolly$AVG_TEMP_bioclim^2
loblolly$aridity2<-loblolly$ai_et0_NAD^2

par(mfrow=c (1,1))
LoblollySMA <- smatr::sma(data=loblolly, log10(DIA) ~ log10(AGEDIA), method = "SMA")
LoblollyMA <- smatr::sma(data=loblolly, log(DIA) ~ log(AGEDIA), method = "MA")
#Call: smatr::sma(formula = log10(DIA) ~ log10(AGEDIA), data = loblolly, 
# method = "SMA") 
# 
# Fit using Standardized Major Axis 
# 
# ------------------------------------------------------------
#   Coefficients:
#   elevation     slope
# estimate     0.002217684 0.7405648
# lower limit -0.064798077 0.6945966
# upper limit  0.069233446 0.7895752
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

MPV::PRESS(loblollymodel1) #6.793591 ***fullmodel
MPV::PRESS(loblollymodel1.5) #6.822152
MPV::PRESS(model1) #6.772319
MPV::PRESS(model1.5) #6.663459
MPV::PRESS(loblollymodel1.6) #6.817234

#adj_rsquared****model1

BIC(loblollymodel1) #-822.264
BIC(loblollymodel1.5) #-824.2888
BIC(model1) #-836.2988****model1
BIC(model1.5) #-828.2569
BIC(loblollymodel1.6) #-816.0232

olsrr::ols_mallows_cp(loblollymodel1.5, loblollymodel1) # should be 3, 4.7
olsrr::ols_mallows_cp(model1, loblollymodel1) # should be 4, .16
olsrr::ols_mallows_cp(model1.5, loblollymodel1) # should be 4, 5.3
olsrr::ols_mallows_cp(notemp.model, full.model) # should be 2, 16.1


car::vif(loblollymodel1)

# ai_et0_NAD  loblolly$X30s_NAD SOILGRIDS_CN_SCALE 
# 2.582804           4.037075           2.552541 

loblollymodel1<-lm(residloblollysma~ai_et0_NAD + SOILGRIDS_CN_SCALE +aridity2, data=(loblolly))
summary(loblollymodel1) #0.1289 

loblollymodel1.5<-lm(residloblollysma~ai_et0_NAD+SOILGRIDS_CN_SCALE, data=(loblolly))
summary(loblollymodel1.5) #0.1237

loblollymodel1.6<-lm(residloblollysma~ai_et0_NAD*SOILGRIDS_CN_SCALE + loblolly$X30s_NAD, data=(loblolly))
summary(loblollymodel1.6) #0.1274

model1<-lm(residloblollysma~ai_et0_NAD*X30s_NAD, data=loblolly)
summary(model1) #0.1462

Inter.HandPick <- effects::effect('ai_et0_NAD*X30s_NAD', model1,
                                  xlevels=list(X30s_NAD = c(20, 22, 24),
                                               ai_et0_NAD = c(.7, .8, .9)),
                                  se=TRUE, confidence.level=.95, typical=mean)

#Put data in data frame 
Inter.HandPick <- as.data.frame(Inter.HandPick)

dbh<-10^((7.586+0.002)-(9.204*Inter.HandPick$ai_et0_NAD)-
           (0.372*Inter.HandPick$X30s_NAD)+(0.454*Inter.HandPick$ai_et0_NAD*Inter.HandPick$X30s_NAD))*0.741*(10^0.741-1)

Inter.HandPick[,"DBH"]<-dbh
#Check out what the "head" (first 6 rows) of your data looks like
head(Inter.HandPick)
#Create a factor of the IQ variable used in the interaction                   
Inter.HandPick$X30s_NAD <- factor(Inter.HandPick$X30s_NAD,
                                            levels=c(20, 22, 24),
                                            labels=c("20", "22", "24"))

#Create a factor of the Work Ethic variable used in the interaction 
Inter.HandPick$ai_et0_NAD <- factor(Inter.HandPick$ai_et0_NAD,
                                    levels=c(.7, .8, .9),
                                    labels=c("Drier", "Wetter", "Wettest"))



# library(ggplot2)                
ggplot2::ggplot(data=Inter.HandPick, aes(x=ai_et0_NAD, y=DBH, group=X30s_NAD))+
  geom_line(size=2, aes(color=X30s_NAD))+
  # ylim(0,4)+
  ylab("DBH")+
  xlab("Aridity")+
  labs(color = "MAT")+
  ggtitle("Loblolly pine interaction plot") +
  theme_Publication() +
    scale_color_manual(values=c("#236670","#F2A20C","#F26B5E"))

scale_color_manual(values=c("#9ACFDD","#668C4A","#B29577"))

  scale_color_manual(values=c("#9ACFDD","#FFF447","#D90B1C"))
# scale_colour_Publication()
#   scale_fill_Publication() +




# Call:
#   lm(formula = residloblollysma ~ ai_et0_NAD * loblolly$X30s_NAD, 
#      data = (loblolly))
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.31782 -0.07277  0.00520  0.07434  0.35620 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   7.58620    1.44580   5.247 2.21e-07 ***
#   ai_et0_NAD                   -9.20441    1.72577  -5.334 1.41e-07 ***
#   loblolly$X30s_NAD            -0.37183    0.07448  -4.992 8.04e-07 ***
#   ai_et0_NAD:loblolly$X30s_NAD  0.45382    0.08961   5.065 5.60e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1105 on 547 degrees of freedom
# Multiple R-squared:  0.1509,	Adjusted R-squared:  0.1462 
# F-statistic:  32.4 on 3 and 547 DF,  p-value: < 2.2e-16

model1.5<-lm(residloblollysma~ai_et0_NAD*loblolly$X30s_NAD + SOILGRIDS_CN_SCALE, data=(loblolly)) #CN not significant
summary(model1.5) #0.1467




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
