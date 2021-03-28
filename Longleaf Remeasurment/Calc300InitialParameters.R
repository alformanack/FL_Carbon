setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment")
load('LongleafData.Rdata')

#SMA model
LongleafSMA <- smatr::sma(data=longleaf, log10(DIA) ~ log10(AGEDIA), method = "SMA")
resid.LLSMA<- residuals(LongleafSMA)

#Env variable model
int.model3<-lm(resid.LLSMA~ SOILGRIDS_CN_SCALE*ai_et0_NAD + X30s_NAD, data=longleaf)

#Sampling 300 parameters from multivariate normal dist for env variable model
mu<-int.model3$coefficients
cov.intmodel<-as.matrix(vcov(int.model3))
growth.par<-MASS::mvrnorm(n=300, mu=mu, Sigma = cov.intmodel)

#Saving parameters of SMA model
mu.age<-LongleafSMA$coef[[1]][2,1]
mu.int<-LongleafSMA$coef[[1]][1,1]
###need: standard error for intercept and age from CI's
sigma<-((LongleafSMA$coef[[1]][2,3]-LongleafSMA$coef[[1]][2,1])/1.96)
age.par<-rnorm(300, mean=mu.age, sd=sigma)

sigma.int<-((LongleafSMA$coef[[1]][1,3]-LongleafSMA$coef[[1]][1,1])/1.96)
int.par<-rnorm(300, mean=mu.int, sd=sigma.int)
# cov.age<-vcov(LongleafSMA)
# age.par<-MASS::mvrnorm(n=300, mu=mu.age, Sigma = cov.age)

#Mortality Model
x<-c(5, 15, 25, 35, 45, 55, 65, 75)
x2<-x^2
y<-c((.162/8), (.026/8), (.006/8), (.013/8), (.024/8), (.047/8), (.060/8), (0.129/8))

mod<-lm(y ~ x+x2)

#Sampling 00 parameters from multivariate normal dist for mortality model
mort.mu<-mod$coefficients
cov.mortmodel<-vcov(mod)
mort.par<-MASS::mvrnorm(n=300, mu=mort.mu, Sigma = cov.mortmodel)

#Added the SMA intercept to the intercept of env var model and kept age slope/intercept constant
initial.par<-cbind(growth.par,age.par, mort.par)
initial.par[1:300,1]<-initial.par[1:300,1]+ int.par

save(initial.par, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/initialpar3.22.21.Rdata")
