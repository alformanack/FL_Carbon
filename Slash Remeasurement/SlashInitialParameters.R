rm(list=ls())
setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement")
load('SlashData.Rdata')

#SMA model
SlashMA <- smatr::sma(data=slashpine, log10(slashpine$DIA)~log10(slashpine$AGEDIA), method = "SMA")
slashresidualsma<-residuals(SlashMA)

#Env variable model
int.model3<-lm(slashresidualsma~ SOILGRIDS_CN_SCALE*ai_et0_NAD + slashpine$X30s_NAD, data=slashpine)

#Sampling 300 parameters from multivariate normal dist for env variable model
mu<-int.model3$coefficients
cov.intmodel<-as.matrix(vcov(int.model3))
growth.par<-MASS::mvrnorm(n=300, mu=mu, Sigma = cov.intmodel)

#Saving parameters of SMA model
mu.age<-SlashMA$coef[[1]][2,1]
mu.int<-SlashMA$coef[[1]][1,1]
###need: standard error for intercept and age from CI's
sigma<-((SlashMA$coef[[1]][2,3]-SlashMA$coef[[1]][2,1])/1.96)
age.par<-rnorm(300, mean=mu.age, sd=sigma)

sigma.int<-((SlashMA$coef[[1]][1,3]-SlashMA$coef[[1]][1,1])/1.96)
int.par<-rnorm(300, mean=mu.int, sd=sigma.int)
# cov.age<-vcov(LongleafSMA)
# age.par<-MASS::mvrnorm(n=300, mu=mu.age, Sigma = cov.age)

#Mortality Model
x<-c(5, 15, 25, 35, 45)

y<-c((.4/8), (.2/8), (.15/8), (.1/8), (.01/8))

mod <- nls(y ~ a*exp(r*x), 
               start = list(a = -2, r = -0.05))

#Sampling 00 parameters from multivariate normal dist for mortality model
mort.mu<-coef(mod)
cov.mortmodel<-vcov(mod)
mort.par<-MASS::mvrnorm(n=300, mu=mort.mu, Sigma = cov.mortmodel)

#Added the SMA intercept to the intercept of env var model and kept age slope/intercept constant
initial.par<-cbind(growth.par,age.par, mort.par)
initial.par[1:300,1]<-initial.par[1:300,1]+ int.par

save(initial.par, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/initialpar3.22.21.Rdata")
