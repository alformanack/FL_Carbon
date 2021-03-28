setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement")
load('LoblollyData.Rdata')

#SMA model
LoblollySMA <- smatr::sma(data=loblolly, log10(DIA) ~ log10(AGEDIA), method = "SMA")
residloblollysma<-residuals(LoblollySMA)

#Env variable model
model1<-lm(residloblollysma~ai_et0_NAD*X30s_NAD, data=loblolly)

#Sampling 300 parameters from multivariate normal dist for env variable model
mu<-model1$coefficients
cov.model<-as.matrix(vcov(model1))
growth.par<-MASS::mvrnorm(n=300, mu=mu, Sigma = cov.model)

#Saving parameters of SMA model
mu.age<-LoblollySMA$coef[[1]][2,1]
mu.int<-LoblollySMA$coef[[1]][1,1]
###need: standard error for intercept and age from CI's
sigma<-((LoblollySMA$coef[[1]][2,3]-LoblollySMA$coef[[1]][2,1])/1.96)
age.par<-rnorm(300, mean=mu.age, sd=sigma)

sigma.int<-((LoblollySMA$coef[[1]][1,3]-LoblollySMA$coef[[1]][1,1])/1.96)
int.par<-rnorm(300, mean=mu.int, sd=sigma.int)
# cov.age<-vcov(LongleafSMA)
# age.par<-MASS::mvrnorm(n=300, mu=mu.age, Sigma = cov.age)

#Mortality Model
x<-c(4, 8, 12, 16, 20, 24, 50, 65, 70, 75, 80, 85, 90)

x2<-x^2

y<-c(.1, 0.05, .028, .017, .009, .0076, .0045, .0098, .018, .04, .089, .22, .44)

mort<-lm(y~x +x2)

#Sampling 00 parameters from multivariate normal dist for mortality model
mort.mu<-mort$coefficients
cov.mortmodel<-vcov(mort)
mort.par<-MASS::mvrnorm(n=300, mu=mort.mu, Sigma = cov.mortmodel)

#Added the SMA intercept to the intercept of env var model and kept age slope/intercept constant
initial.par<-cbind(growth.par,age.par, mort.par)
initial.par[1:300,1]<-initial.par[1:300,1]+ int.par

save(initial.par, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/initialpar3.22.21.Rdata")
