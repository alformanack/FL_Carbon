age<-seq(1,100,1)

par(mfrow=c (1,2))

dbh <- (0.384*sqrt((age+ 0.375))+ 1.486)^2
plot(dbh~age)


dbh_m<-(10^(.381+(.0012*25)+(.346*log10(age)+(-.0069*log10(60)+(-.0001*350)))))*2.54
lines(age,dbh_m)

dbh_2<-(0.523*sqrt((age +0.375)) + 0.798)^2
lines(age,dbh_2, col="red")

#sitetree<-mutate(sitetree, SOILGRIDS_C_AVG=((Soilgrids_C_0.5/10)*(1/3))+((Soilgrids_C_5.15/10)*(2/3)))
C_avg<-(506.48/10)*(1/3)+((184.81/10)*(2/3))

#sitetree<-mutate(sitetree, SOILGRIDS_N_AVG=((Soilgrids_N_0.5/100)*(1/3))+((Soilgrids_N_5.15/100)*(2/3)))                 
N_AVG<-(1347.68/100)*(1/3)+((638.51/100)*(2/3))                 

#sitetree<-mutate(sitetree, SOILGRIDS_CN=SOILGRIDS_C_AVG/SOILGRIDS_N_AVG)  
CN<-C_avg/N_AVG 

CN_SCALE<-(CN*6.045)+19.775

dbh_m_1<-(10^(.381+(.0012*20.05)+(.346*log10(age)+(-.0069*log10(CN_SCALE)+(-.0001*266.82)))))*2.54

hist(dbh_m_1)


rm(list=ls())

age<-matrix(0, nrow=500, ncol=700)
Diameter<-matrix(0, nrow=500, ncol=700)
TASB<-matrix(0, nrow = 500, ncol = 700)

for (j in 1:700){
  
  for (i in 2:500){
    age[i,j]<-age[i-1,j]+1
    growth<-(10^(.381+.0012*22-.0001*250))*(.346*age[i,j]^(-.654))*60^(-.0069)
    #  M<-rbinom(1,1,0.0083*age[i,j])
    # M<-rbinom(1,1,min(0.4,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))))
    #M<-rbinom(1,1,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))/8)
    
    
    
    if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.2/8))}
    else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.15/8))}
    else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.1/8))}
    else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.09/8))}
    else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.08/8))}
    
    #else {M<-rbinom(1,1,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))/8)}
    #if (7.97<Diameter<=11.81){M<- rbinom(1,1,(.15/8))},
    #if (11.81<Diameter<=15.75){M<- rbinom(1,1,(.1/8))},
    
    Diameter[i,j]<-Diameter[i-1,j]+growth-M*(Diameter[i-1,j]+growth)
    
    TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    if (M==1){
      age[i,j]<-0
    }
  }}

hist(age[500,which(Diameter[500,]>=5)])
hist(Diameter[500,which(Diameter[500,]>=5)])
hist(age)


Obs_diameter<-rnorm(264, mean=7.5, sd=2.67)
qqplot(Obs_diameter, Diameter)


obs_age<-rnorm(70, mean=27, sd=61)
qqplot(obs_age, age, xlim=c(0,170), ylim=c(0,170))
qqplot(obs_age, age)
mean(age)
max(age)
qqplot(trunc_obs_age, age)


hist(log(age[500,which(Diameter[500,]>=5)]))
hist(Diameter[500,which(Diameter[500,]>=5)])

library(truncnorm)
trunc_obs_age<-rtruncnorm(n=70, a=0, b=72, mean=27, sd=61)
hist(obs_age)
hist(trunc_obs_age)
hist(age)
hist(Diameter)
hist(Obs_diameter)

log_mean<-2*log(27)-0.5*log((61^2)+(27^2))
log_sd<-sqrt(-2*log(27)+log((61^2)+(27^2)))
log_obs_age<-rnorm(500, log_mean, log_sd)
hist(exp(log_obs_age))
hist(log_obs_age)                   
trunc<-rtruncnorm(n=200, a=log(10), b=log(72), mean= log_mean, sd=log_sd)
hist(trunc)
