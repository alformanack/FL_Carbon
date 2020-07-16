C_AVG<-(506.48/10)*(1/3)+((184.81/10)*(2/3))

N_AVG<-(1347.68/100)*(1/3)+((638.51/100)*(2/3))                 

CN<-C_AVG/N_AVG 
CN_SCALE<-(CN*6.045)+19.775

d <- matrix()

for(t in 1:3) {

age<-matrix(0, nrow=observed.a[t], ncol=plot_density[t])
Diameter<-matrix(0, nrow=observed.a[t], ncol=plot_density[t])
#Diameter[1,]<-(10^(.381+.0012*20.05-.0001*266.82))*(.346*1^(-.654))*CN_SCALE^(-.0069)
#TASB<-matrix(0, nrow = 72, ncol = 700)

par(mfrow=c(1,2))


# 700 = number of trees = plot_density[i] 
# 72 = observed.a[i]



for (j in 1:plot_density[t]){
  
  Diameter[1,]<-(10^(.381+.012*20.05-.0001*266.82))*(.346*1^(-.654))*CN_SCALE^(-.0069)
  
  for (i in 2:observed.a[t]){
    age[i,j]<-age[i-1,j]+1
    growth<-(10^(.381+.012*20.05-.0001*266.82))*(.346*age[i,j]^(-.654))*CN_SCALE^(-.0069)
    #growth<-(10^(.381+.0012*20.05-.0001*266.82))*(.346*age[i,j]^(-.4))*CN_SCALE^(-.0069)
    #growth<-growth*1.3
    #  M<-rbinom(1,1,0.0083*age[i,j])
    # M<-rbinom(1,1,min(0.4,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))))
    #M<-rbinom(1,1,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))/8)
    
    
    
    if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
    else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
    else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
    else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
    else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
    
    #else {M<-rbinom(1,1,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))/8)}
    #if (7.97<Diameter<=11.81){M<- rbinom(1,1,(.15/8))},
    #if (11.81<Diameter<=15.75){M<- rbinom(1,1,(.1/8))},
    
    Diameter[i,j]<-Diameter[i-1,j]+growth-M*(Diameter[i-1,j]+growth)
    
    #TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    if (M==1){
      age[i,j]<-0
    }
  }}

}

# take mean Diameter at the last age (last row, for all trees)
# mean(Diameter[72,])

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
trunc<-rtruncnorm(n=72, a=log(10), b=log(72), mean= log_mean, sd=log_sd)
hist(trunc)

log_mean_d<-2*log(7.5)-0.5*log((2.67^2)+(7.5^2))
log_sd_d<-sqrt(-2*log(7.5)+log((2.67^2)+(7.5^2)))
log_obs_d<-rnorm(72, log_mean_d, log_sd_d)
hist(exp(log_obs_age))
hist(log_obs_age)                   
trunc_d<-rtruncnorm(n=200, a=log(5), b=log(24.3), mean= log_mean_d, sd=log_sd_d)
hist(trunc_d)


hist(log(age[72,which(Diameter[72,]>=5)]))
hist(trunc)
mean(age[72, which(Diameter[72,]>=5)])
mean(log(age[72, which(Diameter[72,]>=5)]))
mean(trunc)
hist(log(Diameter[72,which(Diameter[72,]>=5)]))

log_mean_d_model<- mean(log(Diameter[72,which(Diameter[72,]>=5)]))
log_sd_d_model<-sd(log(Diameter[72,which(Diameter[72,]>=5)]))
trunc_d_model<-rtruncnorm(n=200, a=log(5), b=log(24.3), mean= log_mean_d_model, sd=log_sd_d_model)
qqplot(trunc_d, trunc_d_model)
abline(0,1)

log_mean_a_model<- mean(log(age[72,which(age[72,]>=5)]))
log_sd_a_model<-sd(log(age[72,which(age[72,]>=5)]))
trunc_a_model<-rtruncnorm(n=200, a=log(10), b=log(72), mean= log_mean_a_model, sd=log_sd_a_model)
qqplot(exp(trunc), (age[72,which(Diameter[72,]>=5)]))
abline(0,1)
