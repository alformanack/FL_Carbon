rm(list=ls())

age<-matrix(0, nrow=500, ncol=700)
Diameter<-matrix(0, nrow=500, ncol=700)
TASB<-matrix(0, nrow = 500, ncol = 700)

# age[2,1]<-age[1,1]+1
# 
# Diameter[2,1]<-Diameter[1,1]+growthage[2]-M(Diameter[1,1]+growthage[2])
# Diameter[3,1]<-Diameter[2,1]+growthrate[3]-M(Diameter[2,1]+growthage[3])


for (i in 2:500){
  age[i,1]<-age[i-1,1]+1
  growth<-(10^(.381+.0012*22-.0001*250))*(.346*age[i,1]^(-.654))*60^(-.0069)
  M<-rbinom(1,1,0.0083*age[i,1])
  Diameter[i,1]<-Diameter[i-1,1]+growth-M*(Diameter[i-1,1]+growth)
  if (M==1){
    age[i,1]<-0
  }
}

plot(Diameter)
plot(age)

M <- as.numeric()

for (j in 1:700){

for (i in 2:500){
  age[i,j]<-age[i-1,j]+1
  growth<-(10^(.381+.0012*22-.0001*250))*(.346*age[i,j]^(-.654))*60^(-.0069)
#  M<-rbinom(1,1,0.0083*age[i,j])
 # M<-rbinom(1,1,min(0.4,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))))
  #M<-rbinom(1,1,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))/8)
  
 
  
  if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
  else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
  else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
  else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
  else {M<-rbinom(1,1,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))/8)}
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

              