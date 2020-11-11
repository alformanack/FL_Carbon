rm(list=ls())

C_AVG<-(485/10)*(1/3)+((151/10)*(2/3))

N_AVG<-(1722/100)*(1/3)+((876/100)*(2/3))

CN<-C_AVG/N_AVG
CN_SCALE<-(CN*6.045)+19.775

temp<-20.76
aridity<-8139*0.0001

age<-matrix(0, nrow=25, ncol=946)
Diameter<-matrix(0, nrow=25, ncol=946)

Diameter[1,]<-(10^(0.8156395 - 0.004348023*temp - 0.2872396*aridity))*(0.3535108*1^(-0.6464892))


for (j in 1:946){
  
  
  for (i in 2:25){
    age[i,j]<-age[i-1,j]+1
    growth<-(10^(0.8156395 - 0.004348023*temp - 0.2872396*aridity))*(0.3535108*age[i,j]^(-0.6464892))

    
    
    if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
    else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
    else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
    else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
    else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}

    
    Diameter[i,j]<-Diameter[i-1,j]+growth-M*(Diameter[i-1,j]+growth)
    
    #TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    if (M==1){
      age[i,j]<-0
    }
  }}

obs.DIA<-(c(4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5))
TPA<-(c(46.54374758, 45.88559814, 70.57225126, 96.30299071, 60.92140921, 35.95407472, 20.71259195, 6.164343786))

mean(obs.DIA)

barplot(TPA~obs.DIA)

mean(Diameter[25,])


# simulation with errror correction -----------

rm(list=ls())
temp<-20.76
aridity<-8139*0.0001

C_AVG<-(485/10)*(1/3)+((151/10)*(2/3))

N_AVG<-(1722/100)*(1/3)+((876/100)*(2/3))


CN<-C_AVG/N_AVG
CN_SCALE<-(CN*6.045)+19.775



age<-matrix(0, nrow=25, ncol=946)
Diameter<-matrix(0, nrow=25, ncol=946)


Diameter[1,]<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*1^(-0.114599))

for (j in 1:946){
  
  
  for (i in 2:25){
    age[i,j]<-age[i-1,j]+1
    growth<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*age[i,j]^(-0.114599))

    
    
    if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
    else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
    else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
    else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
    else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
    

    
    Diameter[i,j]<-Diameter[i-1,j]+growth-M*(Diameter[i-1,j]+growth)
    
    #TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    if (M==1){
      age[i,j]<-0
    }
  }}

obs.DIA<-(c(4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5))
TPA<-(c(46.54374758, 45.88559814, 70.57225126, 96.30299071, 60.92140921, 35.95407472, 20.71259195, 6.164343786))
mean(Diameter[25,])

barplot(TPA~obs.DIA)

mean(Diameter[25,])
