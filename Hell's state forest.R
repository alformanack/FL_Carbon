
rm(list=ls())

# Hells state forest, panhandle simulation --------------------------------

temp1<-19.85
aridity1<-9452*.0001

# CN scaled with FIA data
C_AVG<-(667/10)*(1/3)+((160/10)*(2/3))

N_AVG<-(1217/100)*(1/3)+((491/100)*(2/3))

CN<-C_AVG/N_AVG
CN_SCALE_Hells<-(CN*6.045)+19.775



age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(0.8156395 - 0.004348023*temp1 - 0.2872396*aridity1))*(0.3535108*1^(-0.6464892))



for (j in 1:775){ # specify tree per hectare
  
  
  
  for (i in 2:32){ # specify how long to run the simulation (years)
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(0.8156395 - 0.004348023*temp1 - 0.2872396*aridity1))*(0.3535108*age[i,j]^(-0.6464892))
    
    # define the mortality rate here
    # initialize as a numeric with only 1 possible value
    M <- numeric(length = 1)
    
    # Mortality based on diameter class
    if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
    else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
    else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
    else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
    else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
    
    # Calculate the diameter for jth tree for the ith observed year
    Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
    
    #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
    TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    # If the tree dies, plant a new tree (age = 0)
    if (M==1){age[i,j]<-0}
    
  }
}

mean(Diameter[32,])
hist(Diameter[32,])

