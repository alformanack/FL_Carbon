
rm(list=ls())

# Panhandle TASB ----------------------------------------------------------

temp1<-19.84
aridity1<-9768*.0001
  
  
age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(-4.97836 - 0.004348023*temp1 + 6.06674*aridity1))*(0.885401*1^(-0.114599))
Diameter2[1,]<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*1^(-0.114599))


for (j in 1:700){ # specify tree per hectare
  
  
  
  for (i in 2:80){ # specify how long to run the simulation (years)
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*age[i,j]^(-0.114599))
    
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
Panhandle.Cgperm2<-sum(TASB[80,])*.5*1000*(1/100000)
mean(Diameter[80,])
hist(Diameter[80,])

# Calculating North TASB --------------------------------------------------

temp<-20.30
aridity<-8212*.0001


age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
Diameter2<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
TASB2<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix

# initialize the diameter for the first year
Diameter2[1,]<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*1^(-0.114599))




for (j in 1:700){ # specify tree per hectare
  
  
  
  for (i in 2:80){ # specify how long to run the simulation (years)
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*age[i,j]^(-0.114599))
    
    # define the mortality rate here
    # initialize as a numeric with only 1 possible value
    M <- numeric(length = 1)
    
    # Mortality based on Diameter2 class
    if (Diameter2[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
    else if (Diameter2[i,j]>3.94 & Diameter2[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
    else if (Diameter2[i,j]>7.97 & Diameter2[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
    else if (Diameter2[i,j]>11.81 & Diameter2[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
    else if (Diameter2[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
    
    # Calculate the Diameter2 for jth tree for the ith observed year
    Diameter2[i,j]<-Diameter2[i-1,j] + growth - M*(Diameter2[i-1,j]+growth)
    
    #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
    TASB2 [i,j]<-0.0578*(Diameter2[i,j]^2.41)*(age[i,j]^.204)
    
    # If the tree dies, plant a new tree (age = 0)
    if (M==1){age[i,j]<-0}
    
  }
}
North.Cgperm2<-sum(TASB2[80,])*.5*1000*(1/100000)
mean(Diameter2[80,])
hist(Diameter2[80,])

# TASB Central ------------------------------------------------------------


temp<-22.29
aridity<-7259*.0001
  
  
age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*1^(-0.114599))


for (j in 1:700){ # specify tree per hectare
  
  
  
  for (i in 2:80){ # specify how long to run the simulation (years)
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*age[i,j]^(-0.114599))
    
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
Central.Cgperm2<-sum(TASB[80,])*.5*1000*(1/100000)
hist(Diameter[80,])

# South TASB --------------------------------------------------------------

temp<-23.80
aridity<-7305*.0001
  
  
age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*1^(-0.114599))


for (j in 1:700){ # specify tree per hectare
  
  
  
  for (i in 2:80){ # specify how long to run the simulation (years)
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*age[i,j]^(-0.114599))
    
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
South.Cgperm2<-sum(TASB[80,])*.5*1000*(1/100000)
hist(Diameter[80,])
