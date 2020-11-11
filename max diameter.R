# Run slash pine simulations ----------------------------------------------

rm(list=ls())

par(mfrow=c(2,8))
final_list<-list()

# Plot 1 ----------------------------------------------

# data will be saved as list 1
mylist<-list() 


# set parameters for growth equation
aridity<-6957*0.0001

temp<-22.34389687

temp2<-temp^2

C_AVG<-((725/10)*(1/3))+((760/10)*(2/3))

N_AVG<-((1375/100)*(1/3))+((923/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-379
observed.a<-round((10^.40808)*(8.9^1.06166))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  # save average modeled diameter
  predict.d[o,1]<-max(Diameter[26,])
  predict.tasb[o,1]<-sum(TASB[26,])*.5*1000*(1/10000)
}
observed.d<-c(8.9)
observed.tasb<-c(254.483096)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[26,], main = "1", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN", "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 1
mylist[[1]] <- df


# Plot 12 ----------------------------------------------

# set parameters for growth equation
aridity<-6932*0.0001

temp<-22.33102036

temp2<-temp^2

C_AVG<-((366/10)*(1/3))+((427/10)*(2/3))

N_AVG<-((1137/100)*(1/3))+((628/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-3209
observed.a<-round((10^.40808)*(4.5^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-270.982210

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-max(Diameter[13,])
  predict.tasb[h,1]<-sum(TASB[13,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(4.5)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[13,], main = "12", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 9
mylist[[9]] <- df  


# Plot 14 ----------------------------------------------

# set parameters for growth equation
aridity<-7595*0.0001

temp<-20.97239685

temp2<-temp^2

C_AVG<-((475/10)*(1/3))+((227/10)*(2/3))

N_AVG<-((2085/100)*(1/3))+((1689/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-349
observed.a<-round((10^.40808)*(16.4^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-609.471481

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-max(Diameter[50])
  predict.tasb[h,1]<-sum(TASB[50,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(16.4)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[50,], main = "14", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 11
mylist[[11]] <- df 

# Plot 15 ----------------------------------------------

# set parameters for growth equation
aridity<-6941*0.0001

temp<-22.33102036

temp2<-temp^2

C_AVG<-((486/10)*(1/3))+((475/10)*(2/3))

N_AVG<-((1238/100)*(1/3))+((827/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-585
observed.a<-round((10^.40808)*(11.1^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-661.599248

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-max(Diameter[33,])
  predict.tasb[h,1]<-sum(TASB[33,])*.5*1000*(1/10000)
}  

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(11.1)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[33,], main = "15", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 12
mylist[[12]] <- df

# Plot 16 ----------------------------------------------

# set parameters for growth equation
aridity<-8774*0.0001

temp<-19.62741661

temp2<-temp^2

C_AVG<-((972/10)*(1/3))+((295/10)*(2/3))

N_AVG<-((1585/100)*(1/3))+((505/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-793
observed.a<-round((10^.40808)*(7.2^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-126.913553

for (h in 1:10){    
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-max(Diameter[21,])
  predict.tasb[h,1]<-sum(TASB[21,])*.5*1000*(1/10000)
}  

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(7.2)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[21,], main = "16", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 13
mylist[[13]] <- df

# Plot 17 ----------------------------------------------

# set parameters for growth equation
aridity<-7687*0.0001

temp<-20.97239685

temp2<-temp^2

C_AVG<-((405/10)*(1/3))+((182/10)*(2/3))

N_AVG<-((2095/100)*(1/3))+((1702/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-253
observed.a<-round((10^.40808)*(17.5^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-357.394052

for (h in 1:10){  
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-max(Diameter[53,])
  predict.tasb[h,1]<-sum(TASB[53,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(17.5)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[53,], main = "17", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 14
mylist[[14]] <- df

# Plot 18 ----------------------------------------------

# set parameters for growth equation
aridity<-9677*0.0001

temp<-19.52181244

temp2<-temp^2

C_AVG<-((515/10)*(1/3))+((143/10)*(2/3))

N_AVG<-((1366/100)*(1/3))+((454/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-312
observed.a<-round((10^.40808)*(11.1^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-227.810385


for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-max(Diameter[33,])
  predict.tasb[h,1]<-sum(TASB[33,])*.5*1000*(1/10000)
  
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(11.1)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[33,], main = "18", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 15
mylist[[15]] <- df


final_list <- do.call(rbind.data.frame, mylist)
# Compare observed diameter vs modeled diameter ---------------------------
par(mfrow=c(1,1))

model.1<-lm(data = final_list, log10(Observed_Diameter/Modeled_Diameter)~log10(Age)+Tree_Density+Temperature+Temperature2+Aridity)
summary(model.1)
# plot(Temperature~Aridity, data= final_list[[1]])
# 
# res.age<-residuals(model.1)
# plot(res.age~df2$Age)
# plot(res.age~df2$Aridity)

model.2<-lm(data = final_list, log10(Observed_Diameter/Modeled_Diameter)~log10(Age))
summary(model.2)

plot(data = final_list, Observed_Diameter~Modeled_Diameter, xlim = c(2,12), xlab="Modeled", ylab="Observed",
     col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")

plot(data = final_list, Observed_Biomass~Modeled_Biomass, ylim = c(0,1000), xlab="Modeled", ylab="Observed", main ="Tree Carbon (gC/m^2)",
     col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF") 

# Correcting growth equation -------------------------------------------------

# before
growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))

#after
growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
