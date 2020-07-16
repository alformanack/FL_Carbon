# Run slash pine simulations ----------------------------------------------

rm(list=ls())

par(mfrow=c(2,4))
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
observed.a<-round((10^(3.104632 - 0.4283513*temp + 0.0102844*temp2 +0.581161*aridity))*(8.9^2.30799))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*1^(-0.5667225))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*age[i,j]^(-0.5667225))
      
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
      TASB [i,j]<-(0.041281*((Diameter[i,j]*2.54)^2.722214))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[18, which (Diameter[18,]>=5)])
  predict.tasb[o,1]<-sum(TASB[18, which (Diameter[18,]>=5)])*.5*1000*(1/10000)
}
observed.d<-c(7.243498)
observed.tasb<-c(2304.18856)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[18, which (Diameter[18,]>=5)], main = "1", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

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
observed.a<-round((10^(3.104632 - 0.4283513*temp + 0.0102844*temp2 +0.581161*aridity))*(4.5^2.30799))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-2286.27849

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*1^(-0.5667225))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*age[i,j]^(-0.5667225))
      
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
      TASB [i,j]<-(0.041281*((Diameter[i,j]*2.54)^2.722214))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  predict.d[h,1]<-mean(Diameter[4,])
  predict.tasb[h,1]<-sum(TASB[4,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(3.189155)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[4,], main = "12", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass", "sd.tasb", "sd.dbh")

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
observed.a<-round((10^(3.104632 - 0.4283513*temp + 0.0102844*temp2 +0.581161*aridity))*(16.4^2.30799))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-5751.31370

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*1^(-0.5667225))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*age[i,j]^(-0.5667225))
      
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
      TASB [i,j]<-(0.041281*((Diameter[i,j]*2.54)^2.722214))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  predict.d[h,1]<-mean(Diameter[78, which (Diameter[78,]>=5)])
  predict.tasb[h,1]<-sum(TASB[78, which (Diameter[78,]>=5)])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(9.863562)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[78, which (Diameter[78,]>=5)], main = "14", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass", "sd.tasb", "sd.dbh")

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
observed.a<-round((10^(3.104632 - 0.4283513*temp + 0.0102844*temp2 +0.581161*aridity))*(11.1^2.30799))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-6102.40018

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*1^(-0.5667225))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*age[i,j]^(-0.5667225))
      
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
      TASB [i,j]<-(0.041281*((Diameter[i,j]*2.54)^2.722214))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  predict.d[h,1]<-mean(Diameter[30, which (Diameter[30,]>=5)])
  predict.tasb[h,1]<-sum(TASB[30, which (Diameter[30,]>=5)])*.5*1000*(1/10000)
}  

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(8.878969)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[30, which (Diameter[30,]>=5)], main = "15", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass", "sd.tasb", "sd.dbh")

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
observed.a<-round((10^(3.104632 - 0.4283513*temp + 0.0102844*temp2 +0.581161*aridity))*(7.2^2.30799))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-1113.29871

for (h in 1:10){    
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*1^(-0.5667225))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*age[i,j]^(-0.5667225))
      
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
      TASB [i,j]<-(0.041281*((Diameter[i,j]*2.54)^2.722214))
      
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  predict.d[h,1]<-mean(Diameter[14,])
  predict.tasb[h,1]<-sum(TASB[14,])*.5*1000*(1/10000)
}  

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(3.616565)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[14,], main = "16", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass", "sd.tasb", "sd.dbh")

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
observed.a<-round((10^(3.104632 - 0.4283513*temp + 0.0102844*temp2 +0.581161*aridity))*(17.5^2.30799))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-3385.50500

for (h in 1:10){  
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*1^(-0.5667225))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*age[i,j]^(-0.5667225))
      
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
      TASB [i,j]<-(0.041281*((Diameter[i,j]*2.54)^2.722214))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  predict.d[h,1]<-mean(Diameter[91, which (Diameter[91,]>=5)])
  predict.tasb[h,1]<-sum(TASB[91, which (Diameter[91,]>=5)])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(8.594118)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[91, which (Diameter[91,]>=5)], main = "17", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass", "sd.tasb", "sd.dbh")

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
observed.a<-round((10^(3.104632 - 0.4283513*temp + 0.0102844*temp2 +0.581161*aridity))*(11.1^2.30799))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-2073.95522


for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*1^(-0.5667225))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.345167 + 0.185595*temp - 0.004456*temp2 - 0.251804*aridity))*(0.4332775*age[i,j]^(-0.5667225))
      
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
      TASB [i,j]<-(0.041281*((Diameter[i,j]*2.54)^2.722214))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  predict.d[h,1]<-mean(Diameter[43, which (Diameter[43,]>=5)])
  predict.tasb[h,1]<-sum(TASB[43, which (Diameter[43,]>=5)])*.5*1000*(1/10000)
  
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(7.419048)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[43, which (Diameter[43,]>=5)], main = "18", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 15
mylist[[15]] <- df


final_list <- do.call(rbind.data.frame, mylist)
row.names(final_list) <- c(1,12,14:18)
# final_list$Plot<- c(1,12,14:18)
# final_list$Plot<-as.factor(final_list$Plot)
sdev<-as.vector(final_list$sd.tasb)
sdev.dbh<-as.vector(final_list$sd.dbh)


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

plot(data = final_list, Observed_Diameter~Modeled_Diameter, xlim = c(0,22), ylim = c(0,22), xlab="Modeled DBH (in)", ylab="Observed DBH (in)",
     main = "Before parameter correction", col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Diameter~Modeled_Diameter, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Diameter-sdev.dbh, final_list$Observed_Diameter, final_list$Modeled_Diameter+sdev.dbh, final_list$Observed_Diameter, length=0.05, angle=90, code=3)

plot(data = final_list, Observed_Biomass~Modeled_Biomass, xlim = c(1000,6000), ylim = c(1000,6000), col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="Tree Carbon (gC/m^2)",
     col.axis="#027368", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Biomass-sdev, final_list$Observed_Biomass, final_list$Modeled_Biomass+sdev, final_list$Observed_Biomass, length=0.05, angle=90, code=3)
