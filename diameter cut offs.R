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
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(8.9^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[21, which (Diameter[21,]>=5)])
  predict.tasb[o,1]<-sum(TASB[21, which (Diameter[21,]>=5)])*.5*1000*(1/10000)
}
observed.d<-c(7.243498)
observed.tasb<-c(2304.18856)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[21, which (Diameter[21,]>=5)], main = "1", xlab = "Diameter (in)")

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
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(4.5^1.728844))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-2286.27849

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[h,1]<-mean(Diameter[6,])
  predict.tasb[h,1]<-sum(TASB[6,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(3.189155)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[6,], main = "12", xlab = "Diameter (in)")

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
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(16.4^1.728844))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-5751.31370

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[h,1]<-mean(Diameter[60, which (Diameter[60,]>=5)])
  predict.tasb[h,1]<-sum(TASB[60, which (Diameter[60,]>=5)])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(9.863562)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[60, which (Diameter[60,]>=5)], main = "14", xlab = "Diameter (in)")

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
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(11.1^1.728844))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-6102.40018

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(7.2^1.728844))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-1113.29871

for (h in 1:10){    
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[h,1]<-mean(Diameter[17,])
  predict.tasb[h,1]<-sum(TASB[17,])*.5*1000*(1/10000)
}  

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(3.616565)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[17,], main = "16", xlab = "Diameter (in)")

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
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(17.5^1.728844))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-3385.50500

for (h in 1:10){  
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[h,1]<-mean(Diameter[68, which (Diameter[68,]>=5)])
  predict.tasb[h,1]<-sum(TASB[68, which (Diameter[68,]>=5)])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(8.594118)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[68, which (Diameter[68,]>=5)], main = "17", xlab = "Diameter (in)")

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
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(11.1^1.728844))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-2073.95522


for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[h,1]<-mean(Diameter[39, which (Diameter[39,]>=5)])
  predict.tasb[h,1]<-sum(TASB[39, which (Diameter[39,]>=5)])*.5*1000*(1/10000)
  
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(7.419048)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[39, which (Diameter[39,]>=5)], main = "18", xlab = "Diameter (in)")

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

model.2<-lm(data = final_list, log10(Observed_Diameter/Modeled_Diameter)~Temperature2)
summary(model.3)

model.3<-lm(data = final_list, Modeled_Biomass~Observed_Biomass)

plot(data = final_list, Observed_Diameter~Modeled_Diameter, xlim = c(0,22), ylim = c(0,22), xlab="Modeled DBH (in)", ylab="Observed DBH (in)",
     main = "Before parameter correction", col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Diameter~Modeled_Diameter, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Diameter-sdev.dbh, final_list$Observed_Diameter, final_list$Modeled_Diameter+sdev.dbh, final_list$Observed_Diameter, length=0.05, angle=90, code=3)

plot(data = final_list, Observed_Biomass~Modeled_Biomass, xlim = c(1000,6000), ylim = c(1000,6000), col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="Tree Carbon (gC/m^2)",
     col.axis="#027368", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Biomass-sdev, final_list$Observed_Biomass, final_list$Modeled_Biomass+sdev, final_list$Observed_Biomass, length=0.05, angle=90, code=3)

# Correcting growth equation -------------------------------------------------

# before
growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))

#after
growth<-(10^(-2.5984 + 0.2145*temp - 0.0044*temp2 - 0.2243*aridity))*(0.8239*age[i,j]^(-0.1761))

# Run slash pine simulations ----------------------------------------------

rm(list=ls())

par(mfrow=c(2,4))
final_list<-list()

# Jokela ----------------------------------------------

# data will be saved as list 1
mylist<-list() 


# set parameters for growth equation

# set parameters for growth equation
aridity<-8139*0.0001

temp<-20.76

temp2<-temp^2

C_AVG<-((538/10)*(1/3))+((194/10)*(2/3))

N_AVG<-((1780/100)*(1/3))+((887/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-946
observed.a<-25
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[25,])
  predict.tasb[o,1]<-sum(TASB[25,])*.5*1000*(1/10000)
}
observed.d<-c(8)
observed.tasb<-c(NA)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[25,], main = "Jokela", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 1
mylist[[1]] <- df


# Doren 1993  ----------------------------------------------

# set parameters for growth equation
aridity<-6962*0.0001

temp<-23.83206

temp2<-temp^2

C_AVG<-((1090/10)*(1/3))+((1080/10)*(2/3))

N_AVG<-((1483/100)*(1/3))+((1100/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE_LOP<-(CN*4.941)+29.777


# set stand age and density
plot_density<-c(673)
observed.a<-c(200)
round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(18.7^1.728844))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-NA

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[h,1]<-mean(Diameter[200,which(Diameter[200,]>.732499)]) # trees over 1.5m tall
  predict.tasb[h,1]<-sum(TASB[200,which(Diameter[200,]>.732499)])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(4.133858)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[200,which(Diameter[200,]>.732499)], main = "Doren 1993", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 9
mylist[[9]] <- df  


# Sharma 2019 simultion (Hells state forest) ----------------------------------------------

# set parameters for growth equation
aridity<-9452*.0001

temp<-19.85

temp2<-temp^2

C_AVG<-((592/10)*(1/3))+((189/10)*(2/3))

N_AVG<-((1235/100)*(1/3))+((652/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-c(775)
observed.a<-c(32)
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-NA

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[h,1]<-mean(Diameter[32,])
  predict.tasb[h,1]<-sum(TASB[32,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(8.6)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[32,], main = "Sharma 2019", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 11
mylist[[11]] <- df 

# Bohn 2016 ----------------------------------------------

# set parameters for growth equation
aridity<-9298*.0001

temp<-20.04

temp2<-temp^2

C_AVG<-((675/10)*(1/3))+((260/10)*(2/3))

N_AVG<-((1309/100)*(1/3))+((750/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-c(1180)
observed.a<-c(30)
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-NA

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[h,1]<-sqrt(sum(Diameter[30,]^2)/1180) #converting to QMD
  predict.tasb[h,1]<-sum(TASB[30,])*.5*1000*(1/10000)
}  

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(7.08661) #QMD 18cm
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[30,], main = "Bohn 2016", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 12
mylist[[12]] <- df

# Hedman 1999 ----------------------------------------------

# set parameters for growth equation
aridity<-8762*.0001

temp<-19.10

temp2<-temp^2

C_AVG<-((529/10)*(1/3))+((205/10)*(2/3))

N_AVG<-((504/100)*(1/3))+((197/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-c(529)
observed.a<-c(49)
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-NA

for (h in 1:10){    
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[h,1]<-mean(Diameter[49,which(Diameter[49,]>=0.984252)])
  predict.tasb[h,1]<-sum(TASB[49,which(Diameter[49,]>=0.984252)])
}  

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(7.874)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[49,which(Diameter[49,]>=0.984252)], main = "Hedman 1999", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 13
mylist[[13]] <- df

# Gholz 1982 ----------------------------------------------

# set parameters for growth equation
Lawtey<-8244.150862068966
Starke<-8203.438775510203
Hampton<-8100.927419354839
Brooker<-8155.30223880597

aridity<-((Lawtey + Starke + Hampton + Brooker)/4)*0.0001

temp<-20.05

temp2<-temp^2

C_AVG<-((413.3392/10)*(1/3))+((148.5920/10)*(2/3))

N_AVG<-((1532.3486/100)*(1/3))+((389.9631/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-976
observed.a<-34
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-NA

for (h in 1:10){  
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[h,1]<-mean(Diameter[34, which (Diameter[34,]>=5)])
  predict.tasb[h,1]<-sum(TASB[34, which (Diameter[34,]>=5)])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(6.456693)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[34, which (Diameter[34,]>=5)], main = "Gholz 1982", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 14
mylist[[14]] <- df


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

plot(data = final_list, Observed_Diameter~Modeled_Diameter, xlim=c(1,11), ylim = c(1,11), xlab="Modeled DBH (in)", ylab="Observed DBH (in)",
     main = "Before parameter correction", col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Diameter~Modeled_Diameter, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Diameter-sdev.dbh, final_list$Observed_Diameter, final_list$Modeled_Diameter+sdev.dbh, final_list$Observed_Diameter, length=0.05, angle=90, code=3)

plot(data = final_list, Observed_Biomass~Modeled_Biomass, ylim = c(0,1000), xlim = c(0,1000), col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="Tree Carbon (gC/m^2)",
     col.axis="#027368", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Biomass-sdev, final_list$Observed_Biomass, final_list$Modeled_Biomass+sdev, final_list$Observed_Biomass, length=0.05, angle=90, code=3)

# Calculating total above stump C biomass for year 2100 for 700 tph by region

Panhandle<-vector()
North<-vector()
Central<-vector()
South<-vector()

par(mfrow=c(2,2))

# Panhandle TASB ----------------------------------------------------------

temp<-19.84

temp2<-temp^2

aridity<-9768*.0001

# CN scaled with FIA data

C_AVG<-(539/10)*(1/3)+((165/10)*(2/3))

N_AVG<-(1297/100)*(1/3)+((527/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777

for (b in 1:10){  
  age<-matrix(0, nrow=200, ncol=700) # initialize the age matrix
  Diameter<-matrix(0, nrow=200, ncol=700) # initialize the diameter matrix
  TASB<-matrix(0, nrow=200, ncol=700) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:80){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  Panhandle[b]<-sum(TASB[80,])*.5*1000*(1/10000)
}

hist(Diameter[80,]*2.54, main = "Panhandle", xlab = "Diameter (cm)")

# Calculating North TASB --------------------------------------------------

temp<-20.30

temp2<-temp^2

aridity<-8212*.0001

# CN scaled with FIA data
C_AVG<-((317/10)*(1/3))+((110/10)*(2/3))

N_AVG<-((1665/100)*(1/3))+((300/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777

for (b in 1:10){  
  age<-matrix(0, nrow=200, ncol=700) # initialize the age matrix
  Diameter<-matrix(0, nrow=200, ncol=700) # initialize the diameter matrix
  TASB<-matrix(0, nrow=200, ncol=700) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:80){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on Diameter2 class
      if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
      else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
      else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
      else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
      else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
      
      # Calculate the Diameter2 for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
     TASB [i,j]<-(0.041281*((Diameter[i,j]*2.54)^2.722214))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  North[b]<-sum(TASB[80,])*.5*1000*(1/10000)
}

hist(Diameter[80,]*2.54, main = "North", xlab = "Diameter (cm)")

# TASB Central ------------------------------------------------------------

temp<-22.29

temp2<-temp^2

aridity<-7259*.0001

# CN scaled with FIA data
C_AVG<-((475/10)*(1/3))+((144/10)*(2/3))

N_AVG<-((1729/100)*(1/3))+((1137/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777

for (b in 1:10){  
  
  age<-matrix(0, nrow=200, ncol=700) # initialize the age matrix
  Diameter<-matrix(0, nrow=200, ncol=700) # initialize the diameter matrix
  TASB<-matrix(0, nrow=200, ncol=700) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:80){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  Central[b]<-sum(TASB[80,])*.5*1000*(1/10000)
}

hist(Diameter[80,]*2.54, main = "Central", xlab = "Diameter (cm)")

# South TASB --------------------------------------------------------------

temp<-23.80

temp2<-temp^2

aridity<-7305*.0001

# CN scaled with FIA data
C_AVG<-((1052/10)*(1/3))+((1017/10)*(2/3))

N_AVG<-((1721/100)*(1/3))+((1289/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777

for (b in 1:10){  
  
  age<-matrix(0, nrow=200, ncol=700) # initialize the age matrix
  Diameter<-matrix(0, nrow=200, ncol=700) # initialize the diameter matrix
  TASB<-matrix(0, nrow=200, ncol=700) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:80){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  South[b]<-sum(TASB[80,])*.5*1000*(1/10000)
}

hist(Diameter[80,]*2.54, main = "South", xlab = "Diameter (cm)")



TASB_Panhandle<-c(mean(Panhandle), sd(Panhandle))
TASB_North<-c(mean(North), sd(North))
TASB_Central<-c(mean(Central), sd(Central))
TASB_South<-c(mean(South), sd(South))
table<-rbind(TASB_Panhandle, TASB_North, TASB_Central, TASB_South)
