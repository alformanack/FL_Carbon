# Run slash pine simulations ----------------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
envdata<-read.csv("slash_env_data.csv", header=T, sep=",")

par(mfrow=c(2,5))
final_list_slash<-list()

# Plot 170 ----------------------------------------------

# data will be saved as list 1
mylist<-list() 


# set parameters for growth equation
aridity<-envdata[1,15]*0.0001

temp<-envdata[1,10]

temp2<-envdata[1,10]^2

C_AVG<-((envdata[1,11]/10)*(1/3))+((envdata[1,12]/10)*(2/3))

N_AVG<-((envdata[1,13]/100)*(1/3))+((envdata[1,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[1,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[1,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[21, ])*.5*1000*(1/10000)
}
observed.d<-c(envdata[1,3])
observed.tasb<-c(envdata[1,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "170", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 1
mylist[[1]] <- df


# Plot 196 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[2,15]*0.0001

temp<-envdata[2,10]

temp2<-envdata[2,10]^2

C_AVG<-((envdata[2,11]/10)*(1/3))+((envdata[2,12]/10)*(2/3))

N_AVG<-((envdata[2,13]/100)*(1/3))+((envdata[2,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[2,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[2,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a, ])*.5*1000*(1/10000)
}
observed.d<-c(envdata[2,3])
observed.tasb<-c(envdata[2,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "196", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 2
mylist[[2]] <- df

# Plot 476 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[3,15]*0.0001

temp<-envdata[3,10]

temp2<-envdata[3,10]^2

C_AVG<-((envdata[3,11]/10)*(1/3))+((envdata[3,12]/10)*(2/3))

N_AVG<-((envdata[3,13]/100)*(1/3))+((envdata[3,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[3,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[3,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a, ])*.5*1000*(1/10000)
}
observed.d<-c(envdata[3,3])
observed.tasb<-c(envdata[3,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "476", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 3
mylist[[3]] <- df

# Plot 669 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[4,15]*0.0001

temp<-envdata[4,10]

temp2<-envdata[4,10]^2

C_AVG<-((envdata[4,11]/10)*(1/3))+((envdata[4,12]/10)*(2/3))

N_AVG<-((envdata[4,13]/100)*(1/3))+((envdata[4,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[4,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[4,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a, ])*.5*1000*(1/10000)
}
observed.d<-c(envdata[4,3])
observed.tasb<-c(envdata[4,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "669", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 4
mylist[[4]] <- df

# Plot 771 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[5,15]*0.0001

temp<-envdata[5,10]

temp2<-envdata[5,10]^2

C_AVG<-((envdata[5,11]/10)*(1/3))+((envdata[5,12]/10)*(2/3))

N_AVG<-((envdata[5,13]/100)*(1/3))+((envdata[5,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[5,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[5,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a, ])*.5*1000*(1/10000)
}
observed.d<-c(envdata[5,3])
observed.tasb<-c(envdata[5,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "771", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 5
mylist[[5]] <- df

# Plot 1027 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[6,15]*0.0001

temp<-envdata[6,10]

temp2<-envdata[6,10]^2

C_AVG<-((envdata[6,11]/10)*(1/3))+((envdata[6,12]/10)*(2/3))

N_AVG<-((envdata[6,13]/100)*(1/3))+((envdata[6,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[6,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[6,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a, which(Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[6,3])
observed.tasb<-c(envdata[6,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a, which(Diameter[observed.a,]>=5)], main = "1027", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 6
mylist[[6]] <- df

# Plot 1070 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[7,15]*0.0001

temp<-envdata[7,10]

temp2<-envdata[7,10]^2

C_AVG<-((envdata[7,11]/10)*(1/3))+((envdata[7,12]/10)*(2/3))

N_AVG<-((envdata[7,13]/100)*(1/3))+((envdata[7,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[7,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[7,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[7,3])
observed.tasb<-c(envdata[7,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "1070", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 7
mylist[[7]] <- df

# Plot 1766 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[8,15]*0.0001

temp<-envdata[8,10]

temp2<-envdata[8,10]^2

C_AVG<-((envdata[8,11]/10)*(1/3))+((envdata[8,12]/10)*(2/3))

N_AVG<-((envdata[8,13]/100)*(1/3))+((envdata[8,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[8,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[8,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a, which(Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a, ])*.5*1000*(1/10000)
}
observed.d<-c(envdata[8,3])
observed.tasb<-c(envdata[8,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a, which(Diameter[observed.a,]>=5)], main = "1766", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 8
mylist[[8]] <- df

# Plot 1848 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[9,15]*0.0001

temp<-envdata[9,10]

temp2<-envdata[9,10]^2

C_AVG<-((envdata[9,11]/10)*(1/3))+((envdata[9,12]/10)*(2/3))

N_AVG<-((envdata[9,13]/100)*(1/3))+((envdata[9,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[9,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[9,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[9,3])
observed.tasb<-c(envdata[9,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "1848", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 9
mylist[[9]] <- df

# Plot 1869 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[10,15]*0.0001

temp<-envdata[10,10]

temp2<-envdata[10,10]^2

C_AVG<-((envdata[10,11]/10)*(1/3))+((envdata[10,12]/10)*(2/3))

N_AVG<-((envdata[10,13]/100)*(1/3))+((envdata[10,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[10,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[10,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a, which(Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[10,3])
observed.tasb<-c(envdata[10,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a, which(Diameter[observed.a,]>=5)], main = "1869", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 10
mylist[[10]] <- df

# Plot 1984 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[11,15]*0.0001

temp<-envdata[11,10]

temp2<-envdata[11,10]^2

C_AVG<-((envdata[11,11]/10)*(1/3))+((envdata[11,12]/10)*(2/3))

N_AVG<-((envdata[11,13]/100)*(1/3))+((envdata[11,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[11,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[11,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[11,3])
observed.tasb<-c(envdata[11,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "1984", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 11
mylist[[11]] <- df

# Plot 2022 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[12,15]*0.0001

temp<-envdata[12,10]

temp2<-envdata[12,10]^2

C_AVG<-((envdata[12,11]/10)*(1/3))+((envdata[12,12]/10)*(2/3))

N_AVG<-((envdata[12,13]/100)*(1/3))+((envdata[12,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[12,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[12,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[12,3])
observed.tasb<-c(envdata[12,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "2022", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 12
mylist[[12]] <- df

# Plot 2243 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[13,15]*0.0001

temp<-envdata[13,10]

temp2<-envdata[13,10]^2

C_AVG<-((envdata[13,11]/10)*(1/3))+((envdata[13,12]/10)*(2/3))

N_AVG<-((envdata[13,13]/100)*(1/3))+((envdata[13,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[13,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[13,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[13,3])
observed.tasb<-c(envdata[13,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "2243", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 13
mylist[[13]] <- df

# Plot 2309 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[14,15]*0.0001

temp<-envdata[14,10]

temp2<-envdata[14,10]^2

C_AVG<-((envdata[14,11]/10)*(1/3))+((envdata[14,12]/10)*(2/3))

N_AVG<-((envdata[14,13]/100)*(1/3))+((envdata[14,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[14,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[14,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,which(Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[14,3])
observed.tasb<-c(envdata[14,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,which(Diameter[observed.a,]>=5)], main = "2309", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 14
mylist[[14]] <- df

# Plot 2328 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[15,15]*0.0001

temp<-envdata[15,10]

temp2<-envdata[15,10]^2

C_AVG<-((envdata[15,11]/10)*(1/3))+((envdata[15,12]/10)*(2/3))

N_AVG<-((envdata[15,13]/100)*(1/3))+((envdata[15,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[15,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[15,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[15,3])
observed.tasb<-c(envdata[15,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "2328", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 15
mylist[[15]] <- df

# Plot 2642 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[16,15]*0.0001

temp<-envdata[16,10]

temp2<-envdata[16,10]^2

C_AVG<-((envdata[16,11]/10)*(1/3))+((envdata[16,12]/10)*(2/3))

N_AVG<-((envdata[16,13]/100)*(1/3))+((envdata[16,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[16,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[16,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[16,3])
observed.tasb<-c(envdata[16,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "2642", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 16
mylist[[16]] <- df

# Plot 2711 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[17,15]*0.0001

temp<-envdata[17,10]

temp2<-envdata[17,10]^2

C_AVG<-((envdata[17,11]/10)*(1/3))+((envdata[17,12]/10)*(2/3))

N_AVG<-((envdata[17,13]/100)*(1/3))+((envdata[17,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[17,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[17,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[17,3])
observed.tasb<-c(envdata[17,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "2711", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 17
mylist[[17]] <- df

# Plot 2801 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[18,15]*0.0001

temp<-envdata[18,10]

temp2<-envdata[18,10]^2

C_AVG<-((envdata[18,11]/10)*(1/3))+((envdata[18,12]/10)*(2/3))

N_AVG<-((envdata[18,13]/100)*(1/3))+((envdata[18,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[18,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[18,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a, which(Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[18,3])
observed.tasb<-c(envdata[18,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a, which(Diameter[observed.a,]>=5)], main = "2801", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 18
mylist[[18]] <- df

# Plot 3105 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[19,15]*0.0001

temp<-envdata[19,10]

temp2<-envdata[19,10]^2

C_AVG<-((envdata[19,11]/10)*(1/3))+((envdata[19,12]/10)*(2/3))

N_AVG<-((envdata[19,13]/100)*(1/3))+((envdata[19,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[19,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[19,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[19,3])
observed.tasb<-c(envdata[19,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "3105", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 19
mylist[[19]] <- df

# Plot 3614 ----------------------------------------------


# set parameters for growth equation
aridity<-envdata[20,15]*0.0001

temp<-envdata[20,10]

temp2<-envdata[20,10]^2

C_AVG<-((envdata[20,11]/10)*(1/3))+((envdata[20,12]/10)*(2/3))

N_AVG<-((envdata[20,13]/100)*(1/3))+((envdata[20,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[20,2])
observed.a<-round((10^(-1.701186 +0.01039727*CN -0.01136369*CN*aridity +1.194143*aridity +0.01898789*temp))*(envdata[20,3]^(1.728844)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*1^(-0.4215787))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.9840024 -0.006014*CN + 0.006573*CN*aridity -0.690718*aridity -0.010983*temp))*(0.5784213*age[i,j]^(-0.4215787))
      
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
  predict.d[o,1]<-mean(Diameter[observed.a, which(Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[20,3])
observed.tasb<-c(envdata[20,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a, which(Diameter[observed.a,]>=5)], main = "3614", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 20
mylist[[20]] <- df

final_list_slash <- do.call(rbind.data.frame, mylist)
row.names(final_list_slash) <- c(as.vector(envdata[,9]))
# final_list$Plot<- c(1,12,14:18)
# final_list$Plot<-as.factor(final_list$Plot)
sdev<-as.vector(final_list_slash$sd.tasb)
sdev.dbh<-as.vector(final_list_slash$sd.dbh)

# Compare observed diameter vs modeled diameter ---------------------------
par(mfrow=c(1,1))

model.1<-lm(data = final_list, log10(Observed_Diameter/Modeled_Diameter)~log10(Age)+Tree_Density+Temperature+Soil_CN*Aridity)
summary(model.1)
# plot(Temperature~Aridity, data= final_list[[1]])
# 
# res.age<-residuals(model.1)
# plot(res.age~df2$Age)
# plot(res.age~df2$Aridity)

model.2<-lm(data = final_list, log10(Observed_Diameter/Modeled_Diameter)~Tree_Density+Temperature)
summary(model.2)

model.3<-lm(data = final_list, Modeled_Biomass~Observed_Biomass)
summary(model.3)

plot(data = final_list, Observed_Diameter~Modeled_Diameter, xlim = c(0,15), ylim = c(0,15), xlab="Modeled DBH (in)", ylab="Observed DBH (in)",
     main = "Before parameter correction", col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Diameter~Modeled_Diameter, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Diameter-sdev.dbh, final_list$Observed_Diameter, final_list$Modeled_Diameter+sdev.dbh, final_list$Observed_Diameter, length=0.05, angle=90, code=3)

plot(data = final_list, Observed_Biomass~Modeled_Biomass, xlim = c(0,20000), ylim = c(0,20000), col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="Tree Carbon (gC/m^2)",
     col.axis="#027368", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Biomass-sdev, final_list$Observed_Biomass, final_list$Modeled_Biomass+sdev, final_list$Observed_Biomass, length=0.05, angle=90, code=3)
