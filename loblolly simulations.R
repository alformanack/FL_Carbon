# Run loblolly pine simulations ----------------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")

envdata<-read.csv("loblolly_env_data.csv", header=T, sep=",")

par(mfrow=c(2,5))
final_list_loblolly<-list()

# Plot 18 ----------------------------------------------

# data will be saved as list 1
mylist<-list() 


# set parameters for growth equation
aridity<-envdata[1,10]*0.0001

aridity2<-aridity^2

temp<-envdata[1,11]

C_AVG<-((envdata[1,12]/10)*(1/3))+((envdata[1,13]/10)*(2/3))

N_AVG<-((envdata[1,14]/100)*(1/3))+((envdata[1,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[1,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[1,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a, which (Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[1,4])
observed.tasb<-c(envdata[1,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,which (Diameter[observed.a,]>=5)], main = "18", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 1
mylist[[1]] <- df


# Plot 29 --------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[2,10]*0.0001

aridity2<-aridity^2

temp<-envdata[2,11]

C_AVG<-((envdata[2,12]/10)*(1/3))+((envdata[2,13]/10)*(2/3))

N_AVG<-((envdata[2,14]/100)*(1/3))+((envdata[2,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[2,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[2,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a, which (Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[2,4])
observed.tasb<-c(envdata[2,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,which (Diameter[observed.a,]>=5)], main = "29", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 2
mylist[[2]] <- df


# Plot 30--------------------------------------------------------------------



# set parameters for growth equation
aridity<-envdata[3,10]*0.0001

aridity2<-aridity^2

temp<-envdata[3,11]

C_AVG<-((envdata[3,12]/10)*(1/3))+((envdata[3,13]/10)*(2/3))

N_AVG<-((envdata[3,14]/100)*(1/3))+((envdata[3,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[3,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[3,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a, ])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[3,4])
observed.tasb<-c(envdata[3,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "30", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 3
mylist[[3]] <- df

# Plot 36--------------------------------------------------------------------



# set parameters for growth equation
aridity<-envdata[4,10]*0.0001

aridity2<-aridity^2

temp<-envdata[4,11]

C_AVG<-((envdata[4,12]/10)*(1/3))+((envdata[4,13]/10)*(2/3))

N_AVG<-((envdata[4,14]/100)*(1/3))+((envdata[4,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[4,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[4,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a, ])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[4,4])
observed.tasb<-c(envdata[4,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "36", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 3
mylist[[4]] <- df

# Plot 75--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[5,10]*0.0001

aridity2<-aridity^2

temp<-envdata[5,11]

C_AVG<-((envdata[5,12]/10)*(1/3))+((envdata[5,13]/10)*(2/3))

N_AVG<-((envdata[5,14]/100)*(1/3))+((envdata[5,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[5,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[5,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a, ])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[5,4])
observed.tasb<-c(envdata[5,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "75", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 3
mylist[[5]] <- df

# Plot 83--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[6,10]*0.0001

aridity2<-aridity^2

temp<-envdata[6,11]

C_AVG<-((envdata[6,12]/10)*(1/3))+((envdata[6,13]/10)*(2/3))

N_AVG<-((envdata[6,14]/100)*(1/3))+((envdata[6,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[6,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[6,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a, which(Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a, ])*.5*1000*(1/10000)
}
observed.d<-c(envdata[6,4])
observed.tasb<-c(envdata[6,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a, which(Diameter[observed.a,]>=5)], main = "83", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 6
mylist[[6]] <- df

# Plot 91--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[7,10]*0.0001

aridity2<-aridity^2

temp<-envdata[7,11]

C_AVG<-((envdata[7,12]/10)*(1/3))+((envdata[7,13]/10)*(2/3))

N_AVG<-((envdata[7,14]/100)*(1/3))+((envdata[7,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[7,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[7,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[7,4])
observed.tasb<-c(envdata[7,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "91", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 7
mylist[[7]] <- df

# Plot 122--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[8,10]*0.0001

aridity2<-aridity^2

temp<-envdata[8,11]

C_AVG<-((envdata[8,12]/10)*(1/3))+((envdata[8,13]/10)*(2/3))

N_AVG<-((envdata[8,14]/100)*(1/3))+((envdata[8,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[8,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[8,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a, which(Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[8,4])
observed.tasb<-c(envdata[8,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a, which(Diameter[observed.a,]>=5)], main = "122", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 8
mylist[[8]] <- df

# Plot 130--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[9,10]*0.0001

aridity2<-aridity^2

temp<-envdata[9,11]

C_AVG<-((envdata[9,12]/10)*(1/3))+((envdata[9,13]/10)*(2/3))

N_AVG<-((envdata[9,14]/100)*(1/3))+((envdata[9,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[9,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[9,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a, which(Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a, ])*.5*1000*(1/10000)
}
observed.d<-c(envdata[9,4])
observed.tasb<-c(envdata[9,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a, which(Diameter[observed.a,]>=5)], main = "130", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 9
mylist[[9]] <- df

# Plot 133--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[10,10]*0.0001

aridity2<-aridity^2

temp<-envdata[10,11]

C_AVG<-((envdata[10,12]/10)*(1/3))+((envdata[10,13]/10)*(2/3))

N_AVG<-((envdata[10,14]/100)*(1/3))+((envdata[10,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[10,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[10,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[10,4])
observed.tasb<-c(envdata[10,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "133", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 10
mylist[[10]] <- df

# Plot 147--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[11,10]*0.0001

aridity2<-aridity^2

temp<-envdata[11,11]

C_AVG<-((envdata[11,12]/10)*(1/3))+((envdata[11,13]/10)*(2/3))

N_AVG<-((envdata[11,14]/100)*(1/3))+((envdata[11,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[11,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[11,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[11,4])
observed.tasb<-c(envdata[11,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "147", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 11
mylist[[11]] <- df

# Plot 153--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[12,10]*0.0001

aridity2<-aridity^2

temp<-envdata[12,11]

C_AVG<-((envdata[12,12]/10)*(1/3))+((envdata[12,13]/10)*(2/3))

N_AVG<-((envdata[12,14]/100)*(1/3))+((envdata[12,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[12,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[12,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a,which(Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[12,4])
observed.tasb<-c(envdata[12,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,which(Diameter[observed.a,]>=5)], main = "153", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 12
mylist[[12]] <- df

# Plot 156--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[13,10]*0.0001

aridity2<-aridity^2

temp<-envdata[13,11]

C_AVG<-((envdata[13,12]/10)*(1/3))+((envdata[13,13]/10)*(2/3))

N_AVG<-((envdata[13,14]/100)*(1/3))+((envdata[13,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[13,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[13,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a,which(Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[13,4])
observed.tasb<-c(envdata[13,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,which(Diameter[observed.a,]>=5)], main = "156", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 13
mylist[[13]] <- df

# Plot 159--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[14,10]*0.0001

aridity2<-aridity^2

temp<-envdata[14,11]

C_AVG<-((envdata[14,12]/10)*(1/3))+((envdata[14,13]/10)*(2/3))

N_AVG<-((envdata[14,14]/100)*(1/3))+((envdata[14,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[14,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[14,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[14,4])
observed.tasb<-c(envdata[14,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "159", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 14
mylist[[14]] <- df

# Plot 182--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[15,10]*0.0001

aridity2<-aridity^2

temp<-envdata[15,11]

C_AVG<-((envdata[15,12]/10)*(1/3))+((envdata[15,13]/10)*(2/3))

N_AVG<-((envdata[15,14]/100)*(1/3))+((envdata[15,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[15,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[15,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[15,4])
observed.tasb<-c(envdata[15,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "182", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 15
mylist[[15]] <- df

# Plot 185--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[16,10]*0.0001

aridity2<-aridity^2

temp<-envdata[16,11]

C_AVG<-((envdata[16,12]/10)*(1/3))+((envdata[16,13]/10)*(2/3))

N_AVG<-((envdata[16,14]/100)*(1/3))+((envdata[16,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[16,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[16,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[16,4])
observed.tasb<-c(envdata[16,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "185", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 16
mylist[[16]] <- df

# Plot 195--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[17,10]*0.0001

aridity2<-aridity^2

temp<-envdata[17,11]

C_AVG<-((envdata[17,12]/10)*(1/3))+((envdata[17,13]/10)*(2/3))

N_AVG<-((envdata[17,14]/100)*(1/3))+((envdata[17,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[17,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[17,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[17,4])
observed.tasb<-c(envdata[17,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "195", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 17
mylist[[17]] <- df

# Plot 210--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[18,10]*0.0001

aridity2<-aridity^2

temp<-envdata[18,11]

C_AVG<-((envdata[18,12]/10)*(1/3))+((envdata[18,13]/10)*(2/3))

N_AVG<-((envdata[18,14]/100)*(1/3))+((envdata[18,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[18,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[18,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a,which(Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[18,4])
observed.tasb<-c(envdata[18,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,which(Diameter[observed.a,]>=5)], main = "210", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 18
mylist[[18]] <- df

# Plot 226--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[19,10]*0.0001

aridity2<-aridity^2

temp<-envdata[19,11]

C_AVG<-((envdata[19,12]/10)*(1/3))+((envdata[19,13]/10)*(2/3))

N_AVG<-((envdata[19,14]/100)*(1/3))+((envdata[19,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[19,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[19,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a,which(Diameter[observed.a,]>=5)])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[19,4])
observed.tasb<-c(envdata[19,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,which(Diameter[observed.a,]>=5)], main = "226", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 19
mylist[[19]] <- df

# Plot 228--------------------------------------------------------------------

# set parameters for growth equation
aridity<-envdata[20,10]*0.0001

aridity2<-aridity^2

temp<-envdata[20,11]

C_AVG<-((envdata[20,12]/10)*(1/3))+((envdata[20,13]/10)*(2/3))

N_AVG<-((envdata[20,14]/100)*(1/3))+((envdata[20,15]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[20,2])
observed.a<-round((10^(1.636527 + 0.001489809*CN -4.327315*aridity + 2.657635*aridity2))*(envdata[20,3]^(1.350321)))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
    }
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a,])*.5*1000*(1/10000)
}
observed.d<-c(envdata[20,4])
observed.tasb<-c(envdata[20,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = "228", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 20
mylist[[20]] <- df

final_list_loblolly <- do.call(rbind.data.frame, mylist)
row.names(final_list_loblolly) <- c(as.vector(envdata[,9]))
# final_list$Plot<- c(1,12,14:18)
# final_list$Plot<-as.factor(final_list$Plot)
sdev<-as.vector(final_list_loblolly$sd.tasb)
sdev.dbh<-as.vector(final_list_loblolly$sd.dbh)



# Saving simulation results/combining species lists -----------------------


write.csv(final_list_loblolly, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/final_list_loblolly.csv")
setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
final_list_loblolly<-read.csv("final_list_loblolly.csv", header=T, sep=",")
final_list_slash<-read.csv("final_list_slash.csv", header=T, sep=",")

#For site index slash
df_slash<-final_list_slash[-c(0,6:9)]
df_longleaf<-final_list_longleaf[-c(0,6:9)]
df_loblolly<-final_list_loblolly[-c(0,6:9)]

final_list<-data.frame()
final_list<-do.call("rbind", list(df_loblolly, df_longleaf, df_slash))
final_list[,"Species"]<-as.character(final_list$Species)
final_list[1:20,10]<-"Loblolly"
final_list[21:40,10]<-"Longleaf"
final_list[41:60,10]<-"Slash"

#for original slash
final_list<-data.frame()
final_list<-do.call("rbind", list(final_list_loblolly, final_list_longleaf, final_list_slash))
final_list[,"Species"]<-as.character(final_list$Species)
final_list[1:20,14]<-"Loblolly"
final_list[21:40,14]<-"Longleaf"
final_list[41:60,14]<-"Slash"
# final_list<-final_list[-c(27), ]

# Compare observed diameter vs modeled diameter ---------------------------
par(mfrow=c(1,1))

model.1<-lm(data = final_list_loblolly, log10(Observed_Diameter/Modeled_Diameter)~log10(Age)+Tree_Density+Temperature+Soil_CN*Aridity)
summary(model.1)
# plot(Temperature~Aridity, data= final_list[[1]])
# 
# res.age<-residuals(model.1)
# plot(res.age~df2$Age)
# plot(res.age~df2$Aridity)

model.2<-lm(data = final_list, log10(Observed_Diameter/Modeled_Diameter)~log10(Age)+Tree_Density)
summary(model.2)

model.3<-lm(data = final_list, Modeled_Biomass~Tree_Density)
plot(data = final_list, Modeled_Biomass~Tree_Density)
plot(data = final_list, Observed_Biomass~Tree_Density)
plot(data = final_list, Modeled_Diameter~Tree_Density)
plot(data = final_list, Observed_Diameter~Tree_Density)


plot(data = final_list, Observed_Diameter~Modeled_Diameter, xlim = c(3,11), ylim = c(3,11), xlab="Modeled DBH (in)", ylab="Observed DBH (in)",
     main = "Before parameter correction", col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Diameter~Modeled_Diameter, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Diameter-sdev.dbh, final_list$Observed_Diameter, final_list$Modeled_Diameter+sdev.dbh, final_list$Observed_Diameter, length=0.05, angle=90, code=3)

ggplot(final_list, aes(Modeled_Diameter,Observed_Diameter )) +
  labs(title="Average DBH (inches)", x="Modeled DBH (in)", y="Observed DBH (in)") +
  xlim(2,16) +
  ylim(2,16) +
  geom_abline(linetype = "dashed") +
  geom_point(aes(size=Tree_Density, color=Species)) +
  scale_color_manual(values=c("#495F8C", "#F2B705", "#F25C05")) +
  
  geom_errorbarh(aes(xmin=final_list$Modeled_Diameter-(1.96*final_list$sd.dbh), 
                    xmax=final_list$Modeled_Diameter+(1.96*final_list$sd.dbh), height = .2)) +
  theme_bw() 

ggplot(final_list, aes(Modeled_Biomass,Observed_Biomass )) +
  labs(title="Tree Carbon (gC/m^2)", x="Modeled", y="Observed") +
  xlim(0,20000) +
  ylim(0,20000) +
  geom_abline(linetype = "dashed") +
  geom_point(aes(size=Tree_Density, color=Species)) +
  scale_color_manual(values=c("#495F8C", "#F2B705", "#F25C05")) +
  
  geom_errorbarh(aes(xmin=final_list$Modeled_Biomass-(1.96*final_list$sd.tasb), 
                    xmax=final_list$Modeled_Biomass+(1.96*final_list$sd.tasb), height = .2)) +
  theme_Publication() 

plot(data = final_list, Observed_Biomass~Modeled_Biomass, xlim = c(0,16000), ylim = c(0,16000), color = "#75BFBF", xlab="Modeled", ylab="Observed", main ="Tree Carbon (gC/m^2)",
     col.axis="#027368", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Biomass-sdev, final_list$Observed_Biomass, final_list$Modeled_Biomass+sdev, final_list$Observed_Biomass, length=0.05, angle=90, code=3)

# Correcting growth equation -------------------------------------------------

# before
growth<-(10^(0.9333281 - 0.009259*CN_SCALE + 0.011340*CN_SCALE*aridity - 0.977477*aridity))*(0.5718692*age[i,j]^(-0.4281308))

#after
growth<-(10^(0.7320281 - 0.009259*CN_SCALE + 0.011340*CN_SCALE*aridity - 0.977477*aridity - 4.574e-05*plot_density))*(0.6834692*age[i,j]^(-0.3165308))

age<- round((10^(-1.071048 + 0.01354706*CN_SCALE  -0.01659182*CN_SCALE*aridity + 1.43017*aridity + 6.692328e-05*plot_density))*(envdata[1,3]^(1.463124)))

rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
envdata<-read.csv("longleaf_env_data.csv", header=T, sep=",")

# Calculating total above stump C biomass for year 2100 for 700 tph by region

Panhandle<-vector()
North<-vector()
Central<-vector()
South<-vector()
plot_density<-700

par(mfrow=c(2,2))

# Panhandle TASB ----------------------------------------------------------

temp<-19.84

temp2<-temp^2

aridity<-9768*.0001

aridity2<-aridity^2

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
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:30){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  Panhandle[b]<-sum(TASB[30,])*.5*1000*(1/10000)
}

hist(Diameter[30,]*2.54, main = "Panhandle", xlab = "Diameter (cm)")

species<-rep("Loblolly",10)

location<-rep("Panhandle", 10)

df<-cbind(species, Panhandle, location)

df<-data.frame(df)

colnames(df)<-c("Species","TASB","Location")

biomass_list[[5]] <- df

# Calculating North TASB --------------------------------------------------

temp<-20.30

temp2<-temp^2

aridity<-8212*.0001

aridity2<-aridity^2

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
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:30){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  North[b]<-sum(TASB[30,])*.5*1000*(1/10000)
}

hist(Diameter[30,]*2.54, main = "North", xlab = "Diameter (cm)")

species<-rep("Loblolly",10)

location<-rep("North", 10)

df<-cbind(species, North, location)

df<-data.frame(df)

colnames(df)<-c("Species","TASB","Location")

biomass_list[[6]] <- df

# TASB Central ------------------------------------------------------------

temp<-22.29

temp2<-temp^2

aridity<-7259*.0001

aridity2<-aridity^2

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
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:30){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  Central[b]<-sum(TASB[30,])*.5*1000*(1/10000)
}

hist(Diameter[30,]*2.54, main = "Central", xlab = "Diameter (cm)")

species<-rep("Loblolly",10)

location<-rep("Central", 10)

df<-cbind(species, Central, location)

df<-data.frame(df)

colnames(df)<-c("Species","TASB","Location")

biomass_list[[7]] <- df

# South TASB --------------------------------------------------------------

temp<-23.80

temp2<-temp^2

aridity<-7305*.0001

aridity2<-aridity^2

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
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:30){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
      else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
      else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
      else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
      else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
      else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
      else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
      else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
      else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
      else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
      else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
      else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
      else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  South[b]<-sum(TASB[30,])*.5*1000*(1/10000)
}

hist(Diameter[30,]*2.54, main = "South", xlab = "Diameter (cm)")

species<-rep("Loblolly",10)

location<-rep("South", 10)

df<-cbind(species, South, location)

df<-data.frame(df)

colnames(df)<-c("Species","TASB","Location")

biomass_list[[8]] <- df



TASB_Panhandle<-c(mean(Panhandle), sd(Panhandle))
TASB_North<-c(mean(North), sd(North))
TASB_Central<-c(mean(Central), sd(Central))
TASB_South<-c(mean(South), sd(South))
table<-rbind(TASB_Panhandle, TASB_North, TASB_Central, TASB_South)