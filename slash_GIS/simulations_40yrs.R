# Run slash pine simulations ----------------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon/slash_GIS")
envdata<-read.csv("Export_Output.csv", header=T, sep=",")

par(mfrow=c(2,5))
final_list_slash<-list()

# Plot 170 ----------------------------------------------

# data will be saved as list 1
mylist<-list() 

for (s in 1:172) {

# set parameters for growth equation
aridity<-envdata[s,5]*0.0001

temp<-envdata[s,7]

temp2<-temp^2


# set stand age and density
plot_density<- 900
observed.a<- 40
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
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a, ])*.5*1000*(1/10000)
}

modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)

# set up dataframe to store simulated data
df<-cbind(modeled.d, observed.a, plot_density, temp, aridity, modeled.tasb, sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Modeled_Diameter","Age", "Tree_Density", "Temperature", "Aridity", "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 1
mylist[[s]] <- df

}

final_list_slash <- do.call(rbind.data.frame, mylist)

write.csv(final_list_slash, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/slash_GIS/simulations_40yrs.csv")
