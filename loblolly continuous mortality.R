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
M <- numeric(length = 1)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  RD<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  sum_RD<-vector()
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*1^(-0.2594352))
  RD[1,]<-(0.00015+0.00218*.51)*((Diameter[1,]*2.54)/25)^1.6
  sum_RD[1]<-sum(RD[1,])
  
  for (i in 2:observed.a){  # specify how long to run the simulation (years)
  
      for (j in 1:plot_density){ # specify tree per hectare
    
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.211954 - 0.0011033*CN + 3.2046573*aridity - 1.9681509*aridity2))*(0.7405648*age[i,j]^(-0.2594352))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      RD[i,j]<-RD[i-1,j]
      sum_RD[i]<-sum_RD[i-1]
      
      if (Diameter[i-1,j]>0){ 
                prob_live<-1+exp(-(-4.924 - 1.421*Diameter[i,j]*2.54 + 0.451*((Diameter[i,j]*2.54)^(2)) 
                                 + 0.449*sum_RD[i] - 0.160*(sum_RD[i]^(2)) + 0.136))
                prob_mort<-1-(prob_live*.001)
                M<- rbinom(1,1,(1/prob_mort))
              }
   
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-(0.037403*((Diameter[i,j]*2.54)^2.676835))
      RD[i,j]<-(0.00015+0.00218*.51)*(((Diameter[i,j]*2.54)/25)^1.6)
      sum_RD<-sum(RD[i,])
      
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
