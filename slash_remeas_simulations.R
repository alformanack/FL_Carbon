# Run slash pine simulations ----------------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
envdata<-read.csv("slash_remeas_env.csv", header=T, sep=",")
load("slash_remeas_start.Rdata")


a<-c(451, 177, 587, 546, 124, 547, 235, 131, 589, 141, 303, 305, 188,  10, 391, 318, 348, 464, 402, 67)

par(mfrow=c(2,5))
final_list_slash<-list()

# Plot 170 ----------------------------------------------

# data will be saved as list 1
for (s in 2:20){
  
  plot_data<-final.start[[s]] %>%
    filter(STATUSCD=="1") %>%
    mutate(TPA=ifelse(is.na(TPA_UNADJ), TPAGROW_UNADJ, TPA_UNADJ)) %>%
    mutate(TPA_total=sum(round(TPA))) %>%
    mutate(age=round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity)*(DIA^1.728844))))  

  diameter.totals<-vector()
  age.totals<-vector
    for (h in 1:length(plot_data$DIA))
      {diameter.totals<-rep((plot_data$DIA), round(plot_data$TPA))
        age.totals<-rep((plot_data$age), round(plot_data$TPA))}
  hist(diameter.totals, main = paste("Start plot", s), xlab = "Diameter (in)")
      
  plot_data_end<-final.end[[s]] %>%
    filter(STATUSCD=="1") %>%
    mutate(TPA=ifelse(is.na(TPA_UNADJ), TPAGROW_UNADJ, TPA_UNADJ)) %>%
    mutate(TPA_total=sum(round(TPA))) %>%
    mutate(age=round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity)*(DIA^1.728844))))  
  
  diameter.totals.end<-vector()
  age.totals.end<-vector
    for (h in 1:length(plot_data_end$DIA))
    {diameter.totals.end<-rep((plot_data_end$DIA), round(plot_data_end$TPA))
      age.totals.end<-rep((plot_data_end$age), round(plot_data_end$TPA))}
  hist(diameter.totals.end, main =paste("End plot", s), xlab = "Diameter (in)")
      
}

for (c in a){

# set parameters for growth equation
aridity<-envdata[10, 7]*0.0001

temp<-envdata[10, 6]

temp2<-envdata[10, 6]^2

# C_AVG<-((envdata[1,11]/10)*(1/3))+((envdata[1,12]/10)*(2/3))
# 
# N_AVG<-((envdata[1,13]/100)*(1/3))+((envdata[1,14]/100)*(2/3))
# 
# CN<-C_AVG/N_AVG
# 
# CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-diameter.totals
observed.a<-unique(plot_data$how_long)
ages<-age.totals
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a, ncol=length(plot_density)) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a, length(plot_density)) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a, length(plot_density)) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-plot_density
  age[1,]<-ages
  
  for (j in 1:length(plot_density)){ # specify tree per hectare
    
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
  predict.tasb[o,1]<-sum(TASB[21, ])*.5*1000*(1/10000)
}
observed.d<-c(envdata[1,4])
observed.tasb<-c(envdata[1,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[observed.a,], main = a, xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, CN_SCALE*aridity, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "CN*aridity", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 1
mylist[[1]] <- df

