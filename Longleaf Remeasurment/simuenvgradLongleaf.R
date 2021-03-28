# Run longleaf pine simulations ----------------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
# envdata<-read.csv("random_sample_envdata.csv", header=T, sep=",") 
load("scenarios.rdata")

# setwd("C:/Users/Alicia/Downloads/LongleafCalibration3.15.21")
# load("Longleafparameters3.15.21.rdata")
setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment")
load("sampleparameters3.24.21.rdata") #estimating sigma
par(mfrow=c(2,5))

mylist<-list()


# SOILGRIDS_C_AVG<-((envdata$SOC0_5_NAD/10)*(1/3))+((envdata$SOC5_15_NA/10)*(2/3))
# 
# SOILGRIDS_N_AVG<-((envdata$N0_5_NAD83/100)*(1/3))+((envdata$N5_15_NAD8/100)*(2/3))                 
# 
# SOILGRIDS_CN<-SOILGRIDS_C_AVG/SOILGRIDS_N_AVG 
# 
# envdata$SOILGRIDS_CN_SCALE<-(SOILGRIDS_CN*6.212)+24.634
# 
# 
# envdata$aridity<-envdata$ai_et0_NAD*.0001


par.name <- c("a1","b1","b2","b3","b4","b5", "b6", "b7", "b8","sigma")
row.names(sample.parameters)<-par.name
modeled.d<-list()
modeled.tasb<-list()

for (s in 1:8){
  
  # set stand age and density
  plot_density<-700
  observed.a<-30
  # temp<-envdata[s,5]
  temp<-scenarios[s,3]
  CN<-scenarios[s,2]
  # CN<-envdata[s,10]
  # aridity<-envdata[s,11]
  aridity<-scenarios[s,1]
  predict.tasb<-matrix(nrow = 300, ncol = 1,0)
  predict.d<-matrix(nrow = 300, ncol = 1,0)
  
  for (o in 1:300){
    age<-matrix(0, nrow=observed.a, ncol=plot_density) # initialize the age matrix
    Diameter<-matrix(0, nrow=observed.a, plot_density) # initialize the diameter matrix
    TASB<-matrix(0, nrow=observed.a, plot_density) # initialize the total above-stump biomass matrix
    
    # initialize the diameter for the first year
    Diameter[1,]<-(10^(sample.parameters[1,o] - sample.parameters[2,o]*CN + sample.parameters[3,o]*CN*aridity 
                       - sample.parameters[4,o]*aridity- sample.parameters[5,o]*temp))*(sample.parameters[6,o]*1^(sample.parameters[7,o]))
    age[1,]<-1
    
    for (j in 1:plot_density){ # specify tree per hectare
      
      for (i in 2:observed.a){ # specify how long to run the simulation (years)
        
        age[i,j]<-age[i-1,j]+1
        
        growth<-(10^(sample.parameters[1,o] - sample.parameters[2,o]*CN + sample.parameters[3,o]*CN*aridity 
                     - sample.parameters[4,o]*aridity- sample.parameters[5,o]*temp))*(sample.parameters[6,o]*age[i,j]^(sample.parameters[7,o]))
        
        # define the mortality rate here
        # initialize as a numeric with only 1 possible value
        M <- numeric(length = 1)
        
        # Mortality based on diameter class
        if (Diameter[i,j]>=0) {
          M<- rbinom(1,1,(sample.parameters[8,o] +(sample.parameters[9,o] *Diameter[i,j]*2.54)+sample.parameters[10,o]*((Diameter[i,j]*2.54)^2)))
          
        }
        
        
        
        # Calculate the diameter for jth tree for the ith observed year
        Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
        
        #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
        TASB [i,j]<-(0.0725*((Diameter[i,j]*2.54)^2.5074))+(0.0016*((Diameter[i,j]*2.54)^3.0786))+(0.0214*((Diameter[i,j]*2.54)^2.0051))
        
        # If the tree dies, plant a new tree (age = 0)
        if (M==1){age[i,j]<-0}
      }
    }
    # save average modeled diameter
    predict.d[o,1]<-mean(Diameter[observed.a,])
    predict.tasb[o,1]<-sum(TASB[observed.a, ])*.5*(1/4047)
    
  }
  modeled.d[[s]]<-predict.d
  modeled.tasb[[s]]<-predict.tasb
  sd.tasb<-sd(predict.tasb)
  sd.dbh<-sd(predict.d)
  hist(Diameter[observed.a,], main=paste("End simulation", s), xlab = "Diameter (in)")
  
  # set up dataframe to store simulated data
  df<-cbind( modeled.d, observed.a, plot_density, temp, aridity,  modeled.tasb, 
             sd.tasb, sd.dbh)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Modeled_Diameter","Age", "Tree_Density", "Temperature",  "Aridity",  
                  "Modeled_Biomass", "sd.tasb", "sd.dbh")
  
  # data will be saved as list 1
  mylist[[s]] <- df
  
}

par(mfrow=c(1,1))

AGB_Longleaf<-do.call(rbind.data.frame, modeled.tasb)
AGB_Longleaf[1:300,"plots"]<-"Dry/HighN/Hot"
AGB_Longleaf[301:600,2]<-"Dry/LowN/Hot"
AGB_Longleaf[601:900,2]<-"Wet/HighN/Hot"
AGB_Longleaf[901:1200,2]<-"Wet/LowN/Hot"
AGB_Longleaf[1201:1500,2]<-"Dry/HighN/Cool"
AGB_Longleaf[1501:1800,2]<-"Dry/LowN/Cool"
AGB_Longleaf[1801:2100,2]<-"Wet/HighN/Cool"
AGB_Longleaf[2101:2400,2]<-"Wet/LowN/Cool"
AGB_Longleaf[,"species"]<-"Longleaf"

DBH_Longleaf<-do.call(rbind.data.frame, modeled.d)
DBH_Longleaf[1:300,"plots"]<-"Dry/HighN/Hot"
DBH_Longleaf[301:600,2]<-"Dry/LowN/Hot"
DBH_Longleaf[601:900,2]<-"Wet/HighN/Hot"
DBH_Longleaf[901:1200,2]<-"Wet/LowN/Hot"
DBH_Longleaf[1201:1500,2]<-"Dry/HighN/Cool"
DBH_Longleaf[1501:1800,2]<-"Dry/LowN/Cool"
DBH_Longleaf[1801:2100,2]<-"Wet/HighN/Cool"
DBH_Longleaf[2101:2400,2]<-"Wet/LowN/Cool"
DBH_Longleaf[,"species"]<-"Longleaf"

save(AGB_Longleaf, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/SimuLongleaf30yrsAGB.rdata")
save(DBH_Longleaf, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/SimuLongleaf30yrsDBH.rdata")

final_list <- do.call(rbind.data.frame, mylist)
row.names(final_list) <- c(a)
sdev<-as.vector(final_list$sd.tasb)
sdev.dbh<-as.vector(final_list$sd.dbh)

par(mfrow=c(1,1))
AGB<-lm(data = final_list, Observed_Biomass~Modeled_Biomass)
summary(AGB)

plot(data = final_list, Observed_Biomass~Modeled_Biomass,xlim=c(0,5.5), ylim=c(0, 5.5), col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="AGB (kgC/m^2)",
     col.axis="#027368", pch=16, type="p")
abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Biomass-sdev, final_list$Observed_Biomass, final_list$Modeled_Biomass+sdev, final_list$Observed_Biomass, length=0.05, angle=90, code=3)

plot(data = final_list, Observed_Diameter~Modeled_Diameter, xlim=c(0,20), ylim=c(0,20), xlab="Modeled DBH (in)", ylab="Observed DBH (in)",
     col.axis="#027368", col="#75BFBF", pch=16, type="p")
abline(0,1, col="#048ABF")
text(Observed_Diameter~Modeled_Diameter, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Diameter-sdev.dbh, final_list$Observed_Diameter, final_list$Modeled_Diameter+sdev.dbh, final_list$Observed_Diameter, length=0.05, angle=90, code=3)

final_list[,"species"]<-"Longleaf"
save(final_list, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/SimuLongleaf30yrs.rdata")

