rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment")
envdata<-read.csv("LongleafEnvData2.csv", header=T, sep=",") 
load("longleafAgeTotals2.rdata")
load("longleafAgeTotalsEnd2.rdata")
load("longleafDIATotalsEnd2.rdata")
load("longleafDIATotals2.rdata")
load("longleafPlotStart2.rdata")
load("longleafPlotEnd2.rdata")
load("initialpar3.22.21.Rdata")

load("LongleafInitial.rdata")


par(mfrow=c(2,5))

mylist<-list()


# Plot 170 ----------------------------------------------

# simu.diameters<-list()

total_a<-c(1, 3, 4:6, 8, 9,10, 11:21, 23)
calibration<-c(21, 19, 16,  3, 23, 14,  9,  1, 20, 11)
a <-setdiff(total_a,calibration)

for (s in a){
  
  # set stand age and density
  plot_density<-diameter.totals[[s]]
  observed.a<-envdata[s,6]
  ages<-age.totals[[s]]
  CN <- envdata[s,16]
  aridity <- envdata[s,17]
  temp <- envdata[s,12]
  predict.tasb<-matrix(nrow = 300, ncol = 1,0)
  predict.d<-matrix(nrow = 300, ncol = 1,0)
  
  
  for (o in 1:300){
    age<-matrix(0, nrow=observed.a, ncol=length(plot_density)) # initialize the age matrix
    Diameter<-matrix(0, nrow=observed.a, length(plot_density)) # initialize the diameter matrix
    TASB<-matrix(0, nrow=observed.a, length(plot_density)) # initialize the total above-stump biomass matrix
    
    # initialize the diameter for the first year
    Diameter[1,]<-plot_density
    age[1,]<-ages
    
    for (j in 1:length(plot_density)){ # specify tree per hectare
      
      for (i in 2:observed.a){ # specify how long to run the simulation (years)
        
        age[i,j]<-age[i-1,j]+1
        
        growth<-(10^(initial.par[o,1] + initial.par[o,2]*CN + initial.par[o,5]*CN*aridity 
                     + initial.par[o,3]*aridity + initial.par[o,4]*temp))*(initial.par[o,6]*age[i,j]^(initial.par[o,6]-1))
        
        # define the mortality rate here
        # initialize as a numeric with only 1 possible value
        M <- numeric(length = 1)
        
        # Mortality based on diameter class
 
        if (Diameter[i,j]>=0) {M<- rbinom(1,1,(initial.par[o,7] +(initial.par[o,8] *Diameter[i,j]*2.54)+ initial.par[o,9]*((Diameter[i,j]*2.54)^2)))
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
  
  observed.d<-mean(diameter.totals[[s]])
  observed.tasb<-sum(plot_data_end[[s]]$TASB)*.5*(1/4047)
  modeled.d<-mean(predict.d)
  modeled.tasb<-mean(predict.tasb)
  sd.tasb<-sd(predict.tasb)
  sd.dbh<-sd(predict.d)
  # hist(Diameter[observed.a,], main=paste("End simulation", s), xlab = "Diameter (in)")
  
  # set up dataframe to store simulated data
  df<-cbind(observed.d, modeled.d, observed.a, length(plot_density), envdata[s,12], envdata[s,17], observed.tasb, modeled.tasb, 
            sd.tasb, sd.dbh)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature",  "Aridity", "Observed_Biomass", 
                  "Modeled_Biomass", "sd.tasb", "sd.dbh")
  
  # data will be saved as list 1
  mylist[[s]] <- df
  # simu.diameters[[s]]<-Diameter[observed.a,]
}


par(mfrow=c(1,1))


final_list_longleaf <- do.call(rbind.data.frame, mylist)
row.names(final_list_longleaf) <- c(a)
sdev<-as.vector(final_list_longleaf$sd.tasb)
sdev.dbh<-as.vector(final_list_longleaf$sd.dbh)


plot(data = final_list_longleaf, Observed_Diameter~Modeled_Diameter,  xlim = c(1,12), ylim = c(1,12), xlab="Modeled DBH (in)", ylab="Observed DBH (in)",
     main = "Before parameter correction", col.axis="#027368", col="#75BFBF", pch=16, type="p") 
abline(0,1, col="#048ABF")
text(Observed_Diameter~Modeled_Diameter, labels=rownames(final_list_longleaf),data=final_list_longleaf, cex=0.9, font=2, pos=4)
arrows(final_list_longleaf$Modeled_Diameter-sdev.dbh, final_list_longleaf$Observed_Diameter, final_list_longleaf$Modeled_Diameter+sdev.dbh, final_list_longleaf$Observed_Diameter, length=0.05, angle=90, code=3)

ABG<-lm(data = final_list_longleaf, Observed_Biomass~Modeled_Biomass)
summary(ABG)
DBH<-lm(data = final_list_longleaf, Observed_Diameter~Modeled_Diameter)
summary(DBH)

RMSE<-sqrt(sum((final_list_longleaf$Observed_Biomass-final_list_longleaf$Modeled_Biomass)^2/10))
RMSE.dbh<-sqrt(sum((final_list_longleaf$Observed_Diameter-final_list_longleaf$Modeled_Diameter)^2/10))

plot(data = final_list_longleaf, Observed_Biomass~Modeled_Biomass,  xlim = c(0,6), ylim = c(0,6), col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="AGB (kgC/m^2)",
     col.axis="#027368", pch=16, type="p") 
abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list_longleaf),data=final_list_longleaf, cex=0.9, font=2, pos=4)
arrows(final_list_longleaf$Modeled_Biomass-sdev, final_list_longleaf$Observed_Biomass, final_list_longleaf$Modeled_Biomass+sdev, final_list_longleaf$Observed_Biomass, length=0.05, angle=90, code=3)

final_list_longleaf[,"species"]<-"Longleaf"
save(final_list_longleaf, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/LongleafInitial.rdata")

