# Run longleaf pine simulations ----------------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment")
envdata<-read.csv("LongleafEnvData1.csv", header=T, sep=",") 
load("longleafAgeTotals.rdata")
load("longleafAgeTotalsEnd.rdata")
load("longleafDIATotalsEnd.rdata")
load("longleafDIATotals.rdata")
load("longleafPlotStart.rdata")
load("longleafPlotEnd.rdata")


par(mfrow=c(2,5))

mylist<-list()


# Plot 170 ----------------------------------------------

simu.diameters<-list()

a<-c(1, 3:23)

for (s in a){
  
  # set stand age and density
  plot_density<-diameter.totals[[s]]
  observed.a<-envdata[s,6]
  ages<-age.totals[[s]]
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
        
        
        growth<-(10^(1.872009 - 0.016069*envdata[s,16] + 0.019731*envdata[s,16]*envdata[s,17] - 1.912994*envdata[s,17]))*(0.5718692*age[i,j]^(-0.4281308))
        #growth<-(10^(0.9333281 - 0.009259*CN + 0.011340*CN*aridity - 0.977477*aridity))*(0.5718692*age[i,j]^(-0.4281308))
        
        # define the mortality rate here
        # initialize as a numeric with only 1 possible value
        M <- numeric(length = 1)
        
        # Mortality based on diameter class
        if (Diameter[i,j]>0) {M<- rbinom(1,1,(1.636e-01 +(-8.008e-03 *Diameter[i,j]*2.54)+(9.980e-05*(Diameter[i,j]*2.54^2)))/4)}
        # else {M=0}
        print(M)
        # if (Diameter[i,j]>0) { M<- rbinom(1,1,(2.109e-02 +(-2.662e-03 *Diameter[i-1,j])+(8.540e-05*(Diameter[i-1,j]^2))))}
        # else {M=0}
        # print(M)
        # if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.162/8))}
        # else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.026/8))}
        # else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.006/8))}
        # else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.013/8))}
        # else if (Diameter[i,j]>15.75 & Diameter[i,j]<=19.69){M<-rbinom(1,1,(.024/8))}
        # else if (Diameter[i,j]>19.69 & Diameter[i,j]<=23.62){M<-rbinom(1,1,(.047/8))}
        # else if (Diameter[i,j]>23.62 & Diameter[i,j]<=27.56){M<-rbinom(1,1,(.060/8))}
        # else if (Diameter[i,j]>27.56){M<-rbinom(1,1,(0.129/8))}
        
        
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
  hist(Diameter[observed.a,], main=paste("End simulation", s), xlab = "Diameter (in)")
  
  # set up dataframe to store simulated data
  df<-cbind(observed.d, modeled.d, observed.a, length(plot_density), envdata[s,12], envdata[s,17], observed.tasb, modeled.tasb, 
            sd.tasb, sd.dbh)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature",  "Aridity", "Observed_Biomass", 
                  "Modeled_Biomass", "sd.tasb", "sd.dbh")
  
  # data will be saved as list 1
  mylist[[s]] <- df
  simu.diameters[[s]]<-Diameter[observed.a,]
}

save(simu.diameters, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/longleafDIATotals.simu2.4.rdata")

par(mfrow=c(1,1))

final_list <- do.call(rbind.data.frame, mylist)
row.names(final_list) <- c(a)
# final_list$Plot<- c(1,12,14:18)
# final_list$Plot<-as.factor(final_list$Plot)
sdev<-as.vector(final_list$sd.tasb)
sdev.dbh<-as.vector(final_list$sd.dbh)

model.1<-lm(data = final_list, log10(Observed_Biomass/Modeled_Biomass)~Tree_Density +Temperature + Aridity)
summary(model.1)
# plot(Temperature~Aridity, data= final_list[[1]])
# 
# res.age<-residuals(model.1)
# plot(res.age~df2$Age)
# plot(res.age~df2$Aridity)

model.2<-lm(data = final_list_slash, log10(Observed_Diameter/Modeled_Diameter)~Tree_Density)
summary(model.2)

model.3<-lm(data = final_list, Modeled_Biomass~Observed_Biomass)
summary(model.3)

plot(data = final_list, Observed_Diameter~Modeled_Diameter,  xlim = c(1,12), ylim = c(1,12), xlab="Modeled DBH (in)", ylab="Observed DBH (in)",
     main = "Before parameter correction", col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Diameter~Modeled_Diameter, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Diameter-sdev.dbh, final_list$Observed_Diameter, final_list$Modeled_Diameter+sdev.dbh, final_list$Observed_Diameter, length=0.05, angle=90, code=3)

plot(data = final_list, Observed_Biomass~Modeled_Biomass, xlim = c(0,6), ylim = c(0,6),col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="AGB (kgC/m^2)",
     col.axis="#027368", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Biomass-sdev, final_list$Observed_Biomass, final_list$Modeled_Biomass+sdev, final_list$Observed_Biomass, length=0.05, angle=90, code=3)
