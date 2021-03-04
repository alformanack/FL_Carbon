rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment")
envdata<-read.csv("LongleafEnvData1.csv", header=T, sep=",") 
load("longleafAgeTotals.rdata")
load("longleafAgeTotalsEnd.rdata")
load("longleafDIATotalsEnd.rdata")
load("longleafDIATotals.rdata")
load("longleafPlotStart.rdata")
load("longleafPlotEnd.rdata")
load("sampleparameters2.20.21.rdata")

par(mfrow=c(2,5))

mylist<-list()
mod.tasb<-list()
simu.diameters<-list()

a<-c(1, 3, 4, 6, 8, 9, 11:21, 23)

a<-c(1, 3:23)

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
        
        growth<-(10^(sample.parameters[1,o] - sample.parameters[2,o]*CN + sample.parameters[3,o]*CN*aridity - sample.parameters[4,o]*aridity- sample.parameters[5,o]*temp))*(sample.parameters[6,o]*age[i,j]^(sample.parameters[7,o]))
        
        # define the mortality rate here
        # initialize as a numeric with only 1 possible value
        M <- numeric(length = 1)
        
        # Mortality based on diameter class
        if (Diameter[i,j]>0) {
          M<- rbinom(1,1,(sample.parameters[8,o] +(sample.parameters[9,o] *Diameter[i,j]*2.54)+sample.parameters[10,o]*((Diameter[i,j]*2.54)^2)))

        }
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
  # mod.tasb[[s]]<-predict.tasb
  observed.d<-mean(diameter.totals.end[[s]])
  observed.tasb<-sum(plot_data_end[[s]]$TASB)*.5*(1/4047)
  modeled.d<-mean(predict.d)
  modeled.tasb<-mean(predict.tasb)
  # sd.tasb<-mad(predict.tasb, center = median(predict.tasb), constant = 1.4826, na.rm = FALSE,
      # low = FALSE, high = FALSE)
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
  simu.diameters[[s]]<-Diameter[observed.a,]
  mod.tasb[[s]]<-predict.tasb[,1]
}

par(mfrow=c(1,3))

hist(diameter.totals[[5]])
hist(diameter.totals.end[[5]])
hist(simu.diameters[[5]])

# 

# outliers<-vector()
# for (m in a){
# outliers  <-boxplot(mod.tasb[[6]])$out
# }
# 
# which(mod.tasb[[6]] %in% outliers)
# mad(x, center = median(x), constant = 1.4826, na.rm = FALSE,
#     low = FALSE, high = FALSE)

final_list <- do.call(rbind.data.frame, mylist)
row.names(final_list) <- c(a)
sdev<-as.vector(final_list$sd.tasb)
sdev.dbh<-as.vector(final_list$sd.dbh)

par(mfrow=c(1,1))


plot(data = final_list, Observed_Biomass~Modeled_Biomass,col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="AGB (kgC/m^2)",
     col.axis="#027368", pch=16, type="p")
abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Biomass-sdev, final_list$Observed_Biomass, final_list$Modeled_Biomass+sdev, final_list$Observed_Biomass, length=0.05, angle=90, code=3)

plot(data = final_list, Observed_Diameter~Modeled_Diameter, xlim=c(0,20), ylim=c(0,20), xlab="Modeled DBH (in)", ylab="Observed DBH (in)",
      col.axis="#027368", col="#75BFBF", pch=16, type="p")
abline(0,1, col="#048ABF")
text(Observed_Diameter~Modeled_Diameter, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Diameter-sdev.dbh, final_list$Observed_Diameter, final_list$Modeled_Diameter+sdev.dbh, final_list$Observed_Diameter, length=0.05, angle=90, code=3)

