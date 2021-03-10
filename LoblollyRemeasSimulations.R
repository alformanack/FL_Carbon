# Run longleaf pine simulations ----------------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement")
envdata<-read.csv("LoblollyEnvData.csv", header=T, sep=",") 
load("LoblollyAgeTotals.rdata")
load("LoblollyAgeTotalsEnd.rdata")
load("LoblollyDIATotalsEnd.rdata")
load("LoblollyDIATotals.rdata")
load("LoblollyPlotStart.rdata")
load("LoblollyPlotEnd.rdata")

envdata1<-subset(envdata, envdata$TPA_start<=envdata$TPA_end)
a<- as.numeric(rownames(envdata1))

calibration<-c(8, 10,  2,  6,  7)
total<-c(1:10)

a<-setdiff(total, calibration)

mylist<-list()
  
for (s in a){
  
  
  # set stand age and density
  plot_density<-diameter.totals[[s]]
  observed.a<-envdata[s,6]
  CN <- envdata[s,16]
  aridity <- envdata[s,17]
  temp <- envdata[s,7]
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
        
        
        #growth<-(10^(17.47301 - 0.8562*temp + 1.0450*temp*aridity - 21.1939*aridity))*(0.7405648*age[i,j]^(-0.2594352))
        growth<-(10^(7.588418 -0.37183*temp + 0.45382*temp*aridity -9.20441 *aridity))*(0.7405648*age[i,j]^(-0.2594352))
        # age<-(10^((7.588418/-0.7405648)+ (-0.37183/-0.7405648)*temp + (0.45382/-0.7405648)*temp*aridity 
                     # +(-9.20441/0.7405648)*aridity))*(DIA^(-1/-0.7405648))
        
        
        # define the mortality rate here
        # initialize as a numeric with only 1 possible value
        M <- numeric(length = 1)
        
        # Mortality based on diameter class
        if (Diameter[i,j]>=0) {M<- rbinom(1,1,(1.329e-01  +(-1.075e-02 *Diameter[i,j]*2.54)+ 1.401e-04*((Diameter[i,j]*2.54)^2)))
        }
        # if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
        # else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
        # else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
        # else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
        # else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
        # else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
        # else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
        # else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
        # else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
        # else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
        # else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
        # else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
        # else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
        
        
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
  df<-cbind(observed.d, modeled.d, observed.a, length(plot_density), temp, aridity, observed.tasb, modeled.tasb, 
            sd.tasb, sd.dbh)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature",  "Aridity", "Observed_Biomass", 
                  "Modeled_Biomass", "sd.tasb", "sd.dbh")
  
  # data will be saved as list 1
  mylist[[s]] <- df
  
}

par(mfrow=c(1,1))

final_list_loblolly <- do.call(rbind.data.frame, mylist)
row.names(final_list_loblolly) <- c(a)
# final_list$Plot<- c(1,12,14:18)
# final_list$Plot<-as.factor(final_list$Plot)
sdev<-as.vector(final_list_loblolly$sd.tasb)
sdev.dbh<-as.vector(final_list_loblolly$sd.dbh)

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

plot(data = final_list, Observed_Biomass~Modeled_Biomass, xlim = c(0,4.5), ylim = c(0,4.5),col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="AGB (kgC/m^2)",
     col.axis="#027368", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Biomass-sdev, final_list$Observed_Biomass, final_list$Modeled_Biomass+sdev, final_list$Observed_Biomass, length=0.05, angle=90, code=3)

final_list_loblolly[,"species"]<-"Loblolly"
save(final_list_loblolly, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/final_list_loblolly.rdata")
