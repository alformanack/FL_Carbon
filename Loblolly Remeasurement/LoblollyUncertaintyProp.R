rm(list=ls())


setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement")
envdata<-read.csv("LoblollyEnvData.csv", header=T, sep=",") 

load("LoblollyAgeTotals.rdata")
load("LoblollyAgeTotalsEnd.rdata")
load("LoblollyDIATotalsEnd.rdata")
load("LoblollyDIATotals.rdata")
load("LoblollyPlotStart.rdata")
load("LoblollyPlotEnd.rdata")


load("initialpar3.22.21.rdata")

# calibration<-c(8, 10,  2,  6,  7)
calibration<-c(8, 1,  5,  3,  10) #new nonrandom

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
        
        
       
        growth<-(10^(initial.par[o,1] + initial.par[o,3]*temp + initial.par[o,4]*temp*aridity + 
                       initial.par[o,2]*aridity))*(initial.par[o,5]*age[i,j]^(initial.par[o,5]-1))
        
        
        # define the mortality rate here
        # initialize as a numeric with only 1 possible value
        M <- numeric(length = 1)
        
        # Mortality based on diameter class
        if (Diameter[i,j]>=0) {
         
          M<- rbinom(1,1,(initial.par[o,6] + (initial.par[o,7]*Diameter[i,j]*2.54) + 
                            initial.par[o,8]*((Diameter[i,j]*2.54)^2)))
        }
        
        
        
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

model.3<-lm(data = final_list_loblolly, Observed_Biomass~Modeled_Biomass)
summary(model.3)

ABG<-lm(data = final_list_loblolly, Observed_Biomass~Modeled_Biomass)
summary(ABG)
DBH<-lm(data = final_list_loblolly, Observed_Diameter~Modeled_Diameter)
summary(DBH)

RMSE<-sqrt(sum((final_list_loblolly$Observed_Biomass-final_list_loblolly$Modeled_Biomass)^2/10))
RMSE.dbh<-sqrt(sum((final_list_loblolly$Observed_Diameter-final_list_loblolly$Modeled_Diameter)^2/10))

plot(data = final_list_loblolly, Observed_Diameter~Modeled_Diameter, xlim = c(0,13), ylim = c(0,13),  xlab="Modeled DBH (in)", ylab="Observed DBH (in)",
     main = "Before parameter correction", col.axis="#027368", col="#75BFBF", pch=16, type="p") 
abline(0,1, col="#048ABF")
text(Observed_Diameter~Modeled_Diameter, labels=rownames(final_list_loblolly),data=final_list_loblolly, cex=0.9, font=2, pos=4)
arrows(final_list_loblolly$Modeled_Diameter-sdev.dbh, final_list_loblolly$Observed_Diameter, final_list_loblolly$Modeled_Diameter+sdev.dbh, final_list_loblolly$Observed_Diameter, length=0.05, angle=90, code=3)

plot(data = final_list_loblolly, Observed_Biomass~Modeled_Biomass, xlim = c(0,4), ylim = c(0,4),col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="AGB (kgC/m^2)",
     col.axis="#027368", pch=16, type="p") 
abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list_loblolly),data=final_list_loblolly, cex=0.9, font=2, pos=4)
arrows(final_list_loblolly$Modeled_Biomass-sdev, final_list_loblolly$Observed_Biomass, final_list_loblolly$Modeled_Biomass+sdev, final_list_loblolly$Observed_Biomass, length=0.05, angle=90, code=3)

final_list_loblolly[,"species"]<-"Loblolly"
save(final_list_loblolly, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/LoblollyInitial.rdata")

