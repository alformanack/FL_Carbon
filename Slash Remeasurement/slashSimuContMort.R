# Run slash pine simulations ----------------------------------------------

rm(list=ls())


setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement")
# envdata<-read.csv("SlashEnvData1.csv", header=T, sep=",") 
# load("SlashAgeTotals.rdata")
# load("SlashAgeTotalsEnd.rdata")
# load("SlashDIATotalsEnd.rdata")
# load("SlashDIATotals.rdata")
# load("SlashPlotStart.rdata")
# load("SlashPlotEnd.rdata")
envdata<-read.csv("SlashEnvData2.csv", header=T, sep=",") 
load("SlashAgeTotals2.rdata")

load("SlashAgeTotalsEnd2.rdata")
load("SlashDIATotalsEnd2.rdata")
load("SlashDIATotals2.rdata")
load("SlashPlotStart2.rdata")
load("SlashPlotEnd2.rdata")

par(mfrow=c(2,5))

mylist<-list()

simu.diameters<-list()

# total_a<-c(1:18,20:21, 23:32, 34:56)
total_a<-c(1:18,20:21, 23:32, 34:56, 58:65, 67:80, 82:125, 127:132, 134:144)

exclude<-c(6,7,15,18,21,29,37,41,52,54,55)

a<-setdiff(total_a,exclude)

envdata1<-subset(envdata, envdata$TPA_start<=envdata$TPA_end)
a1<- as.numeric(rownames(envdata1))

envdata2<-subset(envdata, envdata$TPA_end/envdata$TPA_start>=.9)

total.calibration<-sample(a1, 40, replace = FALSE)

calibration<-c(44,  45, 142, 101,  24, 141, 119,  77,   1, 110, 115,  20,  92,  90,  68,  14, 130, 111,  30,
                28,  87,  86, 107,  39, 143, 102,  71, 118,  64,  93, 128,  75,  58,  80, 116,  62,  61,  38,
               117,  46)


a<-setdiff(a,calibration)


for (s in a){
  
  # set stand age and density
  plot_density<-diameter.totals[[s]]
  observed.a<-envdata[s,6]
  ages<-age.totals[[s]]
  temp<-envdata[s,7]
  CN<-envdata[s,16]
  aridity<-envdata[s,17]
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
        
        # growth<-(10^(1.308552 - 0.55226*aridity))*(0.4334511*age[i,j]^(-0.5665489))
        # growth<-(10^(0.8033318 - 0.53444*envdata[s, 17]))*(0.5783973*age[i,j]^(-0.4216027))
        # previousgrowth<-(10^(2.103102 - 1.374381*aridity - 0.009940*CN  - 0.026373*temp + 0.010618*CN*aridity))*(0.5783973*age[i,j]^(-0.4216027))
        growth<-(10^(0.9133654 - 0.596886*aridity - 0.004317*CN  - 0.011454*temp +  0.004611*CN*aridity))*(0.5783973*age[i,j]^(-0.4216027))
        # growth<-(10^(2.103289 - 0.015004*CN_scale - 1.435603*aridity + 0.016463*CN_scale*aridity))*(0.4334511*age[i,j]^(-0.5665489))
        # growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j]^(-0.4215787))
        
        # define the mortality rate here
        # initialize as a numeric with only 1 possible value
        M <- numeric(length = 1)
        
        # Mortality based on diameter class
        if (Diameter[i,j]>=0) { M<- rbinom(1,1,(0.06504789*exp(-0.05577336*Diameter[i,j]*2.54)))}
        # else {M=0}
        # v<-10*2.54
        # (0.06504789*exp(-0.05577336*v*2.54))
        # if (Diameter[i,j]>=0) { M<- rbinom(1,1,(0.0859693*exp(-0.05577336*Diameter[i,j])))}
        # if (Diameter[i,j]>0) { M<- rbinom(1,1,(1/200))}
        
        # if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
        # else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
        # else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
        # else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
        # else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
        # 
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

# save(simu.diameters, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/slashDIATotals.simu2.4.rdata")

par(mfrow=c(1,1))

final_list_slash <- do.call(rbind.data.frame, mylist)
row.names(final_list_slash) <- c(a)
# final_list$Plot<- c(1,12,14:18)
# final_list$Plot<-as.factor(final_list$Plot)
sdev<-as.vector(final_list_slash$sd.tasb)
sdev.dbh<-as.vector(final_list_slash$sd.dbh)

model.1<-lm(data = final_list_slash, log10(Observed_Biomass/Modeled_Biomass)~Tree_Density)
summary(model.1)

mod<-lm(data=final_list_slash, Observed_Biomass~Modeled_Biomass)
summary(mod)
# plot(Temperature~Aridity, data= final_list[[1]])
# 
# res.age<-residuals(model.1)
# plot(res.age~df2$Age)
# plot(res.age~df2$Aridity)

model.2<-lm(data = final_list_slash, log10(Observed_Diameter/Modeled_Diameter)~Tree_Density)
summary(model.2)


plot(data = final_list_slash, Observed_Diameter~Modeled_Diameter,  xlim = c(2,9), ylim = c(2,9), xlab="Modeled DBH (in)", ylab="Observed DBH (in)",
     main = "Before parameter correction", col.axis="#027368", col="#75BFBF", pch=16, type="p") 
abline(0,1, col="#048ABF")

text(Observed_Diameter~Modeled_Diameter, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list_slash$Modeled_Diameter-sdev.dbh, final_list_slash$Observed_Diameter, final_list_slash$Modeled_Diameter+sdev.dbh, final_list_slash$Observed_Diameter, length=0.05, angle=90, code=3)

plot(data = final_list_slash, Observed_Biomass~Modeled_Biomass,col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="AGB (kgC/m^2)",
     col.axis="#027368", pch=16, type="p") 
abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list_slash),data=final_list_slash, cex=0.9, font=2, pos=4)
arrows(final_list_slash$Modeled_Biomass-sdev, final_list_slash$Observed_Biomass, final_list_slash$Modeled_Biomass+sdev, final_list_slash$Observed_Biomass, length=0.05, angle=90, code=3)

final_list_slash[,"species"]<-"Slash"
save(final_list_slash, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/final_list_slash.rdata")
