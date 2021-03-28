# Run slash pine simulations ----------------------------------------------

rm(list=ls())


setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")


envdata<-read.csv("random_sample_envdata.csv", header=T, sep=",") 
setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement")
load("SlashParameters3.22.21.Rdata")

par(mfrow=c(2,5))

mylist<-list()


simu.diameters<-list()



SOILGRIDS_C_AVG<-((envdata$SOC0_5_NAD/10)*(1/3))+((envdata$SOC5_15_NA/10)*(2/3))

SOILGRIDS_N_AVG<-((envdata$N0_5_NAD83/100)*(1/3))+((envdata$N5_15_NAD8/100)*(2/3))                 

SOILGRIDS_CN<-SOILGRIDS_C_AVG/SOILGRIDS_N_AVG 

envdata$SOILGRIDS_CN_SCALE<-(SOILGRIDS_CN*6.212)+24.634


envdata$aridity<-envdata$ai_et0_NAD*.0001


par.name <- c("a1","b1","b2","b3","b4","b5", "b6", "b7", "b8","sigma")
row.names(sample.parameters)<-par.name

modeled.d<-list()
modeled.tasb<-list()

for (s in 1:5){
  
  # set stand age and density
  plot_density<-700
  observed.a<-80
  temp<-envdata[s,5]
  CN<-envdata[s,10]
  aridity<-envdata[s,11]
  predict.tasb<-matrix(nrow = 300, ncol = 1,0)
  predict.d<-matrix(nrow = 300, ncol = 1,0)
  
  for (o in 1:300){
    age<-matrix(0, nrow=observed.a, ncol=plot_density) # initialize the age matrix
    Diameter<-matrix(0, nrow=observed.a, plot_density) # initialize the diameter matrix
    TASB<-matrix(0, nrow=observed.a, plot_density) # initialize the total above-stump biomass matrix
    
    # initialize the diameter for the first year
    Diameter[1,]<-(10^(sample.parameters["a1",o] - sample.parameters["b1",o]*CN + sample.parameters["b2",o]*CN*aridity  - sample.parameters["b3",o]*aridity 
                       -  sample.parameters["b4",o]*temp))*(sample.parameters["b5",o]*1^(sample.parameters["b6",o]))
    age[1,]<-1
    
    for (j in 1:plot_density){ # specify tree per hectare
      
      for (i in 2:observed.a){ # specify how long to run the simulation (years)
        
        age[i,j]<-age[i-1,j]+1
        
        growth<-(10^(sample.parameters["a1",o] - sample.parameters["b1",o]*CN + sample.parameters["b2",o]*CN*aridity  - sample.parameters["b3",o]*aridity 
                     -  sample.parameters["b4",o]*temp))*(sample.parameters["b5",o]*age[i,j]^(sample.parameters["b6",o]))
        
        # define the mortality rate here
        # initialize as a numeric with only 1 possible value
        M <- numeric(length = 1)
        
        # Mortality based on diameter class
        if (Diameter[i,j]>=0) { M<- rbinom(1,1,(sample.parameters["b7",o]*exp(sample.parameters["b8",o]*Diameter[i,j]*2.54)))}
        
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

# save(simu.diameters, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/slashDIATotals.simu2.4.rdata")
AGB<-do.call(rbind.data.frame, modeled.tasb)
AGB[1:300,"plots"]<-1
AGB[301:600,2]<-2
AGB[601:900,2]<-3
AGB[901:1200,2]<-4
AGB[1201:1500,2]<-5
AGB[,"species"]<-"Slash"
DBH<-do.call(rbind.data.frame, modeled.d)
DBH[1:300,"plots"]<-1
DBH[301:600,2]<-2
DBH[601:900,2]<-3
DBH[901:1200,2]<-4
DBH[1201:1500,2]<-5
DBH[,"species"]<-"Slash"

save(AGB, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/simuSlash30yrAGB.rdata")
save(DBH, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/simuSlash30yrDBH.rdata")
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

plot(data = final_list_slash, Observed_Biomass~Modeled_Biomass, xlim = c(0, 10.5), ylim = c(0, 10.5), col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="AGB (kgC/m^2)",
     col.axis="#027368", pch=16, type="p") 
abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list_slash),data=final_list_slash, cex=0.9, font=2, pos=4)
arrows(final_list_slash$Modeled_Biomass-sdev, final_list_slash$Observed_Biomass, final_list_slash$Modeled_Biomass+sdev, final_list_slash$Observed_Biomass, length=0.05, angle=90, code=3)

final_list_slash[,"species"]<-"Slash"
save(final_list_slash, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/simuSlash30yr.rdata")
