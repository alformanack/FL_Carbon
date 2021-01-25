# Run slash pine simulations ----------------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
envdata<-read.csv("slashRemeasPlots.csv", header=T, sep=",") %>%
  mutate(PLT_CN=finalSummary$PLT_CN) %>%
  select(-OID, -Field1) 
view(envdata)

load("slashStartPlots.Rdata")
load("slashEndPlots")

envdata$PLT_CN<-sort(envdata$PLT_CN, decreasing = FALSE)

envdata$PLT_CN<-as.character(envdata$PLT_CN)


par(mfrow=c(2,5))

mylist<-list()

# Plot 170 ----------------------------------------------
diameter.totals<-list()
age.totals<-list()
diameter.totals.end<-list()
age.totals.end<-list()
plot_data_end<-list()


for (s in 1:21){
  
  aridity<-envdata[s, 12]*0.0001
  
  temp<-envdata[s, 11]
  
  temp2<-envdata[s, 11]^2
  
  plot_data<-startPlots[[s]] %>%
    filter(STATUSCD=="1") %>%
    # mutate(TPA=ifelse(is.na(TPA_UNADJ), TPAGROW_UNADJ, TPA_UNADJ)) %>%
    mutate(TPA_total=sum(round(TPA_UNADJ))) %>%
    mutate(age=round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity)*(DIA^1.728844))))  

    for (h in 1:length(plot_data$DIA))
      {diameter.totals[[s]]<-rep((plot_data$DIA), round(plot_data$TPA_UNADJ))
        age.totals[[s]]<-rep((plot_data$age), round(plot_data$TPA_UNADJ))}
  hist(diameter.totals[[s]], main = paste("Start plot", s), xlab = "Diameter (in)")
      
  plot_data_end[[s]]<-endPlots[[s]] %>%
    filter(secondTreeStatus=="1") %>%
    # mutate(TPA=ifelse(is.na(TPA_UNADJ), TPAGROW_UNADJ, TPA_UNADJ)) %>%
    mutate(TPA_total=sum(round(secondTPA))) %>%
    mutate(age=round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity)*(secondDIA^1.728844)))) %>%
    mutate(TASB=(0.041281*((secondDIA*2.54)^2.722214))*(round(secondTPA)))
  

    for (h in 1:length(plot_data_end$DIA))
    {diameter.totals.end[[s]]<-rep((plot_data_end[[s]]$secondDIA), round(plot_data_end[[s]]$secondTPA))
      age.totals.end[[s]]<-rep((plot_data_end[[s]]$age), round(plot_data_end[[s]]$secondTPA))}
  hist(diameter.totals.end[[s]], main =paste("End plot", s), xlab = "Diameter (in)")
      




# # set parameters for growth equation
# aridity<-envdata[a, 7]*0.0001
# 
# temp<-envdata[a, 6]
# 
# temp2<-envdata[a, 6]^2
# 
# # C_AVG<-((envdata[1,11]/10)*(1/3))+((envdata[1,12]/10)*(2/3))
# # 
# # N_AVG<-((envdata[1,13]/100)*(1/3))+((envdata[1,14]/100)*(2/3))
# # 
# # CN<-C_AVG/N_AVG
# # 
# # CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-diameter.totals[[s]]
observed.a<-envdata[s,9]
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
  predict.tasb[o,1]<-sum(TASB[observed.a, ])*.5
}

observed.d<-mean(diameter.totals[[s]])
observed.tasb<-sum(plot_data_end[[s]]$TASB)*.5
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

final_list <- do.call(rbind.data.frame, mylist)
row.names(final_list_slash) <- c(as.vector(envdata[,9]))
# final_list$Plot<- c(1,12,14:18)
# final_list$Plot<-as.factor(final_list$Plot)
sdev<-as.vector(final_list$sd.tasb)
sdev.dbh<-as.vector(final_list$sd.dbh)

plot(data = final_list, Observed_Diameter~Modeled_Diameter, xlim = c(2,13), ylim = c(2,13), xlab="Modeled DBH (in)", ylab="Observed DBH (in)",
     main = "Before parameter correction", col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Diameter~Modeled_Diameter, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Diameter-sdev.dbh, final_list$Observed_Diameter, final_list$Modeled_Diameter+sdev.dbh, final_list$Observed_Diameter, length=0.05, angle=90, code=3)

plot(data = final_list, Observed_Biomass~Modeled_Biomass, xlim = c(0,30500), ylim = c(0,30500), col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="Tree Carbon (gC/m^2)",
     col.axis="#027368", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Biomass-sdev, final_list$Observed_Biomass, final_list$Modeled_Biomass+sdev, final_list$Observed_Biomass, length=0.05, angle=90, code=3)
