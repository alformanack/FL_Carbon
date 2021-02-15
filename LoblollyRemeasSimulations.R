# Run longleaf pine simulations ----------------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
envdata<-read.csv("LoblollyRemeasEnvData.csv", header=T, sep=",") 

setwd("C:/Users/Alicia/Desktop/FL")
load("loblolly_remeas_start.Rdata")
load("loblolly_remeas_end.Rdata")

# envdata$PLT_CN<-sort(envdata$PLT_CN, decreasing = FALSE)
# 
# envdata$PLT_CN<-as.character(envdata$PLT_CN)


par(mfrow=c(2,5))

mylist<-list()


# Plot 170 ----------------------------------------------
diameter.totals<-list()
age.totals<-list()
diameter.totals.end<-list()
age.totals.end<-list()
plot_data_end<-list()
plot_data_start<- list()

envdata$SOILGRIDS_C_AVG<-((envdata$SOC0_5_NAD/10)*(1/3))+((envdata$SOC5_15NAD/10)*(2/3))

envdata$SOILGRIDS_N_AVG<-((envdata$N0_5_NAD/100)*(1/3))+((envdata$N5_15_NAD/100)*(2/3))                 


envdata$SOILGRIDS_CN<-envdata$SOILGRIDS_C_AVG/envdata$SOILGRIDS_N_AVG 

envdata$SOILGRIDS_CN_SCALE<-(envdata$SOILGRIDS_CN*6.212)+24.634

a<-c(1:10)

for (s in a){
  
  aridity<-envdata[s, 7]*0.0001
  
  temp<-envdata[s, 6]
  
  CN_scale<-envdata[s,15]
  
  plot_data_start[[s]]<-plots.start[[s]] %>%
    filter(STATUSCD=="1") %>%
    # mutate(TPA=ifelse(is.na(TPA_UNADJ), TPAGROW_UNADJ, TPA_UNADJ)) %>%
    mutate(TPA_total=sum(round(TPA_UNADJ))) %>%
    mutate(age=round((10^(-23.59417 + 1.156145*temp - 1.411085*temp*aridity + 28.61856*aridity))*(DIA^1.350321)))
 
  
  
  for (h in 1:length(plot_data_start$DIA))
  {diameter.totals[[s]]<-rep((plot_data_start[[s]]$DIA), round(plot_data_start[[s]]$TPA_UNADJ))
  age.totals[[s]]<-rep((plot_data_start[[s]]$age), round(plot_data_start[[s]]$TPA_UNADJ))}
  hist(diameter.totals[[s]], main = paste("Start plot", s), xlab = "Diameter (in)")
  
  plot_data_end[[s]]<-plots.end[[s]] %>%
    filter(STATUSCD=="1") %>%
    # mutate(TPA=ifelse(is.na(TPA_UNADJ), TPAGROW_UNADJ, TPA_UNADJ)) %>%
    mutate(TPA_total=sum(round(TPA_UNADJ))) %>%
    mutate(age=round((10^(-23.59417 + 1.156145*temp - 1.411085*temp*aridity + 28.61856*aridity))*(DIA^1.350321))) %>%
    mutate(TASB=(0.037403*((DIA*2.54)^2.676835)))

  
  
  for (h in 1:length(plot_data_end$DIA))
  {diameter.totals.end[[s]]<-rep((plot_data_end[[s]]$DIA), round(plot_data_end[[s]]$TPA_UNADJ))
  age.totals.end[[s]]<-rep((plot_data_end[[s]]$age), round(plot_data_end[[s]]$TPA_UNADJ))}
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
  observed.a<-envdata[s,5]
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
        
        
        growth<-(10^(17.47301 - 0.8562*temp + 1.0450*temp*aridity - 21.1939*aridity))*(0.7405648*age[i,j]^(-0.2594352))
        
        
        # define the mortality rate here
        # initialize as a numeric with only 1 possible value
        M <- numeric(length = 1)
        
        # Mortality based on diameter class
        if (Diameter[i,j]<=1.574803){ M<- rbinom(1,1,.1)}
        else if (Diameter[i,j]>1.574803 & Diameter[i,j]<=3.149606){M<-rbinom(1,1,.05)}
        else if (Diameter[i,j]>3.149606 & Diameter[i,j]<=4.724409){M<-rbinom(1,1,.028)}
        else if (Diameter[i,j]>4.724409 & Diameter[i,j]<=6.299213){M<-rbinom(1,1,.017)}
        else if (Diameter[i,j]>6.299213 & Diameter[i,j]<=7.874016){M<-rbinom(1,1,.009)}
        else if (Diameter[i,j]>7.874016 & Diameter[i,j]<=9.448819){M<-rbinom(1,1,.0076)}
        else if (Diameter[i,j]>9.448819 & Diameter[i,j]<=19.68504){M<-rbinom(1,1,.0045)}
        else if (Diameter[i,j]>19.68504 & Diameter[i,j]<=25.59055){M<-rbinom(1,1,.0098)}
        else if (Diameter[i,j]>25.59055 & Diameter[i,j]<=27.55906){M<-rbinom(1,1,.018)}
        else if (Diameter[i,j]>27.55906 & Diameter[i,j]<=29.52756){M<-rbinom(1,1,.04)}
        else if (Diameter[i,j]>29.52756 & Diameter[i,j]<=31.49606){M<-rbinom(1,1,.089)}
        else if (Diameter[i,j]>31.49606 & Diameter[i,j]<=33.46457){M<-rbinom(1,1,.22)}
        else if (Diameter[i,j]>35.43307){M<-rbinom(1,1,0.44)}
        
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

plot(data = final_list, Observed_Biomass~Modeled_Biomass, xlim = c(0,4), ylim = c(0,4),col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="AGB (kgC/m^2)",
     col.axis="#027368", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=rownames(final_list),data=final_list, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Biomass-sdev, final_list$Observed_Biomass, final_list$Modeled_Biomass+sdev, final_list$Observed_Biomass, length=0.05, angle=90, code=3)
