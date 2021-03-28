rm(list=ls())


setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
envdata<-read.csv("LoblollyRemeasEnvData.csv", header=T, sep=",") 

setwd("C:/Users/Alicia/Desktop/FL")
load("loblolly_remeas_start.Rdata")
load("loblolly_remeas_end.Rdata")

envdata1<-subset(envdata, envdata$TPA_start<envdata$TPA_end)
a<- rownames(envdata1)

envdata$SOILGRIDS_C_AVG<-((envdata$SOC0_5_NAD/10)*(1/3))+((envdata$SOC5_15NAD/10)*(2/3))

envdata$SOILGRIDS_N_AVG<-((envdata$N0_5_NAD/100)*(1/3))+((envdata$N5_15_NAD/100)*(2/3))                 


envdata$SOILGRIDS_CN<-envdata$SOILGRIDS_C_AVG/envdata$SOILGRIDS_N_AVG 

envdata$SOILGRIDS_CN_SCALE<-(envdata$SOILGRIDS_CN*6.212)+24.634

diameter.totals<-list()
age.totals<-list()
diameter.totals.end<-list()
age.totals.end<-list()
plot_data_end<-list()
plot_data_start<- list()



a<-c(1:10)

# growth<-(10^(1.774252 -0.007376*CN + 0.008805*CN*aridity -1.127642*aridity -0.034125*temp))*(0.5718692*age[i,j]^(-0.4281308))

for (s in a){
  
  aridity<-envdata[s, 7]*0.0001
  
  temp<-envdata[s, 6]
  
  CN_scale<-envdata[s,15]
  
  envdata[s,16]<-envdata[s, 7]*0.0001
  
  plot_data_start[[s]]<-plots.start[[s]] %>%
    filter(STATUSCD=="1") %>%
    # mutate(TPA=ifelse(is.na(TPA_UNADJ), TPAGROW_UNADJ, TPA_UNADJ)) %>%
    mutate(TPA_total=sum(round(TPA_UNADJ))) %>%
    mutate(age=round(10^((7.588418/-0.7405648)+ (-0.37183/-0.7405648)*temp + (0.45382/-0.7405648)*temp*aridity 
                         +(-9.20441/-0.7405648)*aridity)*(DIA^(-1/-0.7405648))))
  # mutate(age=round((10^(-7.143884 + 0.02970085*CN_scale - 0.03545391*CN_scale*aridity + 4.540362*aridity + 0.137402*temp))*(DIA^1.748652)))
  # mutate(age=round((10^(-3.018915 + 1.2741*aridity))*(DIA^2.307065)))  
  
  envdata[s,17]<-unique(plot_data_start[[s]]$TPA_total)
  
  
  for (h in 1:length(plot_data_start$DIA)){
    diameter.totals[[s]]<-rep((plot_data_start[[s]]$DIA), round(plot_data_start[[s]]$TPA_UNADJ))
    age.totals[[s]]<-rep((plot_data_start[[s]]$age), round(plot_data_start[[s]]$TPA_UNADJ))
  }
  
  # hist(diameter.totals[[s]], main = paste("Start plot", s), xlab = "Diameter (in)")
  
  plot_data_end[[s]]<-plots.end[[s]] %>%
    filter(STATUSCD=="1") %>%
    # mutate(TPA=ifelse(is.na(TPA_UNADJ), TPAGROW_UNADJ, TPA_UNADJ)) %>%
    mutate(TPA_total=sum(round(TPA_UNADJ))) %>%
    mutate(age=round(10^((7.588418/-0.7405648)+ (-0.37183/-0.7405648)*temp + (0.45382/-0.7405648)*temp*aridity 
                         +(-9.20441/-0.7405648)*aridity)*(DIA^(-1/-0.7405648)))) %>%
    # mutate(age=round((10^(-7.143884 + 0.02970085*CN_scale - 0.03545391*CN_scale*aridity + 4.540362*aridity + 0.137402*temp))*(DIA^1.748652))) %>%
    mutate(TASB=(0.037403*((DIA*2.54)^2.676835))*(round(TPA_UNADJ)))
  # mutate(TASB=(0.041281*((DIA*2.54)^2.722214))*(round(TPA_UNADJ)))
  
  envdata[s,18]<-unique(plot_data_end[[s]]$TPA_total)
  
  for (h in 1:length(plot_data_end$DIA)){
    diameter.totals.end[[s]]<-rep((plot_data_end[[s]]$DIA), round(plot_data_end[[s]]$TPA_UNADJ))
    age.totals.end[[s]]<-rep((plot_data_end[[s]]$age), round(plot_data_end[[s]]$TPA_UNADJ))
  }
  
}

names(envdata)[16]<-"aridity"
names(envdata)[17]<-"TPA_start"
names(envdata)[18]<-"TPA_end"

diameters.new<-vector()
extra<-vector()
diff<-vector()
age.new<-vector()
age.start<-vector()

for (g in a){
  
  if ((length(diameter.totals.end[[g]])-length(diameter.totals[[g]]))>0) {
    diameters.new<-diameter.totals[[g]]
    diff<-length(diameter.totals.end[[g]])-length(diameter.totals[[g]])
    extra<-runif(diff, 0, 5)
    diameter.totals[[g]]<-c(diameters.new, extra)
    age.start<-age.totals[[g]]
    # age.new<-round((10^(-9.35027 + 0.03729744*envdata[g,15] - 0.04455875*envdata[g,16]*envdata[s,15] + 5.373766*envdata[g,16] + 0.1510293*envdata[g,11]))*(extra^2.257168))
    age.new<-round(10^((7.588418/-0.7405648)+ (-0.37183/-0.7405648)*envdata[g,6] + (0.45382/-0.7405648)*envdata[g,6]*envdata[g,16] 
                       +(-9.20441/-0.7405648)*envdata[g,16])*(extra^(-1/-0.7405648)))
    # age.new<-round((10^(-7.143884 + 0.02970085*envdata[g,15] - 0.03545391*envdata[g,16]*envdata[g,15] + 4.540362*envdata[g,16] + 0.137402*envdata[g,11]))*(extra^1.748652))
    age.totals[[g]]<-c(age.start, age.new)
  }
  
}

save(age.totals, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/LoblollyAgeTotals.rdata")
save(age.totals.end, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/LoblollyAgeTotalsEnd.rdata")
save(diameter.totals.end, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/LoblollyDIATotalsEnd.rdata")
save(diameter.totals, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/LoblollyDIATotals.rdata")
save(plot_data_end, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/LoblollyPlotEnd.rdata")
save(plot_data_start, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/LoblollyPlotStart.rdata")
save(diameter.totals, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/LoblollyDIATotals.rdata")


write.csv(envdata, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/LoblollyEnvData.csv")  



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
# else if (Diameter[i,j]>c){M<-rbinom(1,1,0.44)}

x<-c(4, 8, 12, 16, 20, 24, 50, 65, 70, 75, 80, 85, 90)

x2<-x^2

y<-c(.1, 0.05, .028, .017, .009, .0076, .0045, .0098, .018, .04, .089, .22, .44)

mort<-lm(y~x +x2)
summary(mort)
plot(mort)
