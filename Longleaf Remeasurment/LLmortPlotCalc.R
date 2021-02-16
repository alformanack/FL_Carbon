rm(list=ls())


setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
envdata<-read.csv("LongleafRemeasEnvData.csv", header=T, sep=",") 

setwd("C:/Users/Alicia/Desktop/FL")
load("longleaf_remeas_start.Rdata")
load("longleaf_remeas_end.Rdata")

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

names(envdata)[16]<-"aridity"
names(envdata)[17]<-"TPA_start"
names(envdata)[18]<-"TPA_end"

a<-c(1, 3:23)



for (s in a){

aridity<-envdata[s, 6]*0.0001

temp<-envdata[s, 11]

CN_scale<-envdata[s,15]

envdata[s,16]<-envdata[s, 6]*0.0001

plot_data_start[[s]]<-plots.start[[s]] %>%
  filter(STATUSCD=="1") %>%
  # mutate(TPA=ifelse(is.na(TPA_UNADJ), TPAGROW_UNADJ, TPA_UNADJ)) %>%
  mutate(TPA_total=sum(round(TPA_UNADJ))) %>%
  mutate(age=round((10^(-7.143884 + 0.02970085*CN_scale - 0.03545391*CN_scale*aridity + 4.540362*aridity + 0.137402*temp))*(DIA^1.748652)))
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
  mutate(age=round((10^(-7.143884 + 0.02970085*CN_scale - 0.03545391*CN_scale*aridity + 4.540362*aridity + 0.137402*temp))*(DIA^1.748652))) %>%
  mutate(TASB=((0.0725*((DIA*2.54)^2.5074))+(0.0016*((DIA*2.54)^3.0786))+(0.0214*((DIA*2.54)^2.0051)))*(round(TPA_UNADJ)))
# mutate(TASB=(0.041281*((DIA*2.54)^2.722214))*(round(TPA_UNADJ)))

envdata[s,18]<-unique(plot_data_end[[s]]$TPA_total)

for (h in 1:length(plot_data_end$DIA)){
  diameter.totals.end[[s]]<-rep((plot_data_end[[s]]$DIA), round(plot_data_end[[s]]$TPA_UNADJ))
  age.totals.end[[s]]<-rep((plot_data_end[[s]]$age), round(plot_data_end[[s]]$TPA_UNADJ))
}

}



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
  age.new<-round((10^(-7.143884 + 0.02970085*envdata[g,15] - 0.03545391*envdata[g,16]*envdata[s,15] + 4.540362*envdata[g,16] + 0.137402*envdata[g,11]))*(extra^1.748652))
  age.totals[[g]]<-c(age.start, age.new)
}

}

save(age.totals, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/longleafAgeTotals.rdata")
save(age.totals.end, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/longleafAgeTotalsEnd.rdata")
save(diameter.totals.end, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/longleafDIATotalsEnd.rdata")
save(diameter.totals, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/longleafDIATotals.rdata")
save(plot_data_end, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/longleafPlotEnd.rdata")
save(plot_data_start, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/longleafPlotStart.rdata")
save(diameter.totals, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/longleafDIATotals.rdata")


write.csv(envdata, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/longleafEnvData1.csv")  

# if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.162/8))}
# else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.026/8))}
# else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.006/8))}
# else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.013/8))}
# else if (Diameter[i,j]>15.75 & Diameter[i,j]<=19.69){M<-rbinom(1,1,(.024/8))}
# else if (Diameter[i,j]>19.69 & Diameter[i,j]<=23.62){M<-rbinom(1,1,(.047/8))}
# else if (Diameter[i,j]>23.62 & Diameter[i,j]<=27.56){M<-rbinom(1,1,(.060/8))}
# else if (Diameter[i,j]>27.56){M<-rbinom(1,1,(0.129/8))}

x<-c(5, 15, 25, 35, 45, 55, 65, 75)
x2<-x^2
y<-c((.162/8), (.026/8), (.006/8), (.013/8), (.024/8), (.047/8), (.060/8), (0.129/8))
newx<-seq(0,80,0.5)
newx2<-newx^2

plot(y~x)
mod<-lm(y ~ x+x2)
summary(mod)

new.y <- predict(mod, list(x = newx, x2=newx2),type="response")

plot(x, y, pch = 16, xlab = "DIA", ylab = "p(mort)")
lines(newx, new.y)



