rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
envdata<-read.csv("SlashRemeasEnvData144.csv", header=T, sep=",") 

setwd("C:/Users/Alicia/Desktop/FL")
load("slash_remeas_end144.Rdata")
load("slash_remeas_start144.Rdata")

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

a<-c(1:18,20:21, 23:32, 34:56, 58:65, 67:80, 82:125, 127:132, 134:144)

growth<-(10^(0.9133654 - 0.596886*aridity - 0.004317*CN  - 0.011454*temp +  0.004611*CN*aridity))*(0.5783973*age[i,j]^(-0.4216027))

for (s in a){
  

  temp<-envdata[s, 6]
  
  CN_scale<-envdata[s,15]
  
  envdata[s,16]<-envdata[s, 7]*0.0001
  
  plot_data_start[[s]]<-plots.start[[s]] %>%
    filter(STATUSCD=="1") %>%
    # mutate(TPA=ifelse(is.na(TPA_UNADJ), TPAGROW_UNADJ, TPA_UNADJ)) %>%
    mutate(TPA_total=sum(round(TPA_UNADJ))) %>%
    mutate(age=round((10^((0.9133654/-0.5783973) + (- 0.596886/-0.5783973)*envdata[s,16] + (-0.004317/-0.5783973)*CN_scale + (-0.011454/-0.5783973)*temp 
                          + (0.004611/-0.5783973)*CN_scale*envdata[s,16]))*(DIA^(-1/-0.5783973))))

    # mutate(age=round((10^(-3.636085 + 2.376188*envdata[s,16] + 0.01718542*CN_scale + 0.04559669*temp -0.01835762*CN_scale*envdata[s,16]))*(DIA^1.728915)))
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
    # mutate(age=round((10^(-1.388893 + 0.9240015*envdata[s,16]))*(DIA^1.728915))) %>%
    # mutate(age=round((10^(-3.636085 + 2.376188*envdata[s,16] + 0.01718542*CN_scale + 0.04559669*temp -0.01835762*CN_scale*envdata[s,16]))*(DIA^1.728915))) %>%
    mutate(age=round((10^((0.9133654/-0.5783973) + (- 0.596886/-0.5783973)*envdata[s,16] + (-0.004317/-0.5783973)*CN_scale + (-0.011454/-0.5783973)*temp 
                          + (0.004611/-0.5783973)*CN_scale*envdata[s,16]))*(DIA^(-1/-0.5783973)))) %>%
    mutate(TASB=(0.041281*((DIA*2.54)^2.722214))*(round(TPA_UNADJ)))
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
    # age.new<-round((10^(-1.388893 + 0.9240015*envdata[g,16]))*(extra^1.728915))
    age.new<-round((10^((0.9133654/-0.5783973) + (- 0.596886/-0.5783973)*envdata[g,16] + (-0.004317/-0.5783973)*envdata[g,15] + (-0.011454/-0.5783973)*envdata[g,6] 
                          + (0.004611/-0.5783973)*envdata[g,15]*envdata[g,16]))*(extra^(-1/-0.5783973)))
    # age.new<-round((10^(-3.636085 + 2.376188*envdata[g,16] + 0.01718542*envdata[g,15] + 0.04559669*envdata[g,6] -0.01835762*envdata[g,15]*envdata[s,16]))*(extra^1.728915))
    age.totals[[g]]<-c(age.start, age.new)
  }
  
}

save(age.totals, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/slashAgeTotals2.rdata")
save(age.totals.end, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/slashAgeTotalsEnd2.rdata")
save(diameter.totals.end, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/slashDIATotalsEnd2.rdata")
save(diameter.totals, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/slashDIATotals2.rdata")
save(plot_data_end, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/slashPlotEnd2.rdata")
save(plot_data_start, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/slashPlotStart2.rdata")
save(diameter.totals, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/slashDIATotals2.rdata")


write.csv(envdata, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/Slash Remeasurement/SlashEnvData2.csv")

# if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
# else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
# else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
# else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
# else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}

x<-c(5, 15, 25, 35, 45)
# x2<-x^2
y<-c((.4/8), (.2/8), (.15/8), (.1/8), (.01/8))

newx<-seq(0,45,0.5)
# newx2<-newx^2
# newy<-exp(-2.32784 + (-0.08071*newx))
equation.y<-exp(-2.32784)*exp(-.08071*newx)
mod<-lm(log(y) ~ x)
summary(mod)
plot(mod)

add_nls <- nls(y ~ a*exp(r*x), 
               start = list(a = -2, r = -0.05))
plot(add_nls)
summary(add_nls)
covar<-vcov(add_nls)
MASS::mvrnorm(n=300, Sigma=covar, mu=nls_coef)

new.y <- predict(mod, list(x = newx,),type="response")
nls.y <- predict(add_nls, list(x = x),type="response")

mse<-sum((y-nls.y)^2)
msee<-sqrt(mse/9)


plot(x, y, pch = 16, xlab = "DIA", ylab = "p(mort)")
lines(newx, new.y)
lines(newx, equation.y)

lm_coef <- coef(mod)
nls_coef <- coef(add_nls)

# make the plot
plot(x, y)
lines(newx, exp(lm_coef[1])*exp(lm_coef[2]*newx), col = "dodgerblue", lwd = 2)
lines(newx, nls_coef[1]*exp(nls_coef[2]*newx), col = "orange2", lwd = 2)
legend("topright", col = c("dodgerblue", "orange2"), 
       legend = c("lm fit", "nls fit"), lwd = 3)

x<-c(1.968504, 5.905512, 9.84252, 13.77953, 17.71654, 21.65354, 25.59055, 29.52756)
x2<-x^2
y<-c((.162), (.026), (.006), (.013), (.024), (.047), (.060), (0.129))
newx<-seq(0,30,0.5)
newx2<-newx^2

plot(y~x)
mod<-lm(y ~ x+x2)
summary(mod)

