rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")

# Load dataset ------------------------------------------------------------

sitetree<-read.csv("aridity2.csv", header=T, sep=",", stringsAsFactors = F)

sitetree$SPCD<-as.factor(sitetree$SPCD)

# Calculate soil C:N for ORNL ---------------------------------------------

sitetree$ORNL_CN<-sitetree$SOC_ORNL_kgperm2*1000/sitetree$N_ORNL_gperm2

# Calculate weighted averages and soil C:N for Soilgrids data -------------

sitetree$SOILGRIDS_C_AVG<-(sitetree$Soilgrids_C_0.5/10)*(1/3)+((sitetree$Soilgrids_C_5.15/10)*(2/3))

sitetree$SOILGRIDS_N_AVG<-(sitetree$Soilgrids_N_0.5/100)*(1/3)+((sitetree$Soilgrids_N_5.15/100)*(2/3))                 

sitetree$SOILGRIDS_CN<-sitetree$SOILGRIDS_C_AVG/sitetree$SOILGRIDS_N_AVG 

sitetree[which(is.finite(sitetree$SOILGRIDS_CN)==F),"SOILGRIDS_CN"]<-NA

sitetree$FIA_CN_RATIO<-as.numeric(sitetree$FIA_CN_RATIO)

# Comparing soils data with FIA -------------------------------------------

plot(sitetree$ORNL_CN, sitetree$FIA_CN_RATIO)                 

plot(sitetree$SOILGRIDS_CN, sitetree$FIA_CN_RATIO)

model<-lm(FIA_CN_RATIO~SOILGRIDS_CN, data=sitetree)
summary(model)

#scale CN from FIA data
sitetree$SOILGRIDS_CN_SCALE<-(sitetree$SOILGRIDS_CN*6.045)+19.775

plot(sitetree$SOILGRIDS_CN_SCALE, sitetree$FIA_CN_RATIO)
model<-lm(FIA_CN_RATIO~SOILGRIDS_CN_SCALE, data=sitetree)
summary(model)

# normalizing predictor variables ---------------------------------------------

sitetree$logDIA<-log10(sitetree$DIA)

sitetree$logSOILGRIDS_CN_SCALE<-log10(sitetree$SOILGRIDS_CN_SCALE)

sitetree$logAGEDIA<-log10(sitetree$AGEDIA)
hist(sitetree$DIA)
hist(sitetree$logDIA)
plot(sitetree$logDIA~sitetree$AVG_TEMP_bioclim)
hist(sitetree$AVG_TEMP_bioclim)
hist(sqrt(sitetree$AVG_TEMP_bioclim))
hist(sitetree$temprange_1)
hist(log10(sitetree$temprange_1))

# removing aridity error
sitetree<-sitetree[-c(1336), ]

sitetree$aridity_1<-sitetree$aridity_1*.0001

summary(sitetree$aridity_1)

# Subsetting species ------------------------------------------------------

slashpine<-subset(sitetree, sitetree$SPCD=="111")

longleaf<-subset(sitetree, sitetree$SPCD=="121")

loblolly<-subset(sitetree, sitetree$SPCD=="131")

# creating models for slashpine
slashfullmodel<-lm(logDIA~logAGEDIA + logSOILGRIDS_CN_SCALE + aridity_1 + slashpine$AVG_TEMP_bioclim, data=slashpine)
slashfullmodel<-lm(logDIA~logAGEDIA + aridity_1 + slashpine$AVG_TEMP_bioclim, data=slashpine)
summary(slashfullmodel)

slashpine$arid2<-slashpine$aridity_1^2
slash.aridity<-lm(logDIA~logAGEDIA + arid2 + slashpine$aridity_1, data=slashpine)
summary(slash.aridity)

slashprecip<-lm(logDIA~AVG_TEMP_bioclim + logAGEDIA + logSOILGRIDS_CN_SCALE + slashpine$precip_1, data=slashpine)
summary(slashprecip)

slashpine$precip2<-slashpine$precip_1^2
slashpine$precip3<-slashpine$precip_1^3
slashprecip2<-lm(logDIA~precip2+precip3+logAGEDIA+slashpine$precip_1, data=slashpine)
summary(slashprecip2)

plot(slashpine$logDIA~slashpine$precip_1)
hist(sitetree$precip_1)
hist(log10(sitetree$precip_1))

slashpine$temp2<-slashpine$AVG_TEMP_bioclim^2
slashpinemodel2<-lm(logDIA~AVG_TEMP_bioclim+aridity_1+logAGEDIA, data=slashpine)
slashtemp<-lm(logDIA~AVG_TEMP_bioclim + temp2 + aridity_1 + logAGEDIA, data=slashpine)
summary(slashtemp)

slash.res<-residuals(slashpinemodel2)
plot(slash.res~slashpine$AVG_TEMP_bioclim)

x<-slashpine$AVG_TEMP_bioclim
curve<--.004428*x^2+.1872*x-1.298
plot(x,curve)
# looks like temperature and aridity are correlated
pairs(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+slashpine$aridity_1, data=slashpine)

vif(slashpinemodel2)

# creating models for longleaf pine
longleafmodel1<-lm(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+aridity_1, data=longleaf)

summary(longleafmodel1)
vif(longleafmodel1)

# creating models for loblolly pine
loblollymodel1<-lm(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+aridity_1, data=(loblolly))
summary(loblollymodel1)

loblollymodel2<-lm(logDIA~logAGEDIA+aridity_1, data=(loblolly))
summary(loblollymodel2)

# call coefficients of models
slash<-slashpinemodel2$coefficients

long<-longleafmodel1$coefficients

lob<-loblollymodel2$coefficients

# create growth equations
slashgrowth<-(10^slash[1])*(10^(slash[2]*20))*(slash[4]*slashpine$AGEDIA^-.649)*(10^(slash[3]*.8753))

longleafgrowth<-(10^long[1])*(10^(long[2]*20))*(long[3]*longleaf$AGEDIA^-.608)*(10^(long[5]*.8753))*(51^long[4])

loblollygrowth<-(10^(lob[1]))*(10^(lob[3]*.8753))*(lob[2]*loblolly$AGEDIA^-.535)  


# Growth rate plots -------------------------------------------------------
par(mfrow=c(1,1))
plot(slashpine$AGEDIA, slashgrowth, cex.axis=1.5, col.axis="#027368", ann=FALSE, col="#75BFBF", pch=16, type="p", font=2)
mtext(side = 1, text = "Age (yr)", line = 3, cex=1.5)
mtext(side = 3, text = "Growth Rates", line = 1, cex=1.5)
mtext(side = 2, text = "Growth (in/yr)", line = 2, cex=1.5)
lines(longleaf$AGEDIA, longleafgrowth, type="p", pch=16, col="#048ABF")
lines(loblolly$AGEDIA, loblollygrowth, type="p", col="#A7C8F2", pch=16)
legend(90,.2, legend=c("Loblolly", "Longleaf","Slashpine"), col=c("#A7C8F2", "#048ABF","#75BFBF"), pch=16, cex=1.5)
age<-seq(1,100,1)
lines(age, long[3]*age^.9, type="p", pch=16, col="#048ABF")


# Run slash pine simulations ----------------------------------------------

rm(list=ls())

# Gholz 1982 ----------------------------------------------

# load observed data for age and diameter
load(file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/bradford county simulation/models.Rdata") 

# data will be saved as list 1
mylist<-list() 

# Aridity averages per sub county
Lawtey<-8244.150862068966
Starke<-8203.438775510203
Hampton<-8100.927419354839
Brooker<-8155.30223880597

# set parameters for growth equation
aridity<-((Lawtey + Starke + Hampton + Brooker)/4)*0.0001

temp<-20.05

C_AVG<-(418.4782/10)*(1/3)+((156.6189/10)*(2/3))

N_AVG<-(1177.5531/100)*(1/3)+((459.7207/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*6.045)+19.775


# set plot densities for simulations
plot_density<-c(1392,1280,1648,1696,1728,1792,880,1080,1280,
                1008,1040,784,928,1184,1056,1056,928,976)

# initialize matrix for modeled diameter
d <- matrix(0, ncol = 1, nrow = length(observed.a))


for(t in 1:length(observed.a)) {
  
  age<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the diameter matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.8156395 - 0.004348023*temp - 0.2872396*aridity))*(0.3535108*1^(-0.6464892))
  
  
  
  for (j in 1:plot_density[t]){ # for each tree (num of trees = plot_density[t]) 
    
    
    for (i in 2:observed.a[t]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.8156395 - 0.004348023*temp - 0.2872396*aridity))*(0.3535108*age[i,j]^(-0.6464892))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Get the value for M given the diameter from the previous year
      if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
      else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
      else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
      else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
      else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  d[t,] <- mean(Diameter[observed.a[t,],])  # take the mean of the last row (observed.a[t,])
}

# set up dataframe to store simulated data
Temperature<-rep(20.05, 18)
Aridity<-rep(aridity, 18)
CN<-rep(CN_SCALE, 18)

df<-cbind(observed.d, d, observed.a, plot_density, Temperature, Aridity, CN)

df<-data.frame(df)

colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Aridity", "Soil CN")

mylist$Bradford_County <- df


# Roth 2007 simulation ----------------------------------------------------

# set parameters for growth equation
temp<-(20.30676 + 19.65989)/2

C_AVG<-(634.5/10)*(1/3)+((231/10)*(2/3))

N_AVG<-(1245.5/100)*(1/3)+((412/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*6.045)+19.775

# aridity values
Waldo_FL<-8058

Perry_FL<- 9011

aridity<-((Waldo_FL + Perry_FL)/2)*0.0001

# set plot density and stand age
plot_density<-c(1334, 2990, 1334, 2990, 1334, 2990)
observed.a<-c(2, 2, 3, 3, 5, 5)
observed.d<-c(0.94488, 1.023622047, 2.165354331, 2.007874016, 4.133858268, 3.42519685)

# initialize matrix to save average modeled diameter
d <- matrix(0, ncol = 1, nrow = length(observed.a))


for(t in 1:length(observed.a)) {
  
  age<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the diameter matrix
  
  # initialize the diameter for the first year
  #Diameter[1,]<-(10^(.381+.012*20.05-.0001*266.82))*(.346*1^(-.654))*CN_SCALE^(-.069)
  Diameter[1,]<-(10^(0.8156395 - 0.004348023*temp - 0.2872396*aridity))*(0.3535108*1^(-0.6464892))
  
  
  for (j in 1:plot_density[t]){
    
    
    for (i in 2:observed.a[t]){
      age[i,j]<-age[i-1,j]+1
      growth<-(10^(0.8156395 - 0.004348023*temp - 0.2872396*aridity))*(0.3535108*age[i,j]^(-0.6464892))
      
      
      if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
      else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
      else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
      else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
      else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
      
      
      Diameter[i,j]<-Diameter[i-1,j]+growth-M*(Diameter[i-1,j]+growth)
      
      #TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
      
      if (M==1){
        age[i,j]<-0
      }
    }
  }
  d[t,] <- mean(Diameter[observed.a[t],])  # take the mean of the last row (observed.a[t,])
}

# set up dataframe to store simulated data
Temperature<-rep(temp, 6)
Aridity<-rep(aridity, 6)
CN<-rep(CN_SCALE, 6)


df<-cbind(observed.d, d, observed.a, plot_density, Temperature, Aridity, CN)

df<-data.frame(df)

colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Aridity", "Soil CN")

# data will be saved as list 2
mylist$Roth_2007 <- df


# Jokela 2010 simulation  -------------------------------------------------

# set paramenter for growth equation
C_AVG<-(485/10)*(1/3)+((151/10)*(2/3))

N_AVG<-(1722/100)*(1/3)+((876/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*6.045)+19.775

temp<-20.76

aridity<-8139*0.0001

# initialize matrix for modeled age and modeled diameter
age<-matrix(0, nrow=25, ncol=946)
Diameter<-matrix(0, nrow=25, ncol=946)

# initialize first diameter
Diameter[1,]<-(10^(0.8156395 - 0.004348023*temp - 0.2872396*aridity))*(0.3535108*1^(-0.6464892))

# start simulation
for (j in 1:946){
  
  
  for (i in 2:25){
    age[i,j]<-age[i-1,j]+1
    growth<-(10^(0.8156395 - 0.004348023*temp - 0.2872396*aridity))*(0.3535108*age[i,j]^(-0.6464892))
    
    if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
    else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
    else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
    else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
    else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
    
    Diameter[i,j]<-Diameter[i-1,j]+growth-M*(Diameter[i-1,j]+growth)
    
    #TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    if (M==1){
      age[i,j]<-0
    }
  }}

# save average modeled diameter
modeled.d<-mean(Diameter[25,])
observed.d<-c(8)
observed.a<-c(25)
plot_density<-c(946)

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, aridity, CN_SCALE)

df<-data.frame(df)

colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Aridity", "Soil CN")

# data will be saved as list 3
mylist$Jokela_2010 <- df


# Doren 1993 simulation ---------------------------------------------------

#Lostmans Pine aridity/temp
aridity<-6962*0.0001
temp<-23.83206

#Lostmans Pine CN
C_AVG<-(1105/10)*(1/3)+((860/10)*(2/3))

N_AVG<-(1520/100)*(1/3)+((1290/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE_LOP<-(CN*6.045)+19.775

# specify plot density and stand age
plot_density<-c(673)

observed.a<-c(200)

age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
Diameter<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the diameter matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(0.8156395 - 0.004348023*temp - 0.2872396*aridity))*(0.3535108*1^(-0.6464892))


for (j in 1:plot_density[1]){ # for each tree (num of trees = plot_density[t]) 
  
  
  
  for (i in 2:observed.a[1]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(0.8156395 - 0.004348023*temp - 0.2872396*aridity))*(0.3535108*age[i,j]^(-0.6464892))
    
    # define the mortality rate here
    # initialize as a numeric with only 1 possible value
    M <- numeric(length = 1)
    
    # Get the value for M given the diameter from the previous year
    if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
    else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
    else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
    else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
    else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
    
    # Calculate the diameter for jth tree for the ith observed year
    Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
    
    # use diameter and age to calculate total above-stump biomass of the jth tree in the ith year
    #TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    # If the tree dies, plant a new tree (age = 0)
    if (M==1){age[i,j]<-0}
    
  }
}

# save average modeled diameter
modeled.d<-mean(Diameter[200,which(Diameter[200,]>.732499)]) # trees over 1.5m tall
observed.d<-c(4.133858)


# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, aridity, CN_SCALE_LOP)

df<-data.frame(df)

colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Aridity", "Soil CN")

# data will be saved as list 4
mylist$Doren_1993 <- df

# to compare to histogram in paper
hist(Diameter[200,which(Diameter[200,]>.732499)]*2.54) 


# Sharma 2019 simultion (Hells state forest) ------------------------------

temp1<-19.85

aridity1<-9452*.0001

# CN scaled with FIA data
C_AVG<-(667/10)*(1/3)+((160/10)*(2/3))

N_AVG<-(1217/100)*(1/3)+((491/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE_Hells<-(CN*6.045)+19.775

# set stand age and density
observed.a<-c(32)
plot_density<-c(775)

age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(0.8156395 - 0.004348023*temp1 - 0.2872396*aridity1))*(0.3535108*1^(-0.6464892))

for (j in 1:775){ # specify tree per hectare
  
  for (i in 2:32){ # specify how long to run the simulation (years)
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(0.8156395 - 0.004348023*temp1 - 0.2872396*aridity1))*(0.3535108*age[i,j]^(-0.6464892))
    
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
    TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    # If the tree dies, plant a new tree (age = 0)
    if (M==1){age[i,j]<-0}
    
  }
}

# save average modeled diameter
modeled.d<-mean(Diameter[32,])
observed.d<-c(8.6)

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp1, aridity1, CN_SCALE_Hells)

df<-data.frame(df)

colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Aridity", "Soil CN")

# data will be saved as list 5
mylist$Sharma_2019 <- df


# Ziehm 1992 simulation (LA) ----------------------------------------------

# set parameters for growth equations
temp1<-18.97

aridity1<-10093*.0001

# CN scaled with FIA data
C_AVG<-(496/10)*(1/3)+((119/10)*(2/3))

N_AVG<-(254/100)*(1/3)+((75/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE_LA<-(CN*6.045)+19.775

age<-matrix(0, nrow=7, ncol=1165) # initialize the age matrix
Diameter<-matrix(0, nrow=7, ncol=1165) # initialize the diameter matrix
TASB<-matrix(0, nrow=7, ncol=1165) # initialize the total above stump biomass matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(0.8156395 - 0.004348023*temp1 - 0.2872396*aridity1))*(0.3535108*1^(-0.6464892))

for (j in 1:1165){ # specify tree per hectare
  
  for (i in 2:7){ # specify how long to run the simulation (years)
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(0.8156395 - 0.004348023*temp1 - 0.2872396*aridity1))*(0.3535108*age[i,j]^(-0.6464892))
    
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
    TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    # If the tree dies, plant a new tree (age = 0)
    if (M==1){age[i,j]<-0}
    
  }
}

# save average modeled diameter
modeled.d<-mean(Diameter[7,])
observed.d<-c(4.29)
observed.a<-c(7)
plot_density<-c(1165)

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp1, aridity1, CN_SCALE_LA)

df<-data.frame(df)

colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Aridity", "Soil CN")

# data will be saved as list 6
mylist$Ziehm_1992 <- df

df2 <- do.call(rbind.data.frame, mylist)


# Compare observed diameter vs modeled diameter ---------------------------

model.1<-lm(log10(df2$`Observed Diameter`/df2$`Modeled Diameter`)~df2$Temperature+log10(df2$Age)+df2$Aridity+df2$`Tree Density`+df2$`Soil CN`)
summary(model.1)

res.age<-residuals(model.1)
plot(res.age~df2$Age)
plot(res.age~df2$Aridity)

model.2<-lm(log10(df2$`Observed Diameter`/df2$`Modeled Diameter`)~df2$Temperature+log10(df2$Age))
summary(model.2)

plot(df2$`Observed Diameter`~df2$`Modeled Diameter`, xlim=c(1,8), xlab="Modeled Diameter", ylab="Observed Diameter", main= "Before Parameter Correction",
     cex.axis=1.5, col.axis="#027368", col="#75BFBF", pch=16, type="p", font=2)
abline(0,1, col="#048ABF")


# Correcting growth equation -------------------------------------------------

# before
growth<-(10^(0.8156395 - 0.004348023*temp - 0.2872396*aridity))*(0.3535108*age[i,j]^(-0.6464892))

#after
growth<-(10^(3.68541 - 0.166258*temp - 0.2872396*aridity))*(0.72176*age[i,j]^(-0.27824))


# Running simulations with corrected growth equation ----------------------

rm(list=ls())

# Gholz 1982 ----------------------------------------------

# load observed data for age and diameter
load(file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/bradford county simulation/models.Rdata") 

# data will be saved as list 1
mylist<-list() 

# Aridity averages per sub county
Lawtey<-8244.150862068966
Starke<-8203.438775510203
Hampton<-8100.927419354839
Brooker<-8155.30223880597

# set parameters for growth equation
aridity<-((Lawtey + Starke + Hampton + Brooker)/4)*0.0001

temp<-20.05

C_AVG<-(418.4782/10)*(1/3)+((156.6189/10)*(2/3))

N_AVG<-(1177.5531/100)*(1/3)+((459.7207/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*6.045)+19.775


# set plot densities for simulations
plot_density<-c(1392,1280,1648,1696,1728,1792,880,1080,1280,
                1008,1040,784,928,1184,1056,1056,928,976)

# initialize matrix for modeled diameter
d <- matrix(0, ncol = 1, nrow = length(observed.a))


for(t in 1:length(observed.a)) {
  
  age<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the diameter matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(3.68541 - 0.166258*temp - 0.2872396*aridity))*(0.72176*1^(-0.27824))
  
  
  for (j in 1:plot_density[t]){ # for each tree (num of trees = plot_density[t]) 
    
    
    for (i in 2:observed.a[t]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(3.68541 - 0.166258*temp - 0.2872396*aridity))*(0.72176*age[i,j]^(-0.27824))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Get the value for M given the diameter from the previous year
      if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
      else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
      else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
      else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
      else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  d[t,] <- mean(Diameter[observed.a[t,],])  # take the mean of the last row (observed.a[t,])
}

# set up dataframe to store simulated data
Temperature<-rep(20.05, 18)
Aridity<-rep(aridity, 18)
CN<-rep(CN_SCALE, 18)

df<-cbind(observed.d, d, observed.a, plot_density, Temperature, Aridity, CN)

df<-data.frame(df)

colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Aridity", "Soil CN")

mylist$Bradford_County <- df


# Roth 2007 simulation ----------------------------------------------------

# set parameters for growth equation
temp<-(20.30676 + 19.65989)/2

C_AVG<-(634.5/10)*(1/3)+((231/10)*(2/3))

N_AVG<-(1245.5/100)*(1/3)+((412/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*6.045)+19.775

# aridity values
Waldo_FL<-8058

Perry_FL<- 9011

aridity<-((Waldo_FL + Perry_FL)/2)*0.0001

# set plot density and stand age
plot_density<-c(1334, 2990, 1334, 2990, 1334, 2990)
observed.a<-c(2, 2, 3, 3, 5, 5)
observed.d<-c(0.94488, 1.023622047, 2.165354331, 2.007874016, 4.133858268, 3.42519685)

# initialize matrix to save average modeled diameter
d <- matrix(0, ncol = 1, nrow = length(observed.a))


for(t in 1:length(observed.a)) {
  
  age<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the diameter matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(3.68541 - 0.166258*temp - 0.2872396*aridity))*(0.72176*1^(-0.27824))
  
  
  for (j in 1:plot_density[t]){
    
    
    for (i in 2:observed.a[t]){
      age[i,j]<-age[i-1,j]+1
      growth<-(10^(3.68541 - 0.166258*temp - 0.2872396*aridity))*(0.72176*age[i,j]^(-0.27824))
      
      
      if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
      else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
      else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
      else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
      else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
      
      
      Diameter[i,j]<-Diameter[i-1,j]+growth-M*(Diameter[i-1,j]+growth)
      
      #TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
      
      if (M==1){
        age[i,j]<-0
      }
    }
  }
  d[t,] <- mean(Diameter[observed.a[t],])  # take the mean of the last row (observed.a[t,])
}

# set up dataframe to store simulated data
Temperature<-rep(temp, 6)
Aridity<-rep(aridity, 6)
CN<-rep(CN_SCALE, 6)


df<-cbind(observed.d, d, observed.a, plot_density, Temperature, Aridity, CN)

df<-data.frame(df)

colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Aridity", "Soil CN")

# data will be saved as list 2
mylist$Roth_2007 <- df


# Jokela 2010 simulation  -------------------------------------------------

# set paramenter for growth equation
C_AVG<-(485/10)*(1/3)+((151/10)*(2/3))

N_AVG<-(1722/100)*(1/3)+((876/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*6.045)+19.775

temp<-20.76

aridity<-8139*0.0001

# initialize matrix for modeled age and modeled diameter
age<-matrix(0, nrow=25, ncol=946)
Diameter<-matrix(0, nrow=25, ncol=946)

# initialize first diameter
Diameter[1,]<-(10^(3.68541 - 0.166258*temp - 0.2872396*aridity))*(0.72176*1^(-0.27824))

# start simulation
for (j in 1:946){
  
  
  for (i in 2:25){
    age[i,j]<-age[i-1,j]+1
    growth<-(10^(3.68541 - 0.166258*temp - 0.2872396*aridity))*(0.72176*age[i,j]^(-0.27824))
    
    if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
    else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
    else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
    else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
    else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
    
    Diameter[i,j]<-Diameter[i-1,j]+growth-M*(Diameter[i-1,j]+growth)
    
    #TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    if (M==1){
      age[i,j]<-0
    }
  }}

# save average modeled diameter
modeled.d<-mean(Diameter[25,])
observed.d<-c(8)
observed.a<-c(25)
plot_density<-c(946)

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, aridity, CN_SCALE)

df<-data.frame(df)

colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Aridity", "Soil CN")

# data will be saved as list 3
mylist$Jokela_2010 <- df


# Doren 1993 simulation ---------------------------------------------------

#Lostmans Pine aridity/temp
aridity<-6962*0.0001
temp<-23.83206

#Lostmans Pine CN
C_AVG<-(1105/10)*(1/3)+((860/10)*(2/3))

N_AVG<-(1520/100)*(1/3)+((1290/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE_LOP<-(CN*6.045)+19.775

# specify plot density and stand age
plot_density<-c(673)

observed.a<-c(200)

age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
Diameter<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the diameter matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(3.68541 - 0.166258*temp - 0.2872396*aridity))*(0.72176*1^(-0.27824))


for (j in 1:plot_density[1]){ # for each tree (num of trees = plot_density[t]) 
  
  
  
  for (i in 2:observed.a[1]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(3.68541 - 0.166258*temp - 0.2872396*aridity))*(0.72176*age[i,j]^(-0.27824))
    
    # define the mortality rate here
    # initialize as a numeric with only 1 possible value
    M <- numeric(length = 1)
    
    # Get the value for M given the diameter from the previous year
    if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
    else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
    else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
    else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
    else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
    
    # Calculate the diameter for jth tree for the ith observed year
    Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
    
    # use diameter and age to calculate total above-stump biomass of the jth tree in the ith year
    #TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    # If the tree dies, plant a new tree (age = 0)
    if (M==1){age[i,j]<-0}
    
  }
}

# save average modeled diameter
modeled.d<-mean(Diameter[200,which(Diameter[200,]>.732499)]) # trees over 1.5m tall
observed.d<-c(4.133858)


# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, aridity, CN_SCALE_LOP)

df<-data.frame(df)

colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Aridity", "Soil CN")

# data will be saved as list 4
mylist$Doren_1993 <- df

# to compare to histogram in paper
hist(Diameter[200,which(Diameter[200,]>.732499)]*2.54) 


# Sharma 2019 simultion (Hells state forest) ------------------------------

temp1<-19.85

aridity1<-9452*.0001

# CN scaled with FIA data
C_AVG<-(667/10)*(1/3)+((160/10)*(2/3))

N_AVG<-(1217/100)*(1/3)+((491/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE_Hells<-(CN*6.045)+19.775

# set stand age and density
observed.a<-c(32)
plot_density<-c(775)

age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(3.68541 - 0.166258*temp1 - 0.2872396*aridity1))*(0.72176*1^(-0.27824))

for (j in 1:775){ # specify tree per hectare
  
  for (i in 2:32){ # specify how long to run the simulation (years)
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(3.68541 - 0.166258*temp1 - 0.2872396*aridity1))*(0.72176*age[i,j]^(-0.27824))
    
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
    TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    # If the tree dies, plant a new tree (age = 0)
    if (M==1){age[i,j]<-0}
    
  }
}

# save average modeled diameter
modeled.d<-mean(Diameter[32,])
observed.d<-c(8.6)

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp1, aridity1, CN_SCALE_Hells)

df<-data.frame(df)

colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Aridity", "Soil CN")

# data will be saved as list 5
mylist$Sharma_2019 <- df


# Ziehm 1992 simulation (LA) ----------------------------------------------

# set parameters for growth equations
temp1<-18.97

aridity1<-10093*.0001

# CN scaled with FIA data
C_AVG<-(496/10)*(1/3)+((119/10)*(2/3))

N_AVG<-(254/100)*(1/3)+((75/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE_LA<-(CN*6.045)+19.775

age<-matrix(0, nrow=7, ncol=1165) # initialize the age matrix
Diameter<-matrix(0, nrow=7, ncol=1165) # initialize the diameter matrix
TASB<-matrix(0, nrow=7, ncol=1165) # initialize the total above stump biomass matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(3.68541 - 0.166258*temp1 - 0.2872396*aridity1))*(0.72176*1^(-0.27824))

for (j in 1:1165){ # specify tree per hectare
  
  for (i in 2:7){ # specify how long to run the simulation (years)
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(3.68541 - 0.166258*temp1 - 0.2872396*aridity1))*(0.72176*age[i,j]^(-0.27824))
    
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
    TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    # If the tree dies, plant a new tree (age = 0)
    if (M==1){age[i,j]<-0}
    
  }
}

# save average modeled diameter
modeled.d<-mean(Diameter[7,])
observed.d<-c(4.29)
observed.a<-c(7)
plot_density<-c(1165)

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp1, aridity1, CN_SCALE_LA)

df<-data.frame(df)

colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Aridity", "Soil CN")

# data will be saved as list 6
mylist$Ziehm_1992 <- df

df2 <- do.call(rbind.data.frame, mylist)


# Compare observed diameter vs modeled diameter ---------------------------

plot(df2$`Observed Diameter`~df2$`Modeled Diameter`, xlim=c(1,8), xlab="Modeled Diameter", ylab="Observed Diameter", main= "After Parameter Correction",
     cex.axis=1.5, col.axis="#027368", col="#75BFBF", pch=16, type="p", font=2)
abline(0,1, col="#048ABF")


# Calculating total above stump C biomass for year 2100 for 700 tph by region



# Panhandle TASB ----------------------------------------------------------

temp1<-19.84
aridity1<-9768*.0001


age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(3.68541 - 0.166258*temp1 - 0.2872396*aridity1))*(0.72176*1^(-0.27824))


for (j in 1:700){ # specify tree per hectare
  
  
  
  for (i in 2:80){ # specify how long to run the simulation (years)
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(3.68541 - 0.166258*temp1 - 0.2872396*aridity1))*(0.72176*age[i,j]^(-0.27824))
    
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
    TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    # If the tree dies, plant a new tree (age = 0)
    if (M==1){age[i,j]<-0}
    
  }
}
Panhandle.Cgperm2<-sum(TASB[80,])*.5*1000*(1/100000)
mean(Diameter[80,])
hist(Diameter[80,]*2.54)

# Calculating North TASB --------------------------------------------------

temp<-20.30
aridity<-8212*.0001


age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
Diameter2<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
TASB2<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(3.68541 - 0.166258*temp1 - 0.2872396*aridity1))*(0.72176*1^(-0.27824))


for (j in 1:700){ # specify tree per hectare
  
  
  
  for (i in 2:80){ # specify how long to run the simulation (years)
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(3.68541 - 0.166258*temp1 - 0.2872396*aridity1))*(0.72176*age[i,j]^(-0.27824))
    
    # define the mortality rate here
    # initialize as a numeric with only 1 possible value
    M <- numeric(length = 1)
    
    # Mortality based on Diameter2 class
    if (Diameter2[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
    else if (Diameter2[i,j]>3.94 & Diameter2[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
    else if (Diameter2[i,j]>7.97 & Diameter2[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
    else if (Diameter2[i,j]>11.81 & Diameter2[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
    else if (Diameter2[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
    
    # Calculate the Diameter2 for jth tree for the ith observed year
    Diameter2[i,j]<-Diameter2[i-1,j] + growth - M*(Diameter2[i-1,j]+growth)
    
    #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
    TASB2 [i,j]<-0.0578*(Diameter2[i,j]^2.41)*(age[i,j]^.204)
    
    # If the tree dies, plant a new tree (age = 0)
    if (M==1){age[i,j]<-0}
    
  }
}
North.Cgperm2<-sum(TASB2[80,])*.5*1000*(1/100000)
mean(Diameter2[80,])
hist(Diameter2[80,])

# TASB Central ------------------------------------------------------------

temp<-22.29
aridity<-7259*.0001


age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(3.68541 - 0.166258*temp1 - 0.2872396*aridity1))*(0.72176*1^(-0.27824))


for (j in 1:700){ # specify tree per hectare
  
  
  
  for (i in 2:80){ # specify how long to run the simulation (years)
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(3.68541 - 0.166258*temp1 - 0.2872396*aridity1))*(0.72176*age[i,j]^(-0.27824))
    
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
    TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    # If the tree dies, plant a new tree (age = 0)
    if (M==1){age[i,j]<-0}
    
  }
}
Central.Cgperm2<-sum(TASB[80,])*.5*1000*(1/100000)
hist(Diameter[80,])

# South TASB --------------------------------------------------------------

temp<-23.80
aridity<-7305*.0001


age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(3.68541 - 0.166258*temp1 - 0.2872396*aridity1))*(0.72176*1^(-0.27824))


for (j in 1:700){ # specify tree per hectare
  
  
  
  for (i in 2:80){ # specify how long to run the simulation (years)
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(3.68541 - 0.166258*temp1 - 0.2872396*aridity1))*(0.72176*age[i,j]^(-0.27824))
    
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
    TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    # If the tree dies, plant a new tree (age = 0)
    if (M==1){age[i,j]<-0}
    
  }
}
South.Cgperm2<-sum(TASB[80,])*.5*1000*(1/100000)
hist(Diameter[80,]*2.54)

