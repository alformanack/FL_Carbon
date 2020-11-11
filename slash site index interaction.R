# Plot 46 ----------------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")

envdata<-read.csv("slash_env_data_site_index.csv", header=T, sep=",")

# data will be saved as list 1
mylist<-list() 


# set parameters for growth equation
SI<-envdata[1,16]

aridity<-envdata[1,15]*0.0001

temp<-envdata[1,10]

temp2<-envdata[1,10]^2

C_AVG<-((envdata[1,11]/10)*(1/3))+((envdata[1,12]/10)*(2/3))

N_AVG<-((envdata[1,13]/100)*(1/3))+((envdata[1,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[1,2])
observed.a<-round((10^(-1.701186 + 0.01898789*temp +  1.194143*aridity + 0.01039727*CN_SCALE - 0.01136369*aridity*CN_SCALE))*(envdata[1,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(0.9840024 - 0.010983*temp - 0.690718*aridity - 0.006014*CN_SCALE + 0.006573*aridity*CN_SCALE))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize the diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize the diameter matrix
  TASB<-array(dim=c(100,100, observed.a), 0) # initialize the total above-stump biomass matrix
  height<-array(dim=c(100,100, observed.a), 0) # initialize height (ft) matrix
  BA<-array(dim=c(100,100, observed.a), 0) # initialize basal area m^2/hectare
  RBA<-array(dim=c(100,100, observed.a), 0) # initialize relative basal area matrix
  tbaha<-vector() # initialize the total basal area per hectare
  
  
  a_1<-vector()
  a_2<-vector()
  a_3<-vector()
  a_4<-vector()
  a_5<-matrix(nrow = 5, ncol = 5,0)
  a_6<-matrix(nrow = 5, ncol = 5,0)
  a_7<-matrix(nrow = 5, ncol = 5,0)
  a_8<-matrix(nrow = 5, ncol = 5,0)
  
  
  
  Diameter[1:100,1:100,1]<-seq(1:10000)
  BA[1:100,1:100,1]<-seq(1:10000)
  height[1:100,1:100,1]<-seq(1:10000)
  
  
  trees <- sample.int(10000, plot_density, replace = F)
  
  Diameter[,,1] <- replace(Diameter[,,1], Diameter[,,1] %in% trees, initial)
  
  Diameter[,,1] <- replace(Diameter[,,1], Diameter[,,1]!= initial, NA)
  
  
  BA[,,1] <- replace(BA[,,1], BA[,,1] %in% trees, initial_BA)
  
  BA[,,1] <- replace(BA[,,1], BA[,,1]!=initial_BA, NA)
  
  height[,,1] <- replace(height[,,1], height[,,1] %in% trees, initial_height)
  
  height[,,1] <- replace(height[,,1], height[,,1]!=initial_height, NA)
  
  tbaha[1]<- initial_BA*plot_density
  
  M <- numeric(length = 1)
  
  
  for (i in 1:100){
    for (j in 1:100){
      if (!is.na(Diameter[i,j,1])) {
        for (b in 1:5) {
          for (p in 1:5){
            a_1[b]<-ifelse(i-b>0, BA[i-b,j,1],0)     
            a_2[b]<-ifelse(i+b<101, BA[i+b,j,1],0)
            a_3[b]<-ifelse(j+b<101, BA[i,j+b,1],0)
            a_4[b]<-ifelse(j-b>0, BA[i,j-b,1],0)
            
            a_5[b,p]<-ifelse(i-b>0 & j-p>0, BA[i-b,j-p,1],0)
            a_6[b,p]<-ifelse(i+b<101 & j+p<101, BA[i+b,j+p,1],0)
            a_7[b,p]<-ifelse(i-b>0 & j+p<101, BA[i-b,j+p,1],0)
            a_8[b,p]<-ifelse(i+b<101 & j-p>0, BA[i+b,j-p,1],0)
          }
        }
        a_9<-rbind(a_1,a_2,a_3,a_4)
        b_9<-ifelse(a_9>=BA[i,j,1], a_9, 0)
        a_10<-rbind(a_5,a_6,a_7,a_8)
        b_10<-ifelse(a_10>=BA[i,j,1], a_10, 0)
        a_11<-sum(b_9, na.rm = TRUE)+sum(b_10, na.rm = TRUE)
        RBA[i,j,1]<-BA[i,j,1]/(1/a_11) 
      }
    }
  }
  
  
  
  
  
  for (h in 2:observed.a){
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        
        age[i,j,h]<-age[i,j,h-1]+1
        
        growth<-(10^(0.9840024 - 0.010983*temp - 0.690718*aridity - 0.006014*CN_SCALE + 0.006573*aridity*CN_SCALE))*(0.5784213*age[i,j,h]^(-0.4215787))
        
        BA[i,j,h]<-BA[i,j,h-1]
        tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
        height[i,j,h]<- height[i,j,h-1]
        
        if (!is.na(Diameter[i,j,h])) {
          for (b in 1:5) {
            for (p in 1:5){
              a_1[b]<-ifelse(i-b>0, BA[i-b,j,h],0)     
              a_2[b]<-ifelse(i+b<101, BA[i+b,j,h],0)
              a_3[b]<-ifelse(j+b<101, BA[i,j+b,h],0)
              a_4[b]<-ifelse(j-b>0, BA[i,j-b,h],0)
              
              a_5[b,p]<-ifelse(i-b>0 & j-p>0, BA[i-b,j-p,h],0)
              a_6[b,p]<-ifelse(i+b<101 & j+p<101, BA[i+b,j+p,h],0)
              a_7[b,p]<-ifelse(i-b>0 & j+p<101, BA[i-b,j+p,h],0)
              a_8[b,p]<-ifelse(i+b<101 & j-p>0, BA[i+b,j-p,h],0)
            }
          }
          a_9<-rbind(a_1,a_2,a_3,a_4)
          b_9<-ifelse(a_9>=BA[i,j,h], a_9, 0)
          a_10<-rbind(a_5,a_6,a_7,a_8)
          b_10<-ifelse(a_10>=BA[i,j,h], a_10, 0)
          a_11<-sum(b_9, na.rm = TRUE)+sum(b_10, na.rm = TRUE)
          RBA[i,j,h]<-BA[i,j,h]/(1/a_11) 
        }
        
        
        
        # Mortality based on diameter class
        
        if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
        }
        
        
        # Calculate the diameter for jth tree for the ith observed year
        Diameter[i,j,h]<-Diameter[i,j,h-1] + growth - M*(Diameter[i,j,h-1]+growth)
        BA[i,j,h]<-(pi*(Diameter[i,j,h]*2.54)^2)/40000
        tbaha[h]<-sum(BA[,,h], na.rm=TRUE)
        height[i,j,h]<-10^(0.8730716)+Diameter[i,j,h]^(0.8976928)
        
        # use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
        TASB [i,j,h]<-(0.041281*((Diameter[i,j,h]*2.54)^2.722214))
        
        # If the tree dies, plant a new tree (age = 0)
        if (M==1){
          age[i,j,h]<-0
        } 
        
      }
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[observed.a,])
  predict.tasb[o,1]<-sum(TASB[observed.a, ])*.5*1000*(1/10000)
}


observed.d<-c(envdata[1,4])
observed.tasb<-c(envdata[1,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "46", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 1
mylist[[1]] <- df