
# Plot 46 ----------------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")

envdata<-read.csv("slash_env_data_site_index.csv", header=T, sep=",")

# data will be saved as list 1
mylist<-list() 


par(mfrow=c(1,1))
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
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[1,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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

  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  

    
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
    
     for (i in 1:100){ # specify how long to run the simulation (years)
       Diameter[i,j,h]<-Diameter[i,j,h-1]
       if (!is.na(Diameter[i,j,h])) {
         
         k<-k+1
      
       

        # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
        age[i,j,h]<-age[i,j,h-1]+1
        growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
        
        BA[i,j,h]<-BA[i,j,h-1]
        tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
        height[i,j,h]<- height[i,j,h-1]
        
        # if (!is.na(Diameter[i,j,h])) {
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
        # }
        
        
        
        # Mortality based on diameter class
        
        # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                             11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                             tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
        # }
       
        
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
        
    }
      print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}
  
  
observed.d<-c(envdata[1,4])
observed.tasb<-c(envdata[1,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "46", xlab = "Diameter (in)")

qqplot(observed.DBH$plot_98,Diameter[,,observed.a])
mod_DBH<-na.omit(as.vector((Diameter[,,observed.a])))
mod_quant<-quantile(mod_DBH,probs=seq(1/k,1,1/k))
obs_quant<-quantile(obs_DBH,probs=seq(1/k,1,1/k))
> plot(mod_quant)

> plot(seq(1/k,1,1/k)*100,mod_quant)
> obs_quant<-quantile(obs_DBH,probs=seq(1/k,1,1/k))
> plot(obs_quant,mod_quant)
> abline(0,1)
> SEF<-sum((obs_quant-mod_quant)^2)

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 1
mylist[[1]] <- df


# Plot 48 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[2,16]

aridity<-envdata[2,15]*0.0001

temp<-envdata[2,10]

temp2<-envdata[2,10]^2

C_AVG<-((envdata[2,11]/10)*(1/3))+((envdata[2,12]/10)*(2/3))

N_AVG<-((envdata[2,13]/100)*(1/3))+((envdata[2,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[2,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[2,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  mean(Diameter[ ,,observed.a [Diameter[,,observed.a] >= 5 ]], na.rm=TRUE)
  predict.d[o,1]<-mean(Diameter[,,observed.a, which(Diameter[,,53]>=5)], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}


observed.d<-c(envdata[2,4])
observed.tasb<-c(envdata[2,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "48", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 2
mylist[[2]] <- df

# Plot 84 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[3,16]

aridity<-envdata[3,15]*0.0001

temp<-envdata[3,10]

temp2<-envdata[3,10]^2

C_AVG<-((envdata[3,11]/10)*(1/3))+((envdata[3,12]/10)*(2/3))

N_AVG<-((envdata[3,13]/100)*(1/3))+((envdata[3,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[3,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[3,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}


observed.d<-c(envdata[3,4])
observed.tasb<-c(envdata[3,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "84", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 3
mylist[[3]] <- df

# Plot 95 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[4,16]

aridity<-envdata[4,15]*0.0001

temp<-envdata[4,10]

temp2<-envdata[4,10]^2

C_AVG<-((envdata[4,11]/10)*(1/3))+((envdata[4,12]/10)*(2/3))

N_AVG<-((envdata[4,13]/100)*(1/3))+((envdata[4,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[4,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[4,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}

observed.d<-c(envdata[4,4])
observed.tasb<-c(envdata[4,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "95", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 4
mylist[[4]] <- df

# Plot 98 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[5,16]

aridity<-envdata[5,15]*0.0001

temp<-envdata[5,10]

temp2<-envdata[5,10]^2

C_AVG<-((envdata[5,11]/10)*(1/3))+((envdata[5,12]/10)*(2/3))

N_AVG<-((envdata[5,13]/100)*(1/3))+((envdata[5,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[5,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[5,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}


observed.d<-c(envdata[5,4])
observed.tasb<-c(envdata[5,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "98", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 5
mylist[[5]] <- df

# Plot 126 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[6,16]

aridity<-envdata[6,15]*0.0001

temp<-envdata[6,10]

temp2<-envdata[6,10]^2

C_AVG<-((envdata[6,11]/10)*(1/3))+((envdata[6,12]/10)*(2/3))

N_AVG<-((envdata[6,13]/100)*(1/3))+((envdata[6,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[6,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[6,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
for (o in 1:10){  
  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}


observed.d<-c(envdata[6,4])
observed.tasb<-c(envdata[6,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "126", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 6
mylist[[6]] <- df

# Plot 143 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[7,16]

aridity<-envdata[7,15]*0.0001

temp<-envdata[7,10]

temp2<-envdata[7,10]^2

C_AVG<-((envdata[7,11]/10)*(1/3))+((envdata[7,12]/10)*(2/3))

N_AVG<-((envdata[7,13]/100)*(1/3))+((envdata[7,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[7,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[7,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}

observed.d<-c(envdata[7,4])
observed.tasb<-c(envdata[7,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "143", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 7
mylist[[7]] <- df

# Plot 151 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[8,16]

aridity<-envdata[8,15]*0.0001

temp<-envdata[8,10]

temp2<-envdata[8,10]^2

C_AVG<-((envdata[8,11]/10)*(1/3))+((envdata[8,12]/10)*(2/3))

N_AVG<-((envdata[8,13]/100)*(1/3))+((envdata[8,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[8,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[8,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}

observed.d<-c(envdata[8,4])
observed.tasb<-c(envdata[8,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "151", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 8
mylist[[8]] <- df

# Plot 152 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[9,16]

aridity<-envdata[9,15]*0.0001

temp<-envdata[9,10]

temp2<-envdata[9,10]^2

C_AVG<-((envdata[9,11]/10)*(1/3))+((envdata[9,12]/10)*(2/3))

N_AVG<-((envdata[9,13]/100)*(1/3))+((envdata[9,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[9,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[9,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}


observed.d<-c(envdata[9,4])
observed.tasb<-c(envdata[9,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "152", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 9
mylist[[9]] <- df

# Plot 156 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[10,16]

aridity<-envdata[10,15]*0.0001

temp<-envdata[10,10]

temp2<-envdata[10,10]^2

C_AVG<-((envdata[10,11]/10)*(1/3))+((envdata[10,12]/10)*(2/3))

N_AVG<-((envdata[10,13]/100)*(1/3))+((envdata[10,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[10,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[10,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}


observed.d<-c(envdata[10,4])
observed.tasb<-c(envdata[10,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "156", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 10
mylist[[10]] <- df

# Plot 186 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[11,16]

aridity<-envdata[11,15]*0.0001

temp<-envdata[11,10]

temp2<-envdata[11,10]^2

C_AVG<-((envdata[11,11]/10)*(1/3))+((envdata[11,12]/10)*(2/3))

N_AVG<-((envdata[11,13]/100)*(1/3))+((envdata[11,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[11,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[11,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}


observed.d<-c(envdata[11,4])
observed.tasb<-c(envdata[11,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "186", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 11
mylist[[11]] <- df

# Plot 193 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[12,16]

aridity<-envdata[12,15]*0.0001

temp<-envdata[12,10]

temp2<-envdata[12,10]^2

C_AVG<-((envdata[12,11]/10)*(1/3))+((envdata[12,12]/10)*(2/3))

N_AVG<-((envdata[12,13]/100)*(1/3))+((envdata[12,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[12,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[12,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}

observed.d<-c(envdata[12,4])
observed.tasb<-c(envdata[12,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "193", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 12
mylist[[12]] <- df

# Plot 199 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[13,16]

aridity<-envdata[13,15]*0.0001

temp<-envdata[13,10]

temp2<-envdata[13,10]^2

C_AVG<-((envdata[13,11]/10)*(1/3))+((envdata[13,12]/10)*(2/3))

N_AVG<-((envdata[13,13]/100)*(1/3))+((envdata[13,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[13,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[13,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}

observed.d<-c(envdata[13,4])
observed.tasb<-c(envdata[13,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "199", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 13
mylist[[13]] <- df

# Plot 221 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[14,16]

aridity<-envdata[14,15]*0.0001

temp<-envdata[14,10]

temp2<-envdata[14,10]^2

C_AVG<-((envdata[14,11]/10)*(1/3))+((envdata[14,12]/10)*(2/3))

N_AVG<-((envdata[14,13]/100)*(1/3))+((envdata[14,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[14,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[14,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}


observed.d<-c(envdata[14,4])
observed.tasb<-c(envdata[14,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "221", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 14
mylist[[14]] <- df

# Plot 237 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[15,16]

aridity<-envdata[15,15]*0.0001

temp<-envdata[15,10]

temp2<-envdata[15,10]^2

C_AVG<-((envdata[15,11]/10)*(1/3))+((envdata[15,12]/10)*(2/3))

N_AVG<-((envdata[15,13]/100)*(1/3))+((envdata[15,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[15,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[15,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}

observed.d<-c(envdata[15,4])
observed.tasb<-c(envdata[15,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "237", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 15
mylist[[15]] <- df

# Plot 268 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[16,16]

aridity<-envdata[16,15]*0.0001

temp<-envdata[16,10]

temp2<-envdata[16,10]^2

C_AVG<-((envdata[16,11]/10)*(1/3))+((envdata[16,12]/10)*(2/3))

N_AVG<-((envdata[16,13]/100)*(1/3))+((envdata[16,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[16,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[16,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}

observed.d<-c(envdata[16,4])
observed.tasb<-c(envdata[16,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "268", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 16
mylist[[16]] <- df

# Plot 316 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[17,16]

aridity<-envdata[17,15]*0.0001

temp<-envdata[17,10]

temp2<-envdata[17,10]^2

C_AVG<-((envdata[17,11]/10)*(1/3))+((envdata[17,12]/10)*(2/3))

N_AVG<-((envdata[17,13]/100)*(1/3))+((envdata[17,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[17,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[17,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}

observed.d<-c(envdata[17,4])
observed.tasb<-c(envdata[17,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "316", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 17
mylist[[17]] <- df

# Plot 332 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[18,16]

aridity<-envdata[18,15]*0.0001

temp<-envdata[18,10]

temp2<-envdata[18,10]^2

C_AVG<-((envdata[18,11]/10)*(1/3))+((envdata[18,12]/10)*(2/3))

N_AVG<-((envdata[18,13]/100)*(1/3))+((envdata[18,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[18,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[18,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}


observed.d<-c(envdata[18,4])
observed.tasb<-c(envdata[18,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "332", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 18
mylist[[18]] <- df

# Plot 342 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[19,16]

aridity<-envdata[19,15]*0.0001

temp<-envdata[19,10]

temp2<-envdata[19,10]^2

C_AVG<-((envdata[19,11]/10)*(1/3))+((envdata[19,12]/10)*(2/3))

N_AVG<-((envdata[19,13]/100)*(1/3))+((envdata[19,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[19,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[19,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}


observed.d<-c(envdata[19,4])
observed.tasb<-c(envdata[19,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "342", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 19
mylist[[19]] <- df

# Plot 371 ----------------------------------------------

# set parameters for growth equation
SI<-envdata[20,16]

aridity<-envdata[20,15]*0.0001

temp<-envdata[20,10]

temp2<-envdata[20,10]^2

C_AVG<-((envdata[20,11]/10)*(1/3))+((envdata[20,12]/10)*(2/3))

N_AVG<-((envdata[20,13]/100)*(1/3))+((envdata[20,14]/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-round(envdata[20,2])
observed.a<-round((10^(2.475538 - 0.3158355*temp + 0.007800543*temp2 + 0.5217529*aridity))*(envdata[20,3]^1.728844))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){
    
    k<-0
    
    for (j in 1:100){ # specify column
      
      for (i in 1:100){ # specify how long to run the simulation (years)
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          
          
          # growth<-(10^(intercept[k] + A[k]*temp + B[k]*temp2 + C[k]*aridity))*(D[k]*age[i,j,h]^(E[k]))
          age[i,j,h]<-age[i,j,h-1]+1
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # if (!is.na(Diameter[i,j,h])) {
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
          # }
          
          
          
          # Mortality based on diameter class
          
          # if (Diameter[i,j,h-1]>0 & !is.na(Diameter[i,j,h-1])) {
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          # }
          
          
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
      
    }
    print(paste("time step =", h))
  }
  # save average modeled diameter
  predict.d[o,1]<-mean(Diameter[,,observed.a], na.rm = TRUE)
  predict.tasb[o,1]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}


observed.d<-c(envdata[20,4])
observed.tasb<-c(envdata[20,5])
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
sd.tasb<-sd(predict.tasb)
sd.dbh<-sd(predict.d)
hist(Diameter[,,observed.a], main = "371", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb, 
          sd.tasb, sd.dbh)

# df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temp2", "Aridity", "Soil_CN", "Observed_Biomass", 
                "Modeled_Biomass", "sd.tasb", "sd.dbh")

# data will be saved as list 20
mylist[[20]] <- df

final_list_slash <- do.call(rbind.data.frame, mylist)
row.names(final_list_slash) <- c(as.vector(envdata[,9]))
# final_list$Plot<- c(1,12,14:18)
# final_list$Plot<-as.factor(final_list$Plot)
sdev<-as.vector(final_list_slash$sd.tasb)
sdev.dbh<-as.vector(final_list_slash$sd.dbh)

write.csv(final_list_slash, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/final_list_slash_site_index.csv")

# Compare observed diameter vs modeled diameter ---------------------------
setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
final_list_slash<-read.csv("final_list_slash_site_index.csv", header=T, sep=",")


par(mfrow=c(1,1))

model.1<-lm(data = final_list_slash, log10(Observed_Diameter/Modeled_Diameter)~log10(Age)+Tree_Density+Soil_CN)
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

plot(data = final_list_slash, Observed_Biomass~Modeled_Biomass)
abline(0,1)
plot(data = final_list_slash, Observed_Diameter~Modeled_Diameter)
plot(data = final_list, Modeled_Diameter~Tree_Density)
plot(data = final_list, Observed_Diameter~Tree_Density)

plot(data = final_list_slash, Observed_Diameter~Modeled_Diameter, xlim = c(2,11), ylim = c(2,11), xlab="Modeled DBH (in)", ylab="Observed DBH (in)",
     main = "Before parameter correction", col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Diameter~Modeled_Diameter, labels=final_list_slash$X,data=final_list_slash, cex=0.9, font=2, pos=4)
arrows(final_list_slash$Modeled_Diameter-sdev.dbh, final_list_slash$Observed_Diameter, final_list_slash$Modeled_Diameter+sdev.dbh, final_list_slash$Observed_Diameter, length=0.05, angle=90, code=3)

plot(data = final_list_slash, Observed_Biomass~Modeled_Biomass,  xlim = c(1,11000), ylim = c(1,11000), col = "#75BFBF", xlab="Modeled", ylab="Observed", main ="Tree Carbon (gC/m^2)",
     col.axis="#027368", pch=16, type="p") + abline(0,1, col="#048ABF")
text(Observed_Biomass~Modeled_Biomass, labels=final_list_slash$X,data=final_list_slash, cex=0.9, font=2, pos=4)
arrows(final_list$Modeled_Biomass-sdev, final_list$Observed_Biomass, final_list$Modeled_Biomass+sdev, final_list$Observed_Biomass, length=0.05, angle=90, code=3)

# Calculating total above stump C biomass for year 2100 for 700 tph by region

Panhandle<-vector()
North<-vector()
Central<-vector()
South<-vector()
plot_density<-700

par(mfrow=c(2,2))

# Panhandle TASB ----------------------------------------------------------

temp<-19.84

temp2<-temp^2

aridity<-9768*.0001

# CN scaled with FIA data

C_AVG<-(539/10)*(1/3)+((165/10)*(2/3))

N_AVG<-(1297/100)*(1/3)+((527/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777

# set stand age and density
plot_density<-700
observed.a<-80
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)
SI<-72.95

# for (o in 1:2){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:2){  
  for (h in 2:observed.a){ #how long to run simunlation (yrs)
    
    k<-0
    
    for (j in 1:100){ #columns of array
      
      for (i in 1:100){ #rows of array
        Diameter[i,j,h]<-Diameter[i,j,h-1]
        
        if (!is.na(Diameter[i,j,h])) {
          
          k<-k+1
          
          age[i,j,h]<-age[i,j,h-1]+1
          
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          #variables of previous year needed to calculate probability of mortality of current year
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # calculating relative basal area (trees within 6.4m around tree that have BA>=BA of tree [i,j])
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
      
          
          # Calculating the probability of mortality
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          
          
          
          # Calculate the diameter for tree i,j in the h year
          Diameter[i,j,h]<-Diameter[i,j,h-1] + growth - M*(Diameter[i,j,h-1]+growth)
          BA[i,j,h]<-(pi*(Diameter[i,j,h]*2.54)^2)/40000
          tbaha[h]<-sum(BA[,,h], na.rm=TRUE)
          height[i,j,h]<-10^(0.8730716)+Diameter[i,j,h]^(0.8976928)
          
          # use diameter and age to calculate total aboveground biomass of the i,j tree in the h year
          TASB [i,j,h]<-(0.041281*((Diameter[i,j,h]*2.54)^2.722214))
          
          # If the tree dies, plant a new tree (age = 0)
          if (M==1){
            age[i,j,h]<-0
          }
        } 
        
      }
      
    }
    print(paste("time step =", h))
  }
  # save total above stump biomass
  Panhandle[o]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}


hist(Diameter[,,observed.a]*2.54, main = "Panhandle", xlab = "Diameter (cm)")

species<-rep("Slashpine",10)

location<-rep("Panhandle", 10)

df<-cbind(species, Panhandle, location)

df<-data.frame(df)

colnames(df)<-c("Species","TASB","Location")

biomass_list[[9]] <- df

# Calculating North TASB --------------------------------------------------

temp<-20.30

temp2<-temp^2

aridity<-8212*.0001

# CN scaled with FIA data
C_AVG<-((317/10)*(1/3))+((110/10)*(2/3))

N_AVG<-((1665/100)*(1/3))+((300/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777

# set stand age and density
plot_density<-700
observed.a<-80
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)
SI<-72.95

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){ #how long to run simunlation (yrs)
    
    k<-0
    
    for (j in 1:100){ #columns of array
      
      for (i in 1:100){ #rows of array
        
        
        if (!is.na(Diameter[i,j,h-1])) {
          
          k<-k+1
          
          age[i,j,h]<-age[i,j,h-1]+1
          
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          #variables of previous year needed to calculate probability of mortality of current year
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # calculating relative basal area (trees within 6.4m around tree that have BA>=BA of tree [i,j])
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
          
          
          # Calculating the probability of mortality
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          
          
          
          # Calculate the diameter for tree i,j in the h year
          Diameter[i,j,h]<-Diameter[i,j,h-1] + growth - M*(Diameter[i,j,h-1]+growth)
          BA[i,j,h]<-(pi*(Diameter[i,j,h]*2.54)^2)/40000
          tbaha[h]<-sum(BA[,,h], na.rm=TRUE)
          height[i,j,h]<-10^(0.8730716)+Diameter[i,j,h]^(0.8976928)
          
          # use diameter and age to calculate total aboveground biomass of the i,j tree in the h year
          TASB [i,j,h]<-(0.041281*((Diameter[i,j,h]*2.54)^2.722214))
          
          # If the tree dies, plant a new tree (age = 0)
          if (M==1){
            age[i,j,h]<-0
          }
        } 
        
      }
      
    }
    print(paste("time step =", h))
  }
  # save total above stump biomass
  North[o]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}

hist(Diameter[,,observed.a]*2.54, main = "North", xlab = "Diameter (cm)")

species<-rep("Slashpine",10)

location<-rep("North", 10)

df<-cbind(species, North, location)

df<-data.frame(df)

colnames(df)<-c("Species","TASB","Location")

biomass_list[[10]] <- df

# TASB Central ------------------------------------------------------------

temp<-22.29

temp2<-temp^2

aridity<-7259*.0001

# CN scaled with FIA data
C_AVG<-((475/10)*(1/3))+((144/10)*(2/3))

N_AVG<-((1729/100)*(1/3))+((1137/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777

# set stand age and density
plot_density<-700
observed.a<-80
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)
SI<-72.95

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){ #how long to run simunlation (yrs)
    
    k<-0
    
    for (j in 1:100){ #columns of array
      
      for (i in 1:100){ #rows of array
        
        if (!is.na(Diameter[i,j,h-1])) {
          
          k<-k+1
          
          age[i,j,h]<-age[i,j,h-1]+1
          
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          #variables of previous year needed to calculate probability of mortality of current year
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # calculating relative basal area (trees within 6.4m around tree that have BA>=BA of tree [i,j])
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
          
          
          # Calculating the probability of mortality
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          
          
          
          # Calculate the diameter for tree i,j in the h year
          Diameter[i,j,h]<-Diameter[i,j,h-1] + growth - M*(Diameter[i,j,h-1]+growth)
          BA[i,j,h]<-(pi*(Diameter[i,j,h]*2.54)^2)/40000
          tbaha[h]<-sum(BA[,,h], na.rm=TRUE)
          height[i,j,h]<-10^(0.8730716)+Diameter[i,j,h]^(0.8976928)
          
          # use diameter and age to calculate total aboveground biomass of the i,j tree in the h year
          TASB [i,j,h]<-(0.041281*((Diameter[i,j,h]*2.54)^2.722214))
          
          # If the tree dies, plant a new tree (age = 0)
          if (M==1){
            age[i,j,h]<-0
          }
        } 
        
      }
      
    }
    print(paste("time step =", h))
  }
  # save total above stump biomass
  Central[o]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}

hist(Diameter[,,observed.a]*2.54, main = "Central", xlab = "Diameter (cm)")

species<-rep("Slashpine",10)

location<-rep("Central", 10)

df<-cbind(species, Central, location)

df<-data.frame(df)

colnames(df)<-c("Species","TASB","Location")

biomass_list[[11]] <- df

# South TASB --------------------------------------------------------------

temp<-23.80

temp2<-temp^2

aridity<-7305*.0001

# CN scaled with FIA data
C_AVG<-((1052/10)*(1/3))+((1017/10)*(2/3))

N_AVG<-((1721/100)*(1/3))+((1289/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777

# set stand age and density
plot_density<-700
observed.a<-80
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787))
initial_BA<-(pi*(initial*2.47)^2)/40000
initial_height<-10^(0.8730716)+initial^(0.8976928)
SI<-72.95

# for (o in 1:10){
  # initialize the diameter for the first year
  Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter matrix
  age<-array(dim=c(100,100, observed.a), 0) # initialize age matrix
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
  
  # intercept<-rnorm(plot_density, mean = -1.431904, sd= 0.717405)
  # 
  # A<-rnorm(plot_density, mean= 0.182686, sd=0.064590)
  # 
  # B<-rnorm(plot_density, mean = - 0.004512, sd = 0.001491)
  # 
  # C<-rnorm(plot_density, mean = - 0.301793, sd=0.043243)
  # 
  # D<-rnorm(plot_density, mean=0.5784213, sd=0.01874405)
  # 
  # E<-D-1  
  
  
  
for (o in 1:10){  
  for (h in 2:observed.a){ #how long to run simunlation (yrs)
    
    k<-0
    
    for (j in 1:100){ #columns of array
      
      for (i in 1:100){ #rows of array
        
        if (!is.na(Diameter[i,j,h-1])) {
          
          k<-k+1
          
          age[i,j,h]<-age[i,j,h-1]+1
          
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          #variables of previous year needed to calculate probability of mortality of current year
          BA[i,j,h]<-BA[i,j,h-1]
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          height[i,j,h]<- height[i,j,h-1]
          
          # calculating relative basal area (trees within 6.4m around tree that have BA>=BA of tree [i,j])
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
          
          
          # Calculating the probability of mortality
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8))
          
          
          
          # Calculate the diameter for tree i,j in the h year
          Diameter[i,j,h]<-Diameter[i,j,h-1] + growth - M*(Diameter[i,j,h-1]+growth)
          BA[i,j,h]<-(pi*(Diameter[i,j,h]*2.54)^2)/40000
          tbaha[h]<-sum(BA[,,h], na.rm=TRUE)
          height[i,j,h]<-10^(0.8730716)+Diameter[i,j,h]^(0.8976928)
          
          # use diameter and age to calculate total aboveground biomass of the i,j tree in the h year
          TASB [i,j,h]<-(0.041281*((Diameter[i,j,h]*2.54)^2.722214))
          
          # If the tree dies, plant a new tree (age = 0)
          if (M==1){
            age[i,j,h]<-0
          }
        } 
        
      }
      
    }
    print(paste("time step =", h))
  }
  # save total above stump biomass
  South[o]<-sum(TASB[,,observed.a], na.rm = TRUE)*.5*1000*(1/10000)
}

hist(Diameter[,,observed.a]*2.54, main = "South", xlab = "Diameter (cm)")

species<-rep("Slashpine",10)

location<-rep("South", 10)

df<-cbind(species, South, location)

df<-data.frame(df)

colnames(df)<-c("Species","TASB","Location")

biomass_list[[12]] <- df

final_biomass <- do.call(rbind.data.frame, biomass_list)
final_biomass$TASB<-as.numeric(as.character(final_biomass$TASB))
final_biomass<-final_biomass[which(final_biomass$Location!="South"),]



summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

fb <- summarySE(final_biomass, measurevar="TASB", groupvars=c("Species","Location"))

# final_biomass[which(final_biomass$Species=="Slashpine" &
#                       final_biomass$Location=="Panhandle"),"SD"] <- sd(final_biomass[which(final_biomass$Species=="Slashpine" &
#                                                final_biomass$Location=="Panhandle"),"TASB"])
# 
# final_biomass[which(final_biomass$Species=="Slashpine" &
#                       final_biomass$Location=="North"),"SD"] <- sd(final_biomass[which(final_biomass$Species=="Slashpine" &
#                                                final_biomass$Location=="North"),"TASB"])
# 
# final_biomass[which(final_biomass$Species=="Slashpine" &
#                       final_biomass$Location=="Central"),"SD"] <- sd(final_biomass[which(final_biomass$Species=="Slashpine" &
#                                                final_biomass$Location=="Central"),"TASB"])
# 
# final_biomass[which(final_biomass$Species=="Longleaf" &
#                       final_biomass$Location=="Panhandle"),"SD"] <- sd(final_biomass[which(final_biomass$Species=="Longleaf" &
#                                                final_biomass$Location=="Panhandle"),"TASB"])
# 
# final_biomass[which(final_biomass$Species=="Longleaf" &
#                       final_biomass$Location=="North"),"SD"] <- sd(final_biomass[which(final_biomass$Species=="Longleaf" &
#                                                final_biomass$Location=="North"),"TASB"])
# 
# final_biomass[which(final_biomass$Species=="Longleaf" &
#                       final_biomass$Location=="Central"),"SD"] <- sd(final_biomass[which(final_biomass$Species=="Longleaf" &
#                                                final_biomass$Location=="Central"),"TASB"])
# 
# final_biomass[which(final_biomass$Species=="Loblolly" &
#                       final_biomass$Location=="Panhandle"),"SD"] <- sd(final_biomass[which(final_biomass$Species=="Loblolly" &
#                                                final_biomass$Location=="Panhandle"),"TASB"])
# 
# final_biomass[which(final_biomass$Species=="Loblolly" &
#                       final_biomass$Location=="North"),"SD"] <- sd(final_biomass[which(final_biomass$Species=="Loblolly" &
#                                                final_biomass$Location=="North"),"TASB"])
# 
# final_biomass[which(final_biomass$Species=="Loblolly" &
#                       final_biomass$Location=="Central"),"SD"] <- sd(final_biomass[which(final_biomass$Species=="Loblolly" &
#                                                final_biomass$Location=="Central"),"TASB"])



TASB_Panhandle<-c(mean(Panhandle), sd(Panhandle))
TASB_North<-c(mean(North), sd(North))
TASB_Central<-c(mean(Central), sd(Central))
TASB_South<-c(mean(South), sd(South))
table<-rbind(TASB_Panhandle, TASB_North, TASB_Central, TASB_South)