rm(list=ls())


final_list<-list()
for (o in 1:10){
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
  precip<-1325
  
  temp<-20.05
  
  temp2<-temp^2
  
  C_AVG<-((413.3392/10)*(1/3))+((148.5920/10)*(2/3))
  
  N_AVG<-((1532.3486/100)*(1/3))+((389.9631/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  
  # set plot densities for simulations
  plot_density<-c(1392,1280,1648,1696,1728,1792,880,1080,1280,
                  1008,1040,784,928,1184,1056,1056,928,976)
  
  # initialize matrix for modeled diameter
  d <- matrix(0, ncol = 1, nrow = length(observed.a))
  
  
  for(t in 1:length(observed.a)) {
    
    age<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the age matrix
    Diameter<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the diameter matrix
    
    # initialize the diameter for the first year
    Diameter[1,]<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*1^(-0.6504))
    
    
    
    for (j in 1:plot_density[t]){ # for each tree (num of trees = plot_density[t]) 
      
      
      for (i in 2:observed.a[t]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
        
        age[i,j]<-age[i-1,j]+1
        
        growth<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*age[i,j]^(-0.6504))
        
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
  Temperature2<-rep(temp2, 18)
  Precipitation<-rep(precip, 18)
  CN<-rep(CN_SCALE, 18)
  
  df<-cbind(observed.d, d, observed.a, plot_density, Temperature, Temperature2, Precipitation, CN)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  mylist$Gholz_1982 <- df
  
  
  # Roth 2007 simulation ----------------------------------------------------
  
  # set parameters for growth equation
  temp<-(20.30676 + 19.65989)/2
  
  temp2<-temp^2
  
  C_AVG<-((750.5/10)*(1/3))+((217.5/10)*(2/3))
  
  N_AVG<-((1642.5/100)*(1/3))+((405.5/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  # aridity values
  Waldo_FL<-1455
  
  Perry_FL<- 1312
  
  precip<-((Waldo_FL + Perry_FL)/2)
  
  # set plot density and stand age
  plot_density<-c(1334, 2990)
  observed.a<-c(5, 5)
  observed.d<-c(4.133858268, 3.42519685)
  
  # initialize matrix to save average modeled diameter
  d <- matrix(0, ncol = 1, nrow = length(observed.a))
  
  
  for(t in 1:length(observed.a)) {
    
    age<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the age matrix
    Diameter<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the diameter matrix
    
    # initialize the diameter for the first year
    #Diameter[1,]<-(10^(.381+.012*20.05-.0001*266.82))*(.346*1^(-.654))*CN_SCALE^(-.069)
    Diameter[1,]<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*1^(-0.6504))
    
    
    for (j in 1:plot_density[t]){
      
      
      for (i in 2:observed.a[t]){
        age[i,j]<-age[i-1,j]+1
        growth<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*age[i,j]^(-0.6504))
        
        
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
  Temperature<-rep(temp, 2)
  Temperature2<-rep(temp2, 2)
  Precipitation<-rep(precip, 2)
  CN<-rep(CN_SCALE, 2)
  
  
  df<-cbind(observed.d, d, observed.a, plot_density, Temperature, Temperature2, Precipitation, CN)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 2
  mylist$Roth_2007 <- df
  
  
  # Jokela 2010 simulation  -------------------------------------------------
  
  # set paramenter for growth equation
  C_AVG<-((538/10)*(1/3))+((194/10)*(2/3))
  
  N_AVG<-((1780/100)*(1/3))+((887/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  temp<-20.76
  
  temp2<-temp^2
  
  precip<-1325
  
  # initialize matrix for modeled age and modeled diameter
  age<-matrix(0, nrow=25, ncol=946)
  Diameter<-matrix(0, nrow=25, ncol=946)
  
  # initialize first diameter
  Diameter[1,]<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*1^(-0.6504))
  
  # start simulation
  for (j in 1:946){
    
    
    for (i in 2:25){
      age[i,j]<-age[i-1,j]+1
      growth<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*age[i,j]^(-0.6504))
      
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
  df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, precip, CN_SCALE)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 3
  mylist$Jokela_2010 <- df
  
  
  # Doren 1993 simulation ---------------------------------------------------
  
  #Lostmans Pine aridity/temp
  precip<-1296
  temp<-23.83206
  temp2<-temp^2
  
  #Lostmans Pine CN
  C_AVG<-((1090/10)*(1/3))+((1080/10)*(2/3))
  
  N_AVG<-((1483/100)*(1/3))+((1100/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE_LOP<-(CN*4.941)+29.777
  
  # specify plot density and stand age
  plot_density<-c(673)
  
  observed.a<-c(200)
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the diameter matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*1^(-0.6504))
  
  
  for (j in 1:plot_density[1]){ # for each tree (num of trees = plot_density[t]) 
    
    
    
    for (i in 2:observed.a[1]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*age[i,j]^(-0.6504))
      
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
  df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2,precip, CN_SCALE_LOP)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 4
  mylist$Doren_1993 <- df
  
  # to compare to histogram in paper
  hist(Diameter[200,which(Diameter[200,]>.732499)]*2.54) 
  
  
  # Sharma 2019 simultion (Hells state forest) ------------------------------
  
  temp<-19.85
  
  temp2<-temp^2
  
  precip<-1533
  
  # CN scaled with FIA data
  C_AVG<-((592/10)*(1/3))+((189/10)*(2/3))
  
  N_AVG<-((1235/100)*(1/3))+((652/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE_Hells<-(CN*4.941)+29.777
  
  # set stand age and density
  observed.a<-c(32)
  plot_density<-c(775)
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*1^(-0.6504))
  
  for (j in 1:775){ # specify tree per hectare
    
    for (i in 2:32){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*age[i,j]^(-0.6504))
      
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
  df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, precip, CN_SCALE_Hells)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 5
  mylist$Sharma_2019 <- df
  
  
  # Ziehm 1992 simulation (LA) ----------------------------------------------
  
  # set parameters for growth equations
  temp<-18.97
  
  temp2<-temp^2
  
  precip<-1567
  
  # CN scaled with FIA data
  C_AVG<-((580/10)*(1/3))+((164/10)*(2/3))
  
  N_AVG<-((406/100)*(1/3))+((86/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE_LA<-(CN*4.941)+29.777
  
  age<-matrix(0, nrow=7, ncol=1165) # initialize the age matrix
  Diameter<-matrix(0, nrow=7, ncol=1165) # initialize the diameter matrix
  TASB<-matrix(0, nrow=7, ncol=1165) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*1^(-0.6504))
  
  for (j in 1:1165){ # specify tree per hectare
    
    for (i in 2:7){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*age[i,j]^(-0.6504))
      
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
  df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, precip, CN_SCALE_LA)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 6
  mylist$Ziehm_1992 <- df
  
  
  # Bohn 2016 (18th ---------------------------------------------------------
  
  temp<-20.04
  
  temp2<-temp^2
  
  precip<-1512
  
  # CN scaled with FIA data
  C_AVG<-((675/10)*(1/3))+((260/10)*(2/3))
  
  N_AVG<-((1309/100)*(1/3))+((750/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  # set stand age and density
  observed.a<-c(30)
  plot_density<-c(1180)
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*1^(-0.6504))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*age[i,j]^(-0.6504))
      
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
  modeled.d<-sqrt(sum(Diameter[30,]^2)/1180) #converting to QMD
  observed.d<-c(7.08661) #QMD 18cm
  
  # set up dataframe to store simulated data
  df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, precip, CN_SCALE)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 7
  mylist$Bohn_2016 <- df
  
  
  
  # Hedman 1999 -------------------------------------------------------------
  
  temp<-19.10
  
  temp2<-temp^2
  
  precip<-1439
  
  # CN scaled with FIA data
  C_AVG<-((529/10)*(1/3))+((205/10)*(2/3))
  
  N_AVG<-((504/100)*(1/3))+((197/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  # set stand age and density
  observed.a<-c(49)
  plot_density<-c(529)
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*1^(-0.6504))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*age[i,j]^(-0.6504))
      
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
  modeled.d<-mean(Diameter[49,which(Diameter[49,]>=0.984252)])
  observed.d<-c(7.874)
  
  # set up dataframe to store simulated data
  df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, precip, CN_SCALE)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 8
  mylist$Hedman_1999 <- df
  
  final_list[[o]] <- do.call(rbind.data.frame, mylist)
}  

# Compare observed diameter vs modeled diameter ---------------------------
slashcoef_list<-list()
par(mfrow=c(2,5))
for (n in 1:10){
  
  model.1<-lm(data = final_list[[5]], log10(`Observed Diameter`/`Modeled Diameter`)~Temperature+Temperature2+log10(Age)+Precipitation+`Tree Density`+log10(`Soil CN`))
  summary(model.1)
  # 
  # res.age<-residuals(model.1)
  # plot(res.age~df2$Age)
  # plot(res.age~df2$Aridity)
  
  coef<-lm(data = final_list[[n]], log10(`Observed Diameter`/`Modeled Diameter`)~Temperature+Temperature2+log10(Age)+log10(`Soil CN`))$coefficients
  names(coef) <- NULL
  df<-rbind(coef)
  slashcoef_list[[n]]<-df
  
  plot(data = final_list[[n]], `Observed Diameter`~`Modeled Diameter`, xlim=c(1,8), ylim=c(1,8), xlab="Modeled", ylab="Observed",
       col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")
  
}

coef<- do.call(rbind.data.frame, slashcoef_list)
colnames(coef)<-c("Intercept","Temperature", "Temperature2","Age", "Soil CN")

corrected_intercerpt<-c(mean(coef$Intercept), sd(coef$Intercept))
corrected_temp<-c(mean(coef$Temperature), sd(coef$Temperature))
corrected_temp2<-c(mean(coef$Temperature2), sd(coef$Temperature2))
corrected_age<-c(mean(coef$Age), sd(coef$Age))
corrected_soilCN<-c(mean(coef$`Soil CN`), sd(coef$`Soil CN`))

# Correcting growth equation -------------------------------------------------

# before
growth<-(10^(-1.324 + 0.1841*temp - 0.004245*temp2 - 0.0001241*precip))*(0.3496*age[i,j]^(-0.6504))

#after
growth<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*age[i,j]^(-0.27547))*(CN_SCALE^(1.8743))


# Running simulations with corrected growth equation ----------------------

rm(list=ls())


final_list<-list()
for (o in 1:10){
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
  precip<-1325
  
  temp<-20.05
  
  temp2<-temp^2
  
  C_AVG<-((413.3392/10)*(1/3))+((148.5920/10)*(2/3))
  
  N_AVG<-((1532.3486/100)*(1/3))+((389.9631/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  
  # set plot densities for simulations
  plot_density<-c(1392,1280,1648,1696,1728,1792,880,1080,1280,
                  1008,1040,784,928,1184,1056,1056,928,976)
  
  # initialize matrix for modeled diameter
  d <- matrix(0, ncol = 1, nrow = length(observed.a))
  
  
  for(t in 1:length(observed.a)) {
    
    age<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the age matrix
    Diameter<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the diameter matrix
    
    # initialize the diameter for the first year
    Diameter[1,]<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*1^(-0.27547))*(CN_SCALE^(1.8743))
    
    
    
    for (j in 1:plot_density[t]){ # for each tree (num of trees = plot_density[t]) 
      
      
      for (i in 2:observed.a[t]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
        
        age[i,j]<-age[i-1,j]+1
        
        growth<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*age[i,j]^(-0.27547))*(CN_SCALE^(1.8743))
        
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
  Temperature2<-rep(temp2, 18)
  Precipitation<-rep(precip, 18)
  CN<-rep(CN_SCALE, 18)
  
  df<-cbind(observed.d, d, observed.a, plot_density, Temperature, Temperature2, Precipitation, CN)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  mylist$Gholz_1982 <- df
  
  
  # Roth 2007 simulation ----------------------------------------------------
  
  # set parameters for growth equation
  temp<-(20.30676 + 19.65989)/2
  
  temp2<-temp^2
  
  C_AVG<-((750.5/10)*(1/3))+((217.5/10)*(2/3))
  
  N_AVG<-((1642.5/100)*(1/3))+((405.5/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  # aridity values
  Waldo_FL<-1455
  
  Perry_FL<- 1312
  
  precip<-((Waldo_FL + Perry_FL)/2)
  
  # set plot density and stand age
  plot_density<-c(1334, 2990)
  observed.a<-c(5, 5)
  observed.d<-c(4.133858268, 3.42519685)
  
  # initialize matrix to save average modeled diameter
  d <- matrix(0, ncol = 1, nrow = length(observed.a))
  
  
  for(t in 1:length(observed.a)) {
    
    age<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the age matrix
    Diameter<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the diameter matrix
    
    # initialize the diameter for the first year
    #Diameter[1,]<-(10^(.381+.012*20.05-.0001*266.82))*(.346*1^(-.654))*CN_SCALE^(-.069)
    Diameter[1,]<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*1^(-0.27547))*(CN_SCALE^(1.8743))
    
    
    for (j in 1:plot_density[t]){
      
      
      for (i in 2:observed.a[t]){
        age[i,j]<-age[i-1,j]+1
        growth<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*age[i,j]^(-0.27547))*(CN_SCALE^(1.8743))
        
        
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
  Temperature<-rep(temp, 2)
  Temperature2<-rep(temp2, 2)
  Precipitation<-rep(precip, 2)
  CN<-rep(CN_SCALE, 2)
  
  
  df<-cbind(observed.d, d, observed.a, plot_density, Temperature, Temperature2, Precipitation, CN)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 2
  mylist$Roth_2007 <- df
  
  
  # Jokela 2010 simulation  -------------------------------------------------
  
  # set paramenter for growth equation
  C_AVG<-((538/10)*(1/3))+((194/10)*(2/3))
  
  N_AVG<-((1780/100)*(1/3))+((887/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  temp<-20.76
  
  temp2<-temp^2
  
  precip<-1325
  
  # initialize matrix for modeled age and modeled diameter
  age<-matrix(0, nrow=25, ncol=946)
  Diameter<-matrix(0, nrow=25, ncol=946)
  
  # initialize first diameter
  Diameter[1,]<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*1^(-0.27547))*(CN_SCALE^(1.8743))
  
  # start simulation
  for (j in 1:946){
    
    
    for (i in 2:25){
      age[i,j]<-age[i-1,j]+1
      growth<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*age[i,j]^(-0.27547))*(CN_SCALE^(1.8743))
      
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
  df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, precip, CN_SCALE)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 3
  mylist$Jokela_2010 <- df
  
  
  # Doren 1993 simulation ---------------------------------------------------
  
  #Lostmans Pine aridity/temp
  precip<-1296
  temp<-23.83206
  temp2<-temp^2
  
  #Lostmans Pine CN
  C_AVG<-((1090/10)*(1/3))+((1080/10)*(2/3))
  
  N_AVG<-((1483/100)*(1/3))+((1100/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE_LOP<-(CN*4.941)+29.777
  
  # specify plot density and stand age
  plot_density<-c(673)
  
  observed.a<-c(200)
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the diameter matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*1^(-0.27547))*(CN_SCALE^(1.8743))
  
  
  for (j in 1:plot_density[1]){ # for each tree (num of trees = plot_density[t]) 
    
    
    
    for (i in 2:observed.a[1]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*age[i,j]^(-0.27547))*(CN_SCALE^(1.8743))
      
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
  df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2,precip, CN_SCALE_LOP)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 4
  mylist$Doren_1993 <- df
  
  # to compare to histogram in paper
  hist(Diameter[200,which(Diameter[200,]>.732499)]*2.54) 
  
  
  # Sharma 2019 simultion (Hells state forest) ------------------------------
  
  temp<-19.85
  
  temp2<-temp^2
  
  precip<-1533
  
  # CN scaled with FIA data
  C_AVG<-((592/10)*(1/3))+((189/10)*(2/3))
  
  N_AVG<-((1235/100)*(1/3))+((652/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE_Hells<-(CN*4.941)+29.777
  
  # set stand age and density
  observed.a<-c(32)
  plot_density<-c(775)
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*1^(-0.27547))*(CN_SCALE^(1.8743))
  
  for (j in 1:775){ # specify tree per hectare
    
    for (i in 2:32){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*age[i,j]^(-0.27547))*(CN_SCALE^(1.8743))
      
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
  df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, precip, CN_SCALE_Hells)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 5
  mylist$Sharma_2019 <- df
  
  
  # Ziehm 1992 simulation (LA) ----------------------------------------------
  
  # set parameters for growth equations
  temp<-18.97
  
  temp2<-temp^2
  
  precip<-1567
  
  # CN scaled with FIA data
  C_AVG<-((580/10)*(1/3))+((164/10)*(2/3))
  
  N_AVG<-((406/100)*(1/3))+((86/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE_LA<-(CN*4.941)+29.777
  
  age<-matrix(0, nrow=7, ncol=1165) # initialize the age matrix
  Diameter<-matrix(0, nrow=7, ncol=1165) # initialize the diameter matrix
  TASB<-matrix(0, nrow=7, ncol=1165) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*1^(-0.27547))*(CN_SCALE^(1.8743))
  
  for (j in 1:1165){ # specify tree per hectare
    
    for (i in 2:7){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*age[i,j]^(-0.27547))*(CN_SCALE^(1.8743))
      
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
  df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, precip, CN_SCALE_LA)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 6
  mylist$Ziehm_1992 <- df
  
  
  # Bohn 2016 (18th ---------------------------------------------------------
  
  temp<-20.04
  
  temp2<-temp^2
  
  precip<-1512
  
  # CN scaled with FIA data
  C_AVG<-((675/10)*(1/3))+((260/10)*(2/3))
  
  N_AVG<-((1309/100)*(1/3))+((750/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  # set stand age and density
  observed.a<-c(30)
  plot_density<-c(1180)
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*1^(-0.27547))*(CN_SCALE^(1.8743))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*age[i,j]^(-0.27547))*(CN_SCALE^(1.8743))
      
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
  modeled.d<-sqrt(sum(Diameter[30,]^2)/1180) #converting to QMD
  observed.d<-c(7.08661) #QMD 18cm
  
  # set up dataframe to store simulated data
  df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, precip, CN_SCALE)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 7
  mylist$Bohn_2016 <- df
  
  
  
  # Hedman 1999 -------------------------------------------------------------
  
  temp<-19.10
  
  temp2<-temp^2
  
  precip<-1439
  
  # CN scaled with FIA data
  C_AVG<-((529/10)*(1/3))+((205/10)*(2/3))
  
  N_AVG<-((504/100)*(1/3))+((197/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  # set stand age and density
  observed.a<-c(49)
  plot_density<-c(529)
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*1^(-0.27547))*(CN_SCALE^(1.8743))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*age[i,j]^(-0.27547))*(CN_SCALE^(1.8743))
      
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
  modeled.d<-mean(Diameter[49,which(Diameter[49,]>=0.984252)])
  observed.d<-c(7.874)
  
  # set up dataframe to store simulated data
  df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, precip, CN_SCALE)
  
  df<-data.frame(df)
  
  colnames(df)<-c("Observed Diameter","Modeled Diameter","Age", "Tree Density", "Temperature", "Temperature2", "Precipitation", "Soil CN")
  
  # data will be saved as list 8
  mylist$Hedman_1999 <- df
  
  final_list[[o]] <- do.call(rbind.data.frame, mylist)
}
df2 <- do.call(rbind.data.frame, mylist)
write.csv(df2, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/after_correction5.4.csv")


# Compare observed diameter vs modeled diameter ---------------------------
slashcoef_list<-list()
par(mfrow=c(2,5))
for (n in 1:10){
  
  # model.1<-lm(data = final_list[[2]], log10(`Observed Diameter`/`Modeled Diameter`)~Temperature+Temperature2+log10(Age)+Aridity+`Tree Density`+log10(`Soil CN`))
  # summary(model.1)
  # 
  # res.age<-residuals(model.1)
  # plot(res.age~df2$Age)
  # plot(res.age~df2$Aridity)
  
  coef<-lm(data = final_list[[n]], log10(`Observed Diameter`/`Modeled Diameter`)~Temperature+Temperature2+log10(Age)+log10(`Soil CN`))$coefficients
  names(coef) <- NULL
  df<-rbind(coef)
  slashcoef_list[[n]]<-df
  
  plot(data = final_list[[n]], `Observed Diameter`~`Modeled Diameter`, xlim=c(1,8), ylim=c(1,8), xlab="Modeled", ylab="Observed",
       col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")
  
}







# Calculating total above stump C biomass for year 2100 for 700 tph by region

Panhandle<-vector()
North<-vector()
Central<-vector()
South<-vector()

par(mfrow=c(2,2))

for (b in 1:10){
  
  # Panhandle TASB ----------------------------------------------------------
  
  temp<-19.84
  
  temp2<-temp^2
  
  precip<-1544
  
  # CN scaled with FIA data
  
  C_AVG<-(539/10)*(1/3)+((165/10)*(2/3))
  
  N_AVG<-(1297/100)*(1/3)+((527/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  
  age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
  Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
  TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*1^(-0.27547))*(CN_SCALE^(1.8743))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:80){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*age[i,j]^(-0.27547))*(CN_SCALE^(1.8743))
      
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
  Panhandle[b]<-sum(TASB[80,])*.5*1000*(1/10000)
  hist(Diameter[80,]*2.54, main = "Panhandle", xlab = "Diameter (cm)")
  
  # Calculating North TASB --------------------------------------------------
  
  temp<-20.30
  
  temp2<-temp^2
  
  precip<-1300
  
  # CN scaled with FIA data
  C_AVG<-((317/10)*(1/3))+((110/10)*(2/3))
  
  N_AVG<-((1665/100)*(1/3))+((300/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  
  age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
  Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
  TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*1^(-0.27547))*(CN_SCALE^(1.8743))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:80){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*age[i,j]^(-0.27547))*(CN_SCALE^(1.8743))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on Diameter2 class
      if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
      else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
      else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
      else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
      else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
      
      # Calculate the Diameter2 for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  North[b]<-sum(TASB[80,])*.5*1000*(1/10000)
  
  hist(Diameter[80,]*2.54, main = "North", xlab = "Diameter (cm)")
  
  # TASB Central ------------------------------------------------------------
  
  temp<-22.29
  
  temp2<-temp^2
  
  precip<-1278
  
  # CN scaled with FIA data
  C_AVG<-((475/10)*(1/3))+((144/10)*(2/3))
  
  N_AVG<-((1729/100)*(1/3))+((1137/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
  Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
  TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*1^(-0.27547))*(CN_SCALE^(1.8743))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:80){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*age[i,j]^(-0.27547))*(CN_SCALE^(1.8743))
      
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
  Central[b]<-sum(TASB[80,])*.5*1000*(1/10000)
  hist(Diameter[80,]*2.54, main = "Central", xlab = "Diameter (cm)")
  
  # South TASB --------------------------------------------------------------
  
  temp<-23.80
  
  temp2<-temp^2
  
  precip<-1330
  
  # CN scaled with FIA data
  C_AVG<-((1052/10)*(1/3))+((1017/10)*(2/3))
  
  N_AVG<-((1721/100)*(1/3))+((1289/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
  Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
  TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*1^(-0.27547))*(CN_SCALE^(1.8743))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:80){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-74.524 + 6.8046*temp - 0.1611*temp2 - 0.0001241*precip))*(0.72453*age[i,j]^(-0.27547))*(CN_SCALE^(1.8743))
      
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
  South[b]<-sum(TASB[80,])*.5*1000*(1/10000)
  hist(Diameter[80,]*2.54, main = "South", xlab = "Diameter (cm)")
  
}

TASB_Panhandle<-c(mean(Panhandle), sd(Panhandle))
TASB_North<-c(mean(North), sd(North))
TASB_Central<-c(mean(Central), sd(Central))
TASB_South<-c(mean(South), sd(South))
