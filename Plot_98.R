# Plot 98 ----------------------------------------------
rm(list=ls())

# observed diameter distributions by plot, use observed.DBH[[98]]
load("C:/Users/Alicia/Documents/GitHub/FL_Carbon/observed.DBH_site_tree.RData") 

mylist<-list()

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")

envdata<-read.csv("slash_env_data_site_index.csv", header=T, sep=",")

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
predict.tasb<-matrix(nrow = 10, ncol = 1,0) #total above stump biomass
predict.d<-matrix(nrow = 10, ncol = 1,0) #store average diameter for each simulation run
initial<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*1^(-0.4215787)) #initial diameter calculated from growth at year 1
initial_BA<-(pi*(initial*2.47)^2)/40000 #basal area per tree per hectare at year 1
initial_height<-10^(0.8730716)+initial^(0.8976928) #height of trees at year 1


# initialize the diameter for the first year
Diameter<-array(dim=c(100,100, observed.a), 0) # initialize diameter array
age<-array(dim=c(100,100, observed.a), 0) # initialize age array
TASB<-array(dim=c(100,100, observed.a), 0) # initialize the total above-stump biomass array
height<-array(dim=c(100,100, observed.a), 0) # initialize height (ft) array
BA<-array(dim=c(100,100, observed.a), 0) # initialize basal area (m^2/hectare)
RBA<-array(dim=c(100,100, observed.a), 0) # initialize relative basal area array
tbaha<-vector() # initialize vector for the total basal area per hectare

# vectors usesd to calculate RBA
a_1<-vector()
a_2<-vector()
a_3<-vector()
a_4<-vector()
a_5<-matrix(nrow = 5, ncol = 5,0)
a_6<-matrix(nrow = 5, ncol = 5,0)
a_7<-matrix(nrow = 5, ncol = 5,0)
a_8<-matrix(nrow = 5, ncol = 5,0)


#numbering rows and columns in array
Diameter[1:100,1:100,1]<-seq(1:10000)
BA[1:100,1:100,1]<-seq(1:10000)
height[1:100,1:100,1]<-seq(1:10000)

#selecting random numbers of array to place trees
trees <- sample.int(10000, plot_density, replace = F)

#load year 1 diameters where trees were randomly placed
Diameter[,,1] <- replace(Diameter[,,1], Diameter[,,1] %in% trees, initial)

Diameter[,,1] <- replace(Diameter[,,1], Diameter[,,1]!= initial, NA)

#load year 1 basal area where trees were randomly placed in array
BA[,,1] <- replace(BA[,,1], BA[,,1] %in% trees, initial_BA)

BA[,,1] <- replace(BA[,,1], BA[,,1]!=initial_BA, NA)

#load year 1 height where trees were randomly placed in array
height[,,1] <- replace(height[,,1], height[,,1] %in% trees, initial_height)

height[,,1] <- replace(height[,,1], height[,,1]!=initial_height, NA)

tbaha[1]<- initial_BA*plot_density

#introduce variable used for mortality
M <- numeric(length = 1)




for (o in 1:2){  # repeate simulation 10 times to account for stochasticity of mortality
  
  for (h in 2:observed.a){ #run simulation for h years
    
    k<-0
    
    for (j in 1:100){ # columns of array
      
      for (i in 1:100){ # rowsof array
        
        Diameter[i+(10^2)*(h-1),j+(10^2)*(h-1)]<-Diameter[i,j] #mortality calculations depend on previous diameter
        
        if (!is.na(Diameter[i,j,h])) { # only calculate growth and mortality for tree that exist
          
          k<-k+1
          
          age[i,j,h]<-age[i,j,h-1]+1
          
          growth<-(10^(-1.431904 + 0.182686*temp - 0.004512*temp2 - 0.301793*aridity))*(0.5784213*age[i,j,h]^(-0.4215787))
          
          BA[i,j,h]<-BA[i,j,h-1]
          
          tbaha[h]<-sum(BA[,,h-1], na.rm=TRUE)
          
          height[i,j,h]<- height[i,j,h-1]
          
          # create 11x11 grid around tree, collect diameters within grid that are greater than or equal to ith jth tree
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
          RBA[i,j,h]<-BA[i,j,h]/(1/a_11) #relative basal area calculated
          
          
          
          
          # Mortality based on diameter class
        
          prob_denom<-(1+exp(-(-4.2899-0.1022*(Diameter[i,j,h]*2.54)+0.2146*(height[i,j,h]/3.281)-
                                 11.54*RBA[i,j,h]+0.0699*SI+0.1312*tbaha[h]-0.0079*(height[i,j,h]/3.281)*
                                 tbaha[h])))
          M <- rbinom(1, 1,(1/prob_denom/8)) #eight year mortality probability converted to yearly mortality probability
         
          
          
          # Calculate the diameter for ith jth tree for h year
          Diameter[i,j,h]<-Diameter[i,j,h-1] + growth - M*(Diameter[i,j,h-1]+growth) #if M=1, tree dies, diameter=0
          BA[i,j,h]<-(pi*(Diameter[i,j,h]*2.54)^2)/40000
          tbaha[h]<-sum(BA[,,h], na.rm=TRUE)
          height[i,j,h]<-10^(0.8730716)+Diameter[i,j,h]^(0.8976928)
          
          # use diameter and age to calculate total aboveground biomass of the ith jth tree in the h year
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