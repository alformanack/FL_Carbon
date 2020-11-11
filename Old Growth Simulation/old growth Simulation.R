rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")

sitetree<-read.csv("aridity.csv", header=T, sep=",", stringsAsFactors = F)


#Lostmans Pine aridity/temp
aridity1<-6962*0.0001
temp1<-23.83206

#Long Pine Key and Pines West aridity/temp
aridity2<-7328*0.0001
temp2<-24.32199

#Lostmans Pine CN
C_AVG<-(1105/10)*(1/3)+((860/10)*(2/3))

N_AVG<-(1520/100)*(1/3)+((1290/100)*(2/3))

CN<-C_AVG/N_AVG
CN_SCALE_LOP<-(CN*6.045)+19.775

#Long Pine Key and PInes West CN
C_AVG<-(1125/10)*(1/3)+((1006/10)*(2/3))

N_AVG<-(1369/100)*(1/3)+((1051/100)*(2/3))

CN<-C_AVG/N_AVG
CN_SCALE_LPK<-(CN*6.045)+19.775



plot_density<-c(673,643,718)

observed.a<-c(200, 52, 52)


# Lostmans Pine simulation ------------------------------------------------


  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the diameter matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.8156395 - 0.004348023*temp1 - 0.2872396*aridity1))*(0.3535108*1^(-0.6464892))


  
  
  for (j in 1:plot_density[1]){ # for each tree (num of trees = plot_density[t]) 
    
 
    
    for (i in 2:observed.a[1]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.8156395 - 0.004348023*temp1 - 0.2872396*aridity1))*(0.3535108*age[i,j]^(-0.6464892))
   
     
      
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
      
      #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }

hist(Diameter[200,]*2.54, breaks=10)  
mean(Diameter[200,which(Diameter[200,]>=.732499)]) 

h = hist(x) # or hist(x,plot=FALSE) to avoid the plot of the histogram
density = (Diameter[300,]*2.54)/sum(Diameter[300,]*2.54)
                                            h$counts/sum(h$counts)*100
hist((Diameter[300,]*2.54)/sum(Diameter[300,]*2.54), breaks=5)

ggplot2.histogram(Diameter[300,]*2.54, data=aridity, xName='dens',
                  groupName='lines', legendPosition="top",
                  alpha=0.1) + 
  labs(x="X", y="Count") +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme_bw()+
  theme(legend.title=element_blank()) + 
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent)




# LOP simulation with correction ------------------------------------------

age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
Diameter<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the diameter matrix

# initialize the diameter for the first year
Diameter[1,]<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*1^(-0.114599))
#Diameter[1,]<-(10^(0.8156395 - 0.004348023*temp1 - 0.2872396*aridity1))*(0.3535108*1^(-0.6464892))




for (j in 1:plot_density[1]){ # for each tree (num of trees = plot_density[t]) 
  
  
  
  for (i in 2:observed.a[1]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
    
    age[i,j]<-age[i-1,j]+1
    
    growth<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*age[i,j]^(-0.114599))
    #growth<-(10^(0.8156395 - 0.004348023*temp1 - 0.2872396*aridity1))*(0.3535108*age[i,j]^(-0.6464892))
    
    
    
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
    
    #use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
    TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    # If the tree dies, plant a new tree (age = 0)
    if (M==1){age[i,j]<-0}
    
  }
}

hist(Diameter[200,]*2.54, breaks=10)  
mean(Diameter[200,which(Diameter[200,]>=.732499)]) 

h = hist(x) # or hist(x,plot=FALSE) to avoid the plot of the histogram
density = (Diameter[300,]*2.54)/sum(Diameter[300,]*2.54)
h$counts/sum(h$counts)*100
hist((Diameter[300,]*2.54)/sum(Diameter[300,]*2.54), breaks=5)

ggplot2.histogram(Diameter[300,]*2.54, data=aridity, xName='dens',
                  groupName='lines', legendPosition="top",
                  alpha=0.1) + 
  labs(x="X", y="Count") +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme_bw()+
  theme(legend.title=element_blank()) + 
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent)


# Long Pine Key Simulation ------------------------------------------------



  age<-matrix(0, nrow=observed.a[2], ncol=plot_density[2]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[2], ncol=plot_density[2]) # initialize the diameter matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(0.8156395 - 0.004348023*temp2 - 0.2872396*aridity2))*(0.3535108*1^(-0.6464892))
  
  
  
  for (j in 1:plot_density[2]){ # for each tree (num of trees = plot_density[t]) 
    
    
    
    for (i in 2:observed.a[2]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-4.36587 - 0.004348023*temp2 + 5.34389*aridity2))*(0.870971*age[i,j]^(-0.129029))
      
      
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

hist(Diameter[52,]*2.54, breaks=5) 
mean(Diameter)

# Pines West Simulation ----------------------------------------------------

  
  age<-matrix(0, nrow=observed.a[3], ncol=plot_density[3]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[3], ncol=plot_density[3]) # initialize the diameter matrix
  
  # initialize the diameter for the first year
 
  Diameter[1,]<-(10^(0.8156395 - 0.004348023*temp2 - 0.2872396*aridity2))*(0.3535108*1^(-0.6464892))
  
  
  
  for (j in 1:plot_density[3]){ # for each tree (num of trees = plot_density[t]) 
    
    
    
    for (i in 2:observed.a[3]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-4.36587 - 0.004348023*temp2 + 5.34389*aridity2))*(0.870971*age[i,j]^(-0.129029))
      
      
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
  
hist(Diameter[52,]*2.54, breaks=5)
  
  
write.csv(d, file ="C:/Users/Alicia/Documents/GitHub/FL_Carbon/bradford county simulation/corrected_modeled_d_witharidity.csv" )

plot(observed.d~d)
abline(0,1)


