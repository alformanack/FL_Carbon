rm(list=ls())

load(file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/bradford county simulation/models.Rdata")

Lawtey<-8244.150862068966
Starke<-8203.438775510203
Hampton<-8100.927419354839
Brooker<-8155.30223880597

aridity<-((Lawtey + Starke + Hampton + Brooker)/4)*0.0001
temp<-20.05

# C_AVG<-(418.4782/10)*(1/3)+((156.6189/10)*(2/3))
# 
# N_AVG<-(1177.5531/100)*(1/3)+((459.7207/100)*(2/3))                 
# 
# 
# CN<-C_AVG/N_AVG 
# CN_SCALE<-(CN*6.045)+19.775



plot_density<-c(1392,1280,1648,1696,1728,1792,880,1080,1280,
                1008,1040,784,928,1184,1056,1056,928,976)


d <- matrix(0, ncol = 1, nrow = length(observed.a))
a<-matrix(0, ncol = 1, nrow = length(observed.a))

for(t in 1:length(observed.a)) {
  
  age<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the diameter matrix
  
  # initialize the diameter for the first year
  #Diameter[1,]<-(10^(.381+.012*20.05-.0001*266.82))*(.346*1^(-.654))*CN_SCALE^(-.069)
  Diameter[1,]<-(10^(0.8156395 - 0.004348023*temp - 0.2872396*aridity))*(0.3535108*1^(-0.6464892))
  #slashgrowth<-(10^slash[1])*(10^(slash[2]*20))*(slash[3]*slashpine$AGEDIA^-.649)*(10^(slash[4]*.8753))

  #Diameter[1,]<-(10^(.381+.012*20.05-.0001*266.82))*(.346*1^(-.254))*CN_SCALE^(-.0069)
  
  
  for (j in 1:plot_density[t]){ # for each tree (num of trees = plot_density[t]) 
    
    #Diameter[1,]<-(10^(.381+.0012*20.05-.0001*266.82))*(.346*1^(-.654))*CN_SCALE^(-.0069)
    
    for (i in 2:observed.a[t]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(0.8156395 - 0.004348023*temp - 0.2872396*aridity))*(0.3535108*age[i,j]^(-0.6464892))
      
      #growth<-(10^(.381+.012*20.05-.0001*266.82))*(.346*age[i,j]^(-.654))*CN_SCALE^(-.069) growth with AWC
      #growth<-(10^(.381+.012*20.05-.0001*266.82))*(.346*age[i,j]^(-.254))*CN_SCALE^(-.0069)
      
      #growth<-(10^(.381+.0012*20.05-.0001*266.82))*(.346*age[i,j]^(-.4))*CN_SCALE^(-.0069)
      #growth<-growth*1.3
      #  M<-rbinom(1,1,0.0083*age[i,j])
      # M<-rbinom(1,1,min(0.4,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))))
      #M<-rbinom(1,1,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))/8)
      
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
  a[t,] <- mean(age[observed.a[t,],])
  d[t,] <- mean(Diameter[observed.a[t,],])  # take the mean of the last row (observed.a[t,])
}


plot(d, observed.d)
abline(0,1)


plot(observed.a, observed.d)
lines(observed.a, d)
lines(observed.a, observed.d, col="red")


hist(age[34,])


error<-log10(observed.d/d)
error<-log10(observed.d)-log10(d)
#error<-log10(d)/log10(observed.d) 
#error<-log10(d/observed.d)
df<-cbind(error,plot_density,observed.a)

df<-data.frame(df)

colnames(df)<-c("error","plot_density","age")
mod<-lm(error~log10(age)+plot_density,data=df)
summary(mod)
mod2<-lm(error~log10(age),data=df)
summary(mod2)




#write.csv(d, file ="C:/Users/Alicia/Documents/GitHub/FL_Carbon/bradford county simulation/modeled_d_aridity.csv" )



#save(modeled.a,modeled.d,observed.a,observed.d, file="C:/Users/Alicia/Documents/GitHub/FL_Carbon/bradford county simulation/models.Rdata")


# Simulation corrected error ----------------------------------------------

rm(list=ls())

load(file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/bradford county simulation/models.Rdata")

Lawtey<-8244.150862068966
Starke<-8203.438775510203
Hampton<-8100.927419354839
Brooker<-8155.30223880597

aridity<-((Lawtey + Starke + Hampton + Brooker)/4)*0.0001
temp<-20.05

# C_AVG<-(418.4782/10)*(1/3)+((156.6189/10)*(2/3))
# 
# N_AVG<-(1177.5531/100)*(1/3)+((459.7207/100)*(2/3))                 
# 
# CN<-C_AVG/N_AVG 
# CN_SCALE<-(CN*6.045)+19.775



plot_density<-c(1392,1280,1648,1696,1728,1792,880,1080,1280,
                1008,1040,784,928,1184,1056,1056,928,976)

TASB<-matrix(0, nrow = 500, ncol = 700)

d <- matrix(0, ncol = 1, nrow = length(observed.a))
a<-matrix(0, ncol = 1, nrow = length(observed.a))

for(t in 1:length(observed.a)) {
  
  age<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the diameter matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*1^(-0.114599))
  #Diameter[1,]<-(10^(-4.36587 - 0.004348023*temp + 5.34389*aridity))*(0.870971*1^(-0.129029))
  #Diameter[1,]<-(10^(-3.237+.012*20.05+.0114*266.82))*(.866*1^(-.134))*CN_SCALE^(-.069)
  #Diameter[1,]<-(10^(.381+.012*20.05-.0001*266.82))*(.346*1^(-.654))*CN_SCALE^(-.069)*(0.466*1^(-.534))
  #Diameter[1,]<-(10^(.381+.012*20.05-.0001*266.82))*(.346*1^(-.254))*CN_SCALE^(-.0069)
  
  
  for (j in 1:plot_density[t]){ # for each tree (num of trees = plot_density[t]) 
    
    #Diameter[1,]<-(10^(.381+.0012*20.05-.0001*266.82))*(.346*1^(-.654))*CN_SCALE^(-.0069)
    
    for (i in 2:observed.a[t]){ # get age of the tree for each observed year (num of observed years = observed.a[t])
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*age[i,j]^(-0.114599))
      #growth<-(10^(-4.36587 - 0.004348023*temp + 5.34389*aridity))*(0.870971*age[i,j]^(-0.129029))
      #growth<-(10^(.381+.012*20.05-.0001*266.82))*(.346*age[i,j]^(-.654))*CN_SCALE^(-.069)*(0.466*age[i,j]^(-.534))
      #growth<-(10^(-3.237+.012*20.05+.0114*266.82))*(.866*age[i,j]^(-.134))*CN_SCALE^(-.069) last model
      #growth<-(10^(0.8156395-0.004348023*20.05))*(0.3535108*age[i,j]^(-0.6464892))*Aridity^(-0.2872396) #model with aridity
      #growth<-(10^(.381+.012*20.05-.0001*266.82))*(.346*age[i,j]^(-.254))*CN_SCALE^(-.0069)
      
      #growth<-(10^(.381+.0012*20.05-.0001*266.82))*(.346*age[i,j]^(-.4))*CN_SCALE^(-.0069)
      #growth<-growth*1.3
      #  M<-rbinom(1,1,0.0083*age[i,j])
      # M<-rbinom(1,1,min(0.4,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))))
      #M<-rbinom(1,1,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))/8)
      
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
      
      # Use diameter and age to calculate total aboveground biomass of the jth tree in the ith year
      TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){age[i,j]<-0}
      
    }
  }
  a[t,] <- mean(age[observed.a[t,],])
  d[t,] <- mean(Diameter[observed.a[t,],])  # take the mean of the last row (observed.a[t,])
}

write.csv(d, file ="C:/Users/Alicia/Documents/GitHub/FL_Carbon/bradford county simulation/new_corrected_modeled_d_witharidity.csv" )

plot(observed.d~d)
abline(0,1)


