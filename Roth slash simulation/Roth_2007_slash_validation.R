rm(list=ls())

# C_AVG<-(634.5/10)*(1/3)+((231/10)*(2/3))
# 
# N_AVG<-(1245.5/100)*(1/3)+((412/100)*(2/3))                 
# 
# CN<-C_AVG/N_AVG 
# CN_SCALE<-(CN*6.045)+19.775
Waldo_FL<-8058
Perry_FL<- 9011

temp<-(20.30676 + 19.65989)/2
aridity<-((Waldo_FL + Perry_FL)/2)*0.0001

plot_density<-c(1334, 2990, 1334, 2990, 1334, 2990)
observed.a<-c(2, 2, 3, 3, 5, 5)

d <- matrix(0, ncol = 1, nrow = length(observed.a))
a<-matrix(0, ncol = 1, nrow = length(observed.a))


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
      
      
      
      
      #growth<-(10^(.381+.012*19.98-.0001*284.47))*(.346*age[i,j]^(-.654))*CN_SCALE^(-.069)
      #growth<-(10^(.381+.0012*20.05-.0001*266.82))*(.346*age[i,j]^(-.4))*CN_SCALE^(-.0069)
      #growth<-growth*1.3
      #  M<-rbinom(1,1,0.0083*age[i,j])
      # M<-rbinom(1,1,min(0.4,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))))
      #M<-rbinom(1,1,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))/8)
      
      
      
      if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
      else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
      else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
      else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
      else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
      
      #else {M<-rbinom(1,1,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))/8)}
      #if (7.97<Diameter<=11.81){M<- rbinom(1,1,(.15/8))},
      #if (11.81<Diameter<=15.75){M<- rbinom(1,1,(.1/8))},
      
      Diameter[i,j]<-Diameter[i-1,j]+growth-M*(Diameter[i-1,j]+growth)
      
      #TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
      
      if (M==1){
        age[i,j]<-0
        }
      }
      }
  a[t,] <- mean(age[observed.a[t],])
  d[t,] <- mean(Diameter[observed.a[t],])  # take the mean of the last row (observed.a[t,])
  }

write.csv(d, file ="C:/Users/Alicia/Documents/GitHub/FL_Carbon/roth slash simulation/modeled_d_aridity.csv" )

plot(d~observed.a)  
mean(Diameter[5,])
observed.d<-rnorm(2990, mean = 8.7/2.54, sd= 0.76210/2.54)
hist(observed.d)

hist(Diameter[5,], main="DBH - Modeled (red) vs Observed (green)", col="red",breaks=10, 
     xlim=c(0, 7))
abline(v=mean(Diameter[5,]), col="black", lty = 1, lwd=4)
hist(observed.d, breaks=10, add=T, col=rgb(0, 1, 0, 0.5))
abline(v=mean(unburned), col="grey50", lty =1, lwd=4)


# Simulation with error correction for age --------------------------------

rm(list=ls())

Waldo_FL<-8058
Perry_FL<- 9011

temp<-(20.30676 + 19.65989)/2

aridity<-((Waldo_FL + Perry_FL)/2)*0.0001

# C_AVG<-(634.5/10)*(1/3)+((231/10)*(2/3))
# 
# N_AVG<-(1245.5/100)*(1/3)+((412/100)*(2/3))                 
# 
# CN<-C_AVG/N_AVG 
# CN_SCALE<-(CN*6.045)+19.775
plot_density<-c(1334, 2990, 1334, 2990, 1334, 2990)
observed.a<-c(2, 2, 3, 3, 5, 5)

d <- matrix(0, ncol = 1, nrow = length(observed.a))
a<-matrix(0, ncol = 1, nrow = length(observed.a))

for(t in 1:length(observed.a)) {
  
  age<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[t], ncol=plot_density[t]) # initialize the diameter matrix
  
  # initialize the diameter for the first year
  #Diameter[1,]<-(10^(.381+.012*20.05-.0001*266.82))*(.346*1^(-.654))*CN_SCALE^(-.069)
  #Diameter[1,]<-(10^(-4.36587 - 0.004348023*temp + 5.34389*aridity))*(0.870971*1^(-0.129029))
  Diameter[1,]<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*1^(-0.114599))




  for (j in 1:plot_density[t]){
  
  
    for (i in 2:observed.a[t]){
    age[i,j]<-age[i-1,j]+1
    growth<-(10^(-4.97836 - 0.004348023*temp + 6.06674*aridity))*(0.885401*age[i,j]^(-0.114599))
    #growth<-(10^(-4.36587 - 0.004348023*temp + 5.34389*aridity))*(0.870971*age[i,j]^(-0.129029))
    #growth<-(10^(-3.237+.012*19.98+.0114*284.47))*(.866*age[i,j]^(-.134))*CN_SCALE^(-.069)
    #growth<-(10^(.381+.012*19.98-.0001*284.47))*(.346*age[i,j]^(-.654))*CN_SCALE^(-.069)*(0.466*age[i,j]^(.534))
    #growth<-(10^(.381+.0012*20.05-.0001*266.82))*(.346*age[i,j]^(-.4))*CN_SCALE^(-.0069)
    #growth<-growth*1.3
    #  M<-rbinom(1,1,0.0083*age[i,j])
    # M<-rbinom(1,1,min(0.4,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))))
    #M<-rbinom(1,1,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))/8)
    
    
    
    if (Diameter[i,j]<=3.94){ M<- rbinom(1,1,(.4/8))}
    else if (Diameter[i,j]>3.94 & Diameter[i,j]<=7.97){M<-rbinom(1,1,(.2/8))}
    else if (Diameter[i,j]>7.97 & Diameter[i,j]<=11.81){M<-rbinom(1,1,(.15/8))}
    else if (Diameter[i,j]>11.81 & Diameter[i,j]<=15.75){M<-rbinom(1,1,(.1/8))}
    else if (Diameter[i,j]>15.75){M<-rbinom(1,1,(0.01/8))}
    
    #else {M<-rbinom(1,1,(1/(1+exp(-0.05+0.2*Diameter[i-1,j])))/8)}
    #if (7.97<Diameter<=11.81){M<- rbinom(1,1,(.15/8))},
    #if (11.81<Diameter<=15.75){M<- rbinom(1,1,(.1/8))},
    
    Diameter[i,j]<-Diameter[i-1,j]+growth-M*(Diameter[i-1,j]+growth)
    
    #TASB [i,j]<-0.0578*(Diameter[i,j]^2.41)*(age[i,j]^.204)
    
    if (M==1){
      age[i,j]<-0
    }
  }}
  a[t,] <- mean(age[observed.a[t],])
  d[t,] <- mean(Diameter[observed.a[t],])  # take the mean of the last row (observed.a[t,])
}

write.csv(d, file ="C:/Users/Alicia/Documents/GitHub/FL_Carbon/roth slash simulation/new_corected_modeled_d_aridity.csv" )

mean(Diameter[5,])
observed.d<-rnorm(2990, mean = 8.7/2.54, sd= 0.76210/2.54)
hist(observed.d)

hist(Diameter[5,], main="DBH - Modeled (red) vs Observed (green)", col="red",breaks=10, 
     xlim=c(0, 7))
abline(v=mean(Diameter[5,]), col="black", lty = 1, lwd=4)
hist(observed.d, breaks=10, add=T, col=rgb(0, 1, 0, 0.5))
abline(v=mean(unburned), col="grey50", lty =1, lwd=4)




