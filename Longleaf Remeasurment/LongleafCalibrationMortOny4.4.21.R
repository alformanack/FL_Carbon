rm(list=ls())
## Changing directory 
# 
# # Assuming the first lib path is the problematic one:
# path <- "U:\Documents\R"
# # Map to drive letter Z
# system2("net", args=c("use", "Z:", path))
# # # Update so that packages are installed to that directory
# .libPaths("Z:/")
# 
# # Install packages
# install.packages("ggplot2")
# install.packages("mvnfast")
lapply(c("ggplot2","mvnfast"), require, character.only = T)

setwd("~/GitHub/FL_Carbon/Longleaf Remeasurment")
envdata<-read.csv("LongleafEnvData2.csv", header=T, sep=",") 
load("longleafAgeTotals2.rdata")
load("longleafAgeTotalsEnd2.rdata")
load("longleafDIATotalsEnd2.rdata")
load("longleafDIATotals2.rdata")
load("longleafPlotStart2.rdata")
load("longleafPlotEnd2.rdata")
# setwd("C:/Users/Alicia/Documents/GitHub/tree_growth_FL")
# source("generate_pars.R")

## Load functions
generate.pars <- function(p_op,pmin,pmax,d) {
  while(TRUE){
    rand <- runif(length(pmin))
    pnew <- p_op+(rand-0.5)*(pmax-pmin)/d
    if  (Reduce("&", pnew>pmin & pnew<pmax))
      break
  }
  pnew
}

generate.pars.cov <- function(p_op,pmin,pmax, covars) {
  while(TRUE){
    pnew <- rmvn(1, mu = p_op, sigma = covars,ncores = 12)
    if  (Reduce("&", pnew>pmin&pnew<pmax))
      break
  }
  pnew
}

mylist<-list()

Diameter.all<-list()



# Plot 170 ----------------------------------------------
#set prior ranges
pmin <- c()
pmax <- c()
pmin[1] <- 0.0;pmin[2] <- -0.01;pmin[3] <- 0.0
pmax[1] <- 0.04;pmax[2] <- 0.0;pmax[3] <- 0.0001


par.name <- c("a1","b1","b2")
p_op <- c( 2.109e-02, -1.048e-03, 1.324e-05)


names(p_op)<-par.name
no.simu <- 500000   #10000
d <- 6
a<-c(21, 19, 16,  3, 23, 14,  9,  1, 20, 11)
# a<-c(1, 3, 4, 6, 8, 9, 11:21, 23)
st_dev<-4

for (s in a){
  
  
  plot_density<-diameter.totals[[s]]
  observed.a<-envdata[s,6]
  CN <- envdata[s,16]
  aridity <- envdata[s,17]
  temp <- envdata[s,12]
  ages<-age.totals[[s]]
  
  
  
  # for (o in 1:10){
  age<-matrix(0, nrow=observed.a, ncol=length(plot_density)) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a, length(plot_density)) # initialize the diameter matrix
  
  
  # initialize the diameter for the first year
  Diameter[1,]<-plot_density
  age[1,]<-ages
  
  for (j in 1:length(plot_density)){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      
      # growth<-(10^(a1 - b1*envdata[s,16] + b2*envdata[s,16]*envdata[s,17] - b3*envdata[s,17]))*(b4*age[i,j]^(b5))
      # growth<-(10^(a1 - b1*CN + b2*CN*aridity - b3*aridity))*(b4*age[i,j]^(b5))
      growth<-(10^(1.774252 - 0.007376*CN + 0.008805*CN*aridity - 1.127642*aridity- 0.034125*temp))*(0.5718692*age[i,j]^(-0.4281308))
      # growth <- 0.1
      # growth<-(10^(0.9333281 - 0.009259*CN + 0.011340*CN*aridity - 0.977477*aridity))*(0.5718692*age[i,j]^(-0.4281308))
     
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]>=0) {
        M<- rbinom(1,1,(p_op["a1"] +(p_op["b1"] *Diameter[i,j]*2.54)+p_op["b2"]*((Diameter[i,j]*2.54)^2)))
        
      }
      
      
      # Calculate the diameter for jth tree for the ith observed year
      Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
      
      # If the tree dies, plant a new tree (age = 0)
      if (M==1){
        age[i,j]<-0
      }
    }
  }
  mylist[[s]] <- df
  Diameter.all[[s]]<-Diameter[observed.a,]
}

J_old <- mapply(function(x, y){
  # calc <- (sort(x) - sort(y))^2/(2*(0.6*y)^2)
  calc <- (sort(x) - sort(y))^2/(2*(st_dev)^2) ## try 2 instead of 4 as SD (denominator)
  return(calc)
}, Diameter.all, diameter.totals.end)

J_old <-unlist(J_old)


# J_last <- 100000
J_last <- sum(J_old)
updated <- 0

#
p_rec <- matrix(NA,length(pmin), no.simu)
p_upgraded <- matrix(NA, length(pmin), no.simu)
J_keep <- rep(NA,no.simu)
J <- c()
DJ <- c()
J_new <- c()
sd <- 2.38/sqrt(length(pmin))
simu <- 0

# system.time({
for (d1 in 1:no.simu) {
  simu <- simu+1
  if (simu <= 2000) { #two steps; 1-default sampling; 2-sampling from covariances of each pars
    pnew <- generate.pars(p_op, pmin, pmax, d)
  } else {
    pnew <- generate.pars.cov(p_op, pmin, pmax, covars)
  }
  
  p_rec[,simu] <- p_op #save pnew
  
  #assign pars
  for (b in 1:length(par.name)) {
    assign(par.name[b], pnew[b])
  }
  
  names(pnew)<-par.name
  st_dev<-4 
   
  for (s in a){
    
    # set stand age and density
    plot_density<-diameter.totals[[s]]
    observed.a<-envdata[s,6]
    CN <- envdata[s,16]
    aridity <- envdata[s,17]
    temp <- envdata[s,12]
    ages<-age.totals[[s]]
    
    
    
    # for (o in 1:10){
    age<-matrix(0, nrow=observed.a, ncol=length(plot_density)) # initialize the age matrix
    Diameter<-matrix(0, nrow=observed.a, length(plot_density)) # initialize the diameter matrix
    TASB<-matrix(0, nrow=observed.a, length(plot_density)) # initialize the total above-stump biomass matrix
    
    # initialize the diameter for the first year
    Diameter[1,]<-plot_density
    age[1,]<-ages
    
    for (j in 1:length(plot_density)){ # specify tree per hectare
      
      for (i in 2:observed.a){ # specify how long to run the simulation (years)
        
        age[i,j]<-age[i-1,j]+1
        
        
        # growth<-(10^(a1 - b1*envdata[s,16] + b2*envdata[s,16]*envdata[s,17] - b3*envdata[s,17]))*(b4*age[i,j]^(b5))
        # growth<-(10^(a1 - b1*CN + b2*CN*aridity - b3*aridity))*(b4*age[i,j]^(b5))
        # growth<-(10^(pnew["a1"] - pnew["b1"]*CN + pnew["b2"]*CN*aridity - pnew["b3"]*aridity- pnew["b4"]*temp))*(pnew["b5"]*age[i,j]^(pnew["b6"]))
        growth<-(10^(1.774252 - 0.007376*CN + 0.008805*CN*aridity - 1.127642*aridity- 0.034125*temp))*(0.5718692*age[i,j]^(-0.4281308))
        
        
        # define the mortality rate here
        # initialize as a numeric with only 1 possible value
        M <- numeric(length = 1)
        
        # Mortality based on diameter class
        if (Diameter[i,j]>=0) {
          M<- rbinom(1,1,(pnew["a1"] +(pnew["b1"] *Diameter[i,j]*2.54)+pnew["b2"]*((Diameter[i,j]*2.54)^2)))
        }
        
        
        # Calculate the diameter for jth tree for the ith observed year
        Diameter[i,j]<-Diameter[i-1,j] + growth - M*(Diameter[i-1,j]+growth)
        
        # If the tree dies, plant a new tree (age = 0)
        if (M==1){
          age[i,j]<-0
        }
      }
    }
    
    
    # data will be saved as list 1
    mylist[[s]] <- df
    Diameter.all[[s]]<-Diameter[observed.a,]
  }
  
  J_new <- mapply(function(x, y){
    # calc <- (sort(x) - sort(y))^2/(2*(0.6*y)^2)
    calc <- (sort(x) - sort(y))^2/(2*(st_dev)^2) ## try 2 instead of 4 as SD (denominator)
    return(calc)
  }, Diameter.all, diameter.totals.end)
  
  J_new1 <-unlist(J_new)
  delta.J <- sum(J_new1) - J_last
  
  # print(sum(J_new1))
  if (min(1, exp(-delta.J)) > runif(1)) {
    p_op <- pnew
    J_last <- sum(J_new1)
    updated <- updated + 1
    J_keep[updated] <- sum(J_new1) 
    p_upgraded[,updated] <- p_op
    Diam_accepted<- Diameter.all
    # plot(prob_denom.keep)
    if (updated %in% c(100*1:100)) {
      par(mfrow=c(1,3))
      par(mar=c(2,3,2,3))
      for (par.no in 1:3) {
        hist(p_upgraded[par.no,(updated/2):updated],xlim = c(pmin[par.no],pmax[par.no]), main = par.name[par.no], xlab =NA, breaks=20)
        # abline(v=p_original[par.no], col="red", lwd=5)
        }
      
    }
  }
  
  if (simu == 2000) {
    covars <- cov(t(p_rec[,1:simu]))
  }
  if (simu > 2000) {
    covars <- sd*cov(t(p_rec[,1:simu]))
  }
  print(paste("simu =", simu, "updated =", updated))
}

# save.image(file ="C:/Users/al117862/Downloads/LLDiaCalibration2.17.21.Rdata")
# save.image(file ="C:/Users/al117862/Downloads/LLDiaCalibrationmortalitytest2.26.21.Rdata")
# save.image(file ="C:/Users/al117862/Downloads/LongleafCalibration3.15.21.Rdata")
# save.image(file ="~/GitHub/FL_Carbon/Longleaf Remeasurment/longleafCalibEstSigma.rdata")
save.image(file ="~/GitHub/FL_Carbon/Longleaf Remeasurment/longleafCalibMortOnly.rdata")

# dev.off()
# par(mfrow=c(1,1))
# 
# Diam_acc<-unlist(Diam_accepted)
# 
# observed.diam<-list()
# for (z in a){
# observed.diam[[z]]<-diameter.totals.end[[z]]
# }
# 
# observed<-unlist(observed.diam)
#  
# plot(Diam_acc, observed, xlab = "Accepted Diameters (in)", ylab = "Observed Diameters (in)")
# abline(0,1)

# generate final fig for parameter distribution (see line 250)

# sample from posterior parameter distributions to generate site-level samples for average diameter and C stock
## discard the first half of accepted parameters (sample from the second half)
# half.a1<-p_upgraded[1,(updated/2):updated]
# sample.a1
# sample.b1<-p_upgraded[2,(updated/2):updated]
# sample.b2<-p_upgraded[3,(updated/2):updated]
# sample.b4<-p_upgraded[4,(updated/2):updated]
# sample.b5<-p_upgraded[5,(updated/2):updated]
# sample.b6<-p_upgraded[6,(updated/2):updated]
# sample.b7<-p_upgraded[7,(updated/2):updated]
# sample.b8<-p_upgraded[8,(updated/2):updated]
# sample.b9<-p_upgraded[9,(updated/2):updated]
# sample.b10<-p_upgraded[10,(updated/2):updated]
# 
# 
# sample.par<-matrix(0, 10, 300)
# for (q in 1:10){
#   sample.par[q,]<-sample(p_upgraded[q,(updated/2):updated], 300)
# }
# hist(sample.par[1,])
# hist(sample.par[2,])
# 
# # save(sample.par, file="//net.ucf.edu/COS/Profiles/al117862/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/sample300parameters.rdata")
# 
## select 300 parameters (randomly)
## run the IBM model 300 times, calculate average DBH & C stock 300 times
## calculate site-mean DBH and SD and site-mean C stock and SD.

cols<-sample((updated/2):updated, 300)

sample.parameters<-p_upgraded[,c(cols)]
save(sample.parameters, file="//net.ucf.edu/COS/Profiles/al117862/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/sampleparametersMortOnly.rdata")
