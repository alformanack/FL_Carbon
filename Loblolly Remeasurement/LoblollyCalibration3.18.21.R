rm(list=ls())
## Changing directory 

# Assuming the first lib path is the problematic one:
# path <- "U:\Documents\R"
# # Map to drive letter Z
# system2("net", args=c("use", "Z:", path))
# Update so that packages are installed to that directory
.libPaths("Z:/")

# Install packages
install.packages("ggplot2")
install.packages("mvnfast")
lapply(c("ggplot2","mvnfast"), require, character.only = T)

setwd("~/GitHub/FL_Carbon/Loblolly Remeasurement")
envdata<-read.csv("LoblollyEnvData.csv", header=T, sep=",") 
load("LoblollyAgeTotals.rdata")
load("LoblollyAgeTotalsEnd.rdata")
load("LoblollyDIATotalsEnd.rdata")
load("LoblollyDIATotals.rdata")
load("LoblollyPlotStart.rdata")
load("LoblollyPlotEnd.rdata")





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
    pnew <- MASS::mvrnorm(1, mu = p_op, Sigma = covars)
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
#   a1             b1              b2               b3           b4               b5            b6                b7                b8          sigma  
pmin[1] <- 5.0;pmin[2] <-  0.00;pmin[3] <-  0.00;pmin[4] <- 6;pmin[5] <-  0.4;pmin[6] <- -1.0;pmin[7] <- 0.0; pmin[8] <- -.05;pmin[9] <- 0.0; pmin[10]<-1
pmax[1] <- 9.0;pmax[2] <-  0.6;pmax[3] <-  0.8;pmax[4] <- 12;pmax[5] <-  0.9;pmax[6] <- 0.0;pmax[7] <- 0.5;   pmax[8] <- 0.0; pmax[9] <- 0.0005;pmax[10]<-10


par.name <- c("a1","b1","b2","b3","b4","b5", "b6", "b7", "b8","sigma")
p_op <- c(7.588418, 0.37183, 0.45382, 9.20441, 0.7405648, -0.2594352,1.329e-01, -1.075e-02,1.401e-04, 4)

names(p_op)<-par.name
no.simu <- 500000   #10000
d <- 10
a<-c(8, 10,  2,  6,  7)
st_dev<-p_op[10]

for (s in a){
  
  
  plot_density<-diameter.totals[[s]]
  observed.a<-envdata[s,6]
  CN <- envdata[s,16]
  aridity <- envdata[s,17]
  temp <- envdata[s,7]
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
      
      
      
      growth<-(10^(p_op["a1"] - p_op["b1"]*temp + p_op["b2"]*temp*aridity - p_op["b3"]*aridity))*(p_op["b4"]*age[i,j]^(p_op["b5"]))
      
      # define the mortality rate here
      # initialize as a numeric with only 1 possible value
      M <- numeric(length = 1)
      
      # Mortality based on diameter class
      if (Diameter[i,j]>=0) {
        # M<- rbinom(1,1,(exp(p_op["b7"] + p_op["b8"]*Diameter[i,j]*2.54)))
        M<- rbinom(1,1,(p_op["b6"] + (p_op["b7"]*Diameter[i,j]*2.54) + p_op["b8"]*((Diameter[i,j]*2.54)^2)))
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
Diam_first<-Diameter.all
J_old <- mapply(function(x, y){
  # calc <- (sort(x) - sort(y))^2/(2*(0.6*y)^2)
  calc <- (sort(x) - sort(y))^2/(2*(st_dev)^2) ## try 2 instead of 4 as SD (denominator)
  # error<-(sort(x) - sort(y))^2/(2*(st_dev)^2)
  # calc <- -0.5*length(error)*log(3.14*2)-0.5*log(st_dev^2)-error
  # calc <- (sort(x) - sort(y))^2
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
  
  
  
  #assign pars
  for (b in 1:length(par.name)) {
    assign(par.name[b], pnew[b])
  }
  
  names(pnew)<-par.name
  st_dev<-pnew[10]
  
  for (s in a){
    
    # set stand age and density
    plot_density<-diameter.totals[[s]]
    observed.a<-envdata[s,6]
    CN <- envdata[s,16]
    aridity <- envdata[s,17]
    temp <- envdata[s,7]
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
        
        growth<-(10^(pnew["a1"] - pnew["b1"]*temp + pnew["b2"]*temp*aridity - pnew["b3"]*aridity))*(pnew["b4"]*age[i,j]^(pnew["b5"]))
        
        
        # define the mortality rate here
        # initialize as a numeric with only 1 possible value
        M <- numeric(length = 1)
        
        # Mortality based on diameter class
        if (Diameter[i,j]>=0) {
          M<- rbinom(1,1,(pnew["b6"] + (pnew["b7"]*Diameter[i,j]*2.54) + pnew["b8"]*((Diameter[i,j]*2.54)^2)))
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
    # error<-(sort(x) - sort(y))^2/(2*(st_dev)^2)
    # calc <- -0.5*length(error)*log(3.14*2)-0.5*log(st_dev^2)-error
    # calc <- (sort(x) - sort(y))^2
    return(calc)
  }, Diameter.all, diameter.totals.end)
  
  J_new1 <-unlist(J_new)
  delta.J <- sum(J_new1) - J_last
  
  # print(sum(J_new1))
  if (min(1, exp(-delta.J)) > runif(1)) {
    p_op<-pnew
    J_last <- sum(J_new1)
    updated <- updated + 1
    J_keep[updated] <- sum(J_new1) 
    p_upgraded[,updated] <- p_op
    Diam_accepted<- Diameter.all
    # plot(prob_denom.keep)
    if (updated %in% c(100*1:100)) {
      par(mfrow=c(2,5))
      par(mar=c(2,3,1,1))
      for (par.no in 1:10) {
        hist(p_upgraded[par.no,(updated/2):updated],xlim = c(pmin[par.no],pmax[par.no]), main = par.name[par.no], xlab =NA, breaks=20)
        # plot(p_upgraded[par.no,1:updated], ylim = c(pmin[par.no],pmax[par.no]),main = par.name[par.no])
      }
      
    }
  }
  p_rec[,simu] <- p_op#save saved parameters to generate Covar matrix later
  if (simu == 2000) {
    covars <- cov(t(p_rec[,1:simu]))
  }
  if (simu > 2000) {
    covars <- sd*cov(t(p_rec[,1:simu]))
  }
  
  print(paste("simu =", simu, "updated =", updated))
}

save.image(file ="//net.ucf.edu/COS/Profiles/al117862/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/LoblollyCalibration3.18.21.Rdata")
# 
# dev.off()
# par(mfrow=c(1,1))
# 
# Diam_acc<-unlist(Diam_accepted)
# 
# observed.diam<-list()
# for (z in a){
#   observed.diam[[z]]<-diameter.totals.end[[z]]
# }
# 
# observed<-unlist(observed.diam)
# 
# plot(Diam_acc, observed, xlab = "Accepted Diameters (in)", ylab = "Observed Diameters (in)")
# abline(0,1)
# 
# # generate final fig for parameter distribution (see line 250)
# 
# # sample from posterior parameter distributions to generate site-level samples for average diameter and C stock
# ## discard the first half of accepted parameters (sample from the second half)
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
# save(sample.par, file="//net.ucf.edu/COS/Profiles/al117862/Documents/GitHub/FL_Carbon/Longleaf Remeasurment/sample300parameters.rdata")
# 
# ## select 300 parameters (randomly)
# ## run the IBM model 300 times, calculate average DBH & C stock 300 times
# ## calculate site-mean DBH and SD and site-mean C stock and SD. 
# 
cols<-sample((updated/2):updated, 300)

sample.parameters<-p_upgraded[,c(cols)]
save(sample.parameters, file="//net.ucf.edu/COS/Profiles/al117862/Documents/GitHub/FL_Carbon/Loblolly Remeasurement/LoblollyParameters3.18.21.Rdata")
# 