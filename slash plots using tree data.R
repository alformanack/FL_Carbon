rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")

# Load dataset ------------------------------------------------------------

sitetree<-read.csv("aridity3.csv", header=T, sep=",", stringsAsFactors = F)

sitetree$SPCD<-as.factor(sitetree$SPCD)

# Calculate soil C:N for ORNL ---------------------------------------------

sitetree$ORNL_CN<-sitetree$SOC_ORNL_kgperm2*1000/sitetree$N_ORNL_gperm2

# Calculate weighted averages and soil C:N for Soilgrids data -------------

#replace zeros with NA
is.na(sitetree$SOC_0_5_1) <- !sitetree$SOC_0_5_1
is.na(sitetree$SOC_5_15_1) <- !sitetree$SOC_5_15_1
is.na(sitetree$Nitrogen_0_5_1) <- !sitetree$Nitrogen_0_5_1
is.na(sitetree$Nitrogen_5_15_1) <- !sitetree$Nitrogen_5_15_1

sitetree$SOILGRIDS_C_AVG<-(sitetree$SOC_0_5_1/10)*(1/3)+((sitetree$SOC_5_15_1/10)*(2/3))

sitetree$SOILGRIDS_N_AVG<-(sitetree$Nitrogen_0_5_1/100)*(1/3)+((sitetree$Nitrogen_5_15_1/100)*(2/3))                 

sitetree$SOILGRIDS_CN<-sitetree$SOILGRIDS_C_AVG/sitetree$SOILGRIDS_N_AVG 

sitetree[which(is.finite(sitetree$SOILGRIDS_CN)==F),"SOILGRIDS_CN"]<-NA

sitetree$FIA_CN_RATIO<-as.numeric(sitetree$FIA_CN_RATIO)

# Comparing soils data with FIA -------------------------------------------

plot(sitetree$ORNL_CN, sitetree$FIA_CN_RATIO)                 

plot(sitetree$SOILGRIDS_CN, sitetree$FIA_CN_RATIO)

model<-lm(FIA_CN_RATIO~SOILGRIDS_CN, data=sitetree)
summary(model)

#scale CN from FIA data
sitetree$SOILGRIDS_CN_SCALE<-(sitetree$SOILGRIDS_CN*4.941)+29.777

plot(sitetree$SOILGRIDS_CN_SCALE, sitetree$FIA_CN_RATIO)
model<-lm(FIA_CN_RATIO~SOILGRIDS_CN_SCALE, data=sitetree)
summary(model)

# normalizing predictor variables ---------------------------------------------

sitetree$logDIA<-log10(sitetree$DIA)

sitetree$logSOILGRIDS_CN_SCALE<-log10(sitetree$SOILGRIDS_CN_SCALE)

sitetree$logAGEDIA<-log10(sitetree$AGEDIA)

# removing aridity error
sitetree[which(sitetree$aridity_1==-2147483648),"aridity_1"]<-NA

sitetree$aridity_1<-sitetree$aridity_1*.0001

# Subsetting species ------------------------------------------------------

slashpine<-subset(sitetree, sitetree$SPCD=="111")

longleaf<-subset(sitetree, sitetree$SPCD=="121")

loblolly<-subset(sitetree, sitetree$SPCD=="131")

# creating models for slashpine
slashpine$temp2<-slashpine$AVG_TEMP_bioclim^2

slashpinemodel1<-lm(logDIA~AVG_TEMP_bioclim+temp2+logAGEDIA+logSOILGRIDS_CN_SCALE+slashpine$aridity_1, data=slashpine)

summary(slashpinemodel1)

slashpinemodel2<-lm(logDIA~AVG_TEMP_bioclim+temp2+aridity_1+logAGEDIA, data=slashpine)

summary(slashpinemodel2)

# looks like temperature and aridity are correlated
pairs(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+slashpine$aridity_1, data=slashpine)

vif(slashpinemodel2)

# creating models for longleaf pine
longleafmodel1<-lm(logDIA~AVG_TEMP_bioclim+logAGEDIA+logSOILGRIDS_CN_SCALE+aridity_1, data=longleaf)

summary(longleafmodel1)

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
slashgrowth<-(10^slash[1])*(10^(slash[2]*20))*(10^(slash[3]*425))*(slash[5]*slashpine$AGEDIA^-.6464)*(10^(slash[4]*.8753))

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


# Get plot data from FIA TREE file ----------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Desktop/FL")

tree<-read.csv("FL_TREE.csv", header=T, sep=",")

unique.plot<-unique(tree$PLT_CN)

plot_notslash<-unique(tree[tree$SPCD!=111,]$PLT_CN)

unique.111 <- setdiff(unique.plot, plot_notslash)

df <- tree[tree$PLT_CN %in% unique.111,]

plot<-read.csv("FL_PLOT.csv")

plot_id<-unique(df$PLT_CN)

for (i in 1:length(plot_id)){
  test<-subset(plot,CN==plot_id [i])
  rows<-row.names(df[which(df$PLT_CN==plot_id[i]),])
  print(i)
  df[rows,"LAT"]<-test$LAT
  df[rows,"LON"]<-test$LON
  }

plots<-list()

for (i in 1:length(unique.111)){
  plots[[i]]<-subset(df, PLT_CN==unique.111[i])
  }

# lapply(plots, `[`, c('LAT', 'LON'))
# lapply(plots, `[`, c('DESIGNCD'))
# 
# lapply(plots, function(i) sum(na.omit((i[["TPA_UNADJ"]]))))


j = 0
c<-c(1:3838)  
plots2<-matrix(0, ncol = 8, nrow = 749)
colnames(plots2)<-c("Tree_Density", "Maximum_DBH", "Average_DBH", "Observed_Biomass", "LAT", "LON", "PLT_CN",
                    "original_plot")
age<-vector()
TASB<-vector()
  
for (h in c) {
    df<-plots[[h]]
    df$sl <- h
    df2<-df[df$INVYR==min(df$INVYR) & df$STATUSCD=="1",]
    if (nrow(df2) > 20 & sd(df2$DIA)<2) {
      
      j = j+1
      
      # for (j in 1:3838) {
    df2$TPA_UNADJ <- ifelse(is.na(df2$TPA_UNADJ), df2$TPAGROW_UNADJ, df2$TPA_UNADJ)
    df2_sum<-sum(df2$TPA_UNADJ)
    plots2[j,3]<-sum(df2$DIA*df2$TPA_UNADJ)/df2_sum
    plots2[j,1]<-df2_sum*2.47 #trees per acre converted to trees per hectare
    plots2[j,2]<-max(df2$DIA) #find max diameter to predict age
    #age<-round((10^.40808)*(df2$DIA^1.06166))
    #TASB<-na.omit((0.0578*(df2$DIA^2.41)*(age^.204))*(round(df2$TPA_UNADJ)*2.47))
    TASB<-(0.041281*((df2$DIA*2.54)^2.722214))*(round(df2$TPA_UNADJ)*2.47)
    plots2[j,4]<-sum(TASB)*.5*1000*(1/10000)
    plots2[j,5]<-unique(df2$LAT)
    plots2[j,6]<-unique(df2$LON)
    plots2[j,7]<-unique(df2$PLT_CN)
    plots2[j,8]<-unique(df2$sl)
    # colnames(plots2)<-c("Tree Density", "Maximum DBH", "Average DBH", "Observed Biomass", "LAT", "LON", "PLT_CN")
    # }
  }
}

plots3<-as.data.frame(plots2)
ggplot(plots3, aes(x = LAT, y = LON, label=plots3$original_plot)) +
  geom_point() +geom_text(hjust = 0, nudge_x = 0.05, check_overlap = TRUE)

tail(plots2)
sum(plots[[14]][1:11,111])*2.47
hist(plots[[14]][1:11,18])
max(plots[[14]][1:11,18])
DIA<-as.vector(plots[[14]][1:11,18])
Trees<-as.vector(plots[[14]][1:11,111])
Bio<-0.041281*((DIA*2.54)^2.722214)
T_Bio<-(Bio*round(Trees))*2.47
Sum_T_BIO<-sum(T_Bio)*.5*1000*(1/10000)  

plots2[]

#write.csv(plots2, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/plots2.csv")
setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
plots2<-read.csv("plots2.csv", header=T, sep=",")

#producing histograms of diameter distributions by plot

par(mfrow=c(1,5))
c<-c(1, 2, 5:7, 9:20)
c<-c(2711,1869,2642,1766,1848,1027,196,2243,771,2328,170,2309,3105,2801,1984,2022,476,3614,1070,669)
c<-sort(c, decreasing = FALSE)
for (o in c){
  diameter.totals<-vector()
  df<-plots[[o]]
  df2<-df[df$INVYR==min(df$INVYR) & df$STATUSCD=="1",]
  df2$TPA_UNADJ <- ifelse(is.na(df2$TPA_UNADJ), df2$TPAGROW_UNADJ, df2$TPA_UNADJ)
    for (h in 1:length(df2$DIA))
    {diameter.totals<-rep((df2$DIA), round(df2$TPA_UNADJ*2.47))}
      hist(diameter.totals, main = o, xlab = "Diameter (in)")}

#trying to save info from histogram in table
par(mfrow=c(2,5))
c<-c(2711,1869,2642,1766,1848,1027,196,2243,771,2328,170,2309,3105,2801,1984,2022,476,3614,1070,669)
c<-sort(c, decreasing = FALSE)
observed.DBH<-list()
for (p in 1:length(c)){
  for (o in c){
  diameter.totals<-vector()
  df<-plots[[o]]
  df2<-df[df$INVYR==min(df$INVYR) & df$STATUSCD=="1",]
  df2$TPA_UNADJ <- ifelse(is.na(df2$TPA_UNADJ), df2$TPAGROW_UNADJ, df2$TPA_UNADJ)
    for (h in 1:length(df2$DIA))
    {diameter.totals<-rep((df2$DIA), round(df2$TPA_UNADJ*2.47))}
      hist(diameter.totals, main = o, xlab = "Diameter (in)")
  }
  observed.DBH[p]<-diameter.totals
}

df <- plots2[plots2[,8] %in% c,]

write.csv(df, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/slash_plots.csv")

plots[[20]]$INVYR
plots[[20]]$STATUSCD
plots[[20]]$TREE
plots[[20]]$DIA
plots[[10]]$TPA_UNADJ
plots[[10]]$TPAGROW_UNADJ
plots[[10]]$TPA_UNADJ

plots[[8]]$INVYR
plots[[8]]$STATUSCD
plots[[8]]$TREE
plots[[8]]$DIA

sum(plots[[1]][1:12,111])*2.47
hist(plots[[1]][1:12,18])
max(plots[[1]][1:12,18])

plots[[10]]$INVYR
plots[[10]]$STATUSCD
plots[[10]]$TREE
plots[[10]]$DIA
plots[[10]]$SUBP
plots[[10]]$TPA_UNADJ

sum(na.omit(plots[[10]][1:5,111]))*2.47
hist(plots[[10]][1:5,18])
max(plots[[10]][1:5,18])

plots[[11]]$INVYR
plots[[11]]$STATUSCD
plots[[11]]$TREE
plots[[11]]$SUBP

sum(plots[[11]][1:7,111])*2.47
hist(plots[[11]][1:7,18])
max(plots[[11]][1:7,18])

plots[[12]]$INVYR
plots[[12]]$STATUSCD
plots[[12]]$DIA[1:10]
plots[[12]]$TPAGROW_UNADJ

sum(plots[[12]][1:10,111])*2.47
sum(plots[[12]]$TPAGROW_UNADJ[11:17])*2.47
hist(plots[[12]][1:17,18])
max(plots[[12]][1:17,18])

plots[[13]]$INVYR
plots[[13]]$STATUSCD
plots[[13]]$TREE
plots[[13]]$SUBP

sum(plots[[13]][1:2,111])*2.47
hist(plots[[13]][1:2,18])
max(plots[[13]][1:2,18])

plots[[14]]$INVYR
plots[[14]]$STATUSCD
plots[[14]]$TREE
plots[[13]]$TPA_UNADJ



plots[[15]]$INVYR
plots[[15]]$STATUSCD
plots[[15]]$TREE
plots[[13]]$SUBP

sum(plots[[15]][which,'INVYR'=="2007" & 'STATUSCD'=="1"])*2.47
hist(plots[[14]][1:11,18])
max(plots[[14]][1:11,18])



# Run slash pine simulations ----------------------------------------------

rm(list=ls())

par(mfrow=c(2,8))
final_list<-list()

# Plot 1 ----------------------------------------------

# data will be saved as list 1
mylist<-list() 


# set parameters for growth equation
aridity<-6957*0.0001

temp<-22.34389687

temp2<-temp^2

C_AVG<-((725/10)*(1/3))+((760/10)*(2/3))

N_AVG<-((1375/100)*(1/3))+((923/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-379
observed.a<-round((10^.40808)*(8.9^1.06166))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[o,1]<-mean(Diameter[26,])
  predict.tasb[o,1]<-sum(TASB[26,])*.5*1000*(1/10000)
}
observed.d<-c(7.243498)
observed.tasb<-c(254.483096)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[26,], main = "1", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN", "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 1
mylist[[1]] <- df

# Plot 2 ----------------------------------------------

# set parameters for growth equation
aridity<-7977*0.0001

temp<-23.5516777

temp2<-temp^2

CN_SCALE<-NA

# set stand age and density
plot_density<-36
observed.a<-round((10^.40808)*(6.9^1.06166))
observed.tasb<-c(19.353489)
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[o,1]<-mean(Diameter[20,])
  predict.tasb[o,1]<-sum(TASB[20,])*.5*1000*(1/10000)
}
observed.d<-c(6.9)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[20,], main = "2", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN", "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 2
mylist[[2]] <- df

# Plot 5 ----------------------------------------------

# set parameters for growth equation
aridity<-8071*0.0001

temp<-24.04530907

temp2<-temp^2

CN_SCALE<-NA


# set stand age and density
plot_density<-61
observed.a<-round((10^.40808)*(13.1^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-c(140.333843)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[o,1]<-mean(Diameter[39,])
  predict.tasb[o,1]<-sum(TASB[39,])*.5*1000*(1/10000)
}
observed.d<-c(11.679545)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[39,], main = "5", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN", "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 3
mylist[[3]] <- df

# Plot 6 ----------------------------------------------

# set parameters for growth equation
aridity<-8077*0.0001

temp<-24.04530907

temp2<-temp^2

CN_SCALE<-NA

# set stand age and density
plot_density<-47
observed.a<-round((10^.40808)*(10.6^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-c(46.316479)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[o,1]<-mean(Diameter[31,])
  predict.tasb[o,1]<-sum(TASB[31,])*.5*1000*(1/10000)
}
observed.d<-c(8.367647)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[31,], main = "6", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN", "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 4
mylist[[4]] <- df

# Plot 7 ----------------------------------------------

# set parameters for growth equation
aridity<-8137*0.0001

temp<-24.09528542

temp2<-temp^2

CN_SCALE<-NA

# set stand age and density
plot_density<-47
observed.a<-round((10^.40808)*(6^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-c(18.142807)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[o,1]<-mean(Diameter[17,])
  predict.tasb[o,1]<-sum(TASB[17,])*.5*1000*(1/10000)
}
observed.d<-c(6)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[17,], main = "7", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN", "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 5
mylist[[5]] <- df

# Plot 9 ----------------------------------------------

# set parameters for growth equation
aridity<-8018*0.0001

temp<-24.09528542

temp2<-temp^2

CN_SCALE<-NA

# set stand age and density
plot_density<-88
observed.a<-round((10^.40808)*(6.7^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-36.715181

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[o,1]<-mean(Diameter[19,])
  predict.tasb[o,1]<-sum(TASB[19,])*.5*1000*(1/10000)
}
observed.d<-c(6.185714)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[19,], main = "9", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 6
mylist[[6]] <- df

# Plot 10 ----------------------------------------------

# set parameters for growth equation
aridity<-6920*0.0001

temp<-22.33102036

temp2<-temp^2

C_AVG<-((517/10)*(1/3))+((531/10)*(2/3))

N_AVG<-((1446/100)*(1/3))+((927/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-839
observed.a<-round((10^.40808)*(4.2^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-102.412670

for (h in 1:10){
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-mean(Diameter[12,])
  predict.tasb[h,1]<-sum(TASB[12,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(3.794279)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[12,], main = "10", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 7
mylist[[7]] <- df 

# Plot 11 ----------------------------------------------

# set parameters for growth equation
aridity<-6946*0.0001

temp<-22.33102036

temp2<-temp^2

C_AVG<-((476/10)*(1/3))+((431/10)*(2/3))

N_AVG<-((1186/100)*(1/3))+((747/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-1333
observed.a<-round((10^.40808)*(2.2^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-24.856105

for (h in 1:10){  
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-mean(Diameter[5,])
  predict.tasb[h,1]<-sum(TASB[5,])*.5*1000*(1/10000)
}  
# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(1.874043)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[5,], main = "10", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 8
mylist[[8]] <- df  

# Plot 12 ----------------------------------------------

# set parameters for growth equation
aridity<-6932*0.0001

temp<-22.33102036

temp2<-temp^2

C_AVG<-((366/10)*(1/3))+((427/10)*(2/3))

N_AVG<-((1137/100)*(1/3))+((628/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-3209
observed.a<-round((10^.40808)*(4.5^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-270.982210

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-mean(Diameter[13,])
  predict.tasb[h,1]<-sum(TASB[13,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(3.189155)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[13,], main = "12", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 9
mylist[[9]] <- df  

# Plot 13 ----------------------------------------------

# set parameters for growth equation
aridity<-6926*0.0001

temp<-22.47732353

temp2<-temp^2

C_AVG<-((680/10)*(1/3))+((563/10)*(2/3))

N_AVG<-((1508/100)*(1/3))+((936/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-296
observed.a<-round((10^.40808)*(2.5^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-9.486802

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-mean(Diameter[7,])
  predict.tasb[h,1]<-sum(TASB[7,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(2.3)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[7,], main = "13", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 10
mylist[[10]] <- df 

# Plot 14 ----------------------------------------------

# set parameters for growth equation
aridity<-7595*0.0001

temp<-20.97239685

temp2<-temp^2

C_AVG<-((475/10)*(1/3))+((227/10)*(2/3))

N_AVG<-((2085/100)*(1/3))+((1689/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-349
observed.a<-round((10^.40808)*(16.4^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-609.471481

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-mean(Diameter[50,])
  predict.tasb[h,1]<-sum(TASB[50,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(9.863562)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[50,], main = "14", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 11
mylist[[11]] <- df 

# Plot 15 ----------------------------------------------

# set parameters for growth equation
aridity<-6941*0.0001

temp<-22.33102036

temp2<-temp^2

C_AVG<-((486/10)*(1/3))+((475/10)*(2/3))

N_AVG<-((1238/100)*(1/3))+((827/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-585
observed.a<-round((10^.40808)*(11.1^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-661.599248

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-mean(Diameter[33,])
  predict.tasb[h,1]<-sum(TASB[33,])*.5*1000*(1/10000)
}  

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(8.878969)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[33,], main = "15", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 12
mylist[[12]] <- df

# Plot 16 ----------------------------------------------

# set parameters for growth equation
aridity<-8774*0.0001

temp<-19.62741661

temp2<-temp^2

C_AVG<-((972/10)*(1/3))+((295/10)*(2/3))

N_AVG<-((1585/100)*(1/3))+((505/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-793
observed.a<-round((10^.40808)*(7.2^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-126.913553

for (h in 1:10){    
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-mean(Diameter[21,])
  predict.tasb[h,1]<-sum(TASB[21,])*.5*1000*(1/10000)
}  

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(3.616565)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[21,], main = "16", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 13
mylist[[13]] <- df

# Plot 17 ----------------------------------------------

# set parameters for growth equation
aridity<-7687*0.0001

temp<-20.97239685

temp2<-temp^2

C_AVG<-((405/10)*(1/3))+((182/10)*(2/3))

N_AVG<-((2095/100)*(1/3))+((1702/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-253
observed.a<-round((10^.40808)*(17.5^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-357.394052

for (h in 1:10){  
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-mean(Diameter[53,])
  predict.tasb[h,1]<-sum(TASB[53,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(8.594118)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[53,], main = "17", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 14
mylist[[14]] <- df

# Plot 18 ----------------------------------------------

# set parameters for growth equation
aridity<-9677*0.0001

temp<-19.52181244

temp2<-temp^2

C_AVG<-((515/10)*(1/3))+((143/10)*(2/3))

N_AVG<-((1366/100)*(1/3))+((454/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-312
observed.a<-round((10^.40808)*(11.1^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-227.810385


for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-mean(Diameter[33,])
  predict.tasb[h,1]<-sum(TASB[33,])*.5*1000*(1/10000)
  
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(7.419048)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[33,], main = "18", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 15
mylist[[15]] <- df

# Plot 19 ----------------------------------------------

# set parameters for growth equation
aridity<-9796*0.0001

temp<-19.75616646

temp2<-temp^2

C_AVG<-((702/10)*(1/3))+((164/10)*(2/3))

N_AVG<-((1359/100)*(1/3))+((580/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-45
observed.a<-round((10^.40808)*(13.4^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-105.126587

for (h in 1:10){    
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-mean(Diameter[40,])
  predict.tasb[h,1]<-sum(TASB[40,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(11.866667)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[40,], main = "19", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 16
mylist[[16]] <- df

# Plot 20 ----------------------------------------------

# set parameters for growth equation
aridity<-9604*0.0001

temp<-19.6721344

temp2<-temp^2

C_AVG<-((592/10)*(1/3))+((194/10)*(2/3))

N_AVG<-((1335/100)*(1/3))+((571/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777

# set stand age and density
plot_density<-185
observed.a<-round((10^.40808)*(1.6^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-2.204984

for (h in 1:10){  
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*1^(-0.6464))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))
      
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
  predict.d[h,1]<-mean(Diameter[4,])
  predict.tasb[h,1]<-sum(TASB[4,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(1.6)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[4,], main = "20", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 12
mylist[[17]] <- df

final_list <- do.call(rbind.data.frame, mylist)
# Compare observed diameter vs modeled diameter ---------------------------
par(mfrow=c(1,1))
  
  model.1<-lm(data = final_list, log10(Observed_Diameter/Modeled_Diameter)~Age+Tree_Density+Temperature+Temperature2+Aridity)
  summary(model.1)
  # plot(Temperature~Aridity, data= final_list[[1]])
  # 
  # res.age<-residuals(model.1)
  # plot(res.age~df2$Age)
  # plot(res.age~df2$Aridity)
  
  model.2<-lm(data = final_list, log10(Observed_Diameter/Modeled_Diameter)~Soil_CN)
  summary(model.2)
  
  plot(data = final_list, Observed_Diameter~Modeled_Diameter, xlim = c(2,12), xlab="Modeled", ylab="Observed",
       col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")
  
  plot(data = final_list, Observed_Biomass~Modeled_Biomass, ylim = c(0,1000), xlab="Modeled", ylab="Observed", main ="Tree Carbon (gC/m^2)",
       col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF") 

# Correcting growth equation -------------------------------------------------

# before
growth<-(10^(-1.2975 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.3536*age[i,j]^(-0.6464))

#after
growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))

# Run slash pine simulations ----------------------------------------------

rm(list=ls())


final_list<-list()

# Plot 1 ----------------------------------------------

# data will be saved as list 1
mylist<-list() 
par(mfrow=c(2,8))


# set parameters for growth equation
aridity<-6957*0.0001

temp<-22.34389687

temp2<-temp^2

C_AVG<-((725/10)*(1/3))+((760/10)*(2/3))

N_AVG<-((1375/100)*(1/3))+((923/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-379
observed.a<-round((10^.40808)*(8.9^1.06166))
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[o,1]<-mean(Diameter[26,])
  predict.tasb[o,1]<-sum(TASB[26,])*.5*1000*(1/10000)
}
observed.d<-c(7.243498)
observed.tasb<-c(254.483096)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[26,], main = "1", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN", "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 1
mylist[[1]] <- df

# Plot 2 ----------------------------------------------

# set parameters for growth equation
aridity<-7977*0.0001

temp<-23.5516777

temp2<-temp^2

CN_SCALE<-NA

# set stand age and density
plot_density<-36
observed.a<-round((10^.40808)*(6.9^1.06166))
observed.tasb<-c(19.353489)
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[o,1]<-mean(Diameter[20,])
  predict.tasb[o,1]<-sum(TASB[20,])*.5*1000*(1/10000)
}
observed.d<-c(6.9)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[20,], main = "2", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN", "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 2
mylist[[2]] <- df

# Plot 5 ----------------------------------------------

# set parameters for growth equation
aridity<-8071*0.0001

temp<-24.04530907

temp2<-temp^2

CN_SCALE<-NA


# set stand age and density
plot_density<-61
observed.a<-round((10^.40808)*(13.1^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-c(140.333843)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[o,1]<-mean(Diameter[39,])
  predict.tasb[o,1]<-sum(TASB[39,])*.5*1000*(1/10000)
}
observed.d<-c(11.679545)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[39,], main = "5", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN", "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 3
mylist[[3]] <- df

# Plot 6 ----------------------------------------------

# set parameters for growth equation
aridity<-8077*0.0001

temp<-24.04530907

temp2<-temp^2

CN_SCALE<-NA

# set stand age and density
plot_density<-47
observed.a<-round((10^.40808)*(10.6^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-c(46.316479)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[o,1]<-mean(Diameter[31,])
  predict.tasb[o,1]<-sum(TASB[31,])*.5*1000*(1/10000)
}
observed.d<-c(8.367647)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[31,], main = "6", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN", "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 4
mylist[[4]] <- df

# Plot 7 ----------------------------------------------

# set parameters for growth equation
aridity<-8137*0.0001

temp<-24.09528542

temp2<-temp^2

CN_SCALE<-NA

# set stand age and density
plot_density<-47
observed.a<-round((10^.40808)*(6^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-c(18.142807)

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[o,1]<-mean(Diameter[17,])
  predict.tasb[o,1]<-sum(TASB[17,])*.5*1000*(1/10000)
}
observed.d<-c(6)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[17,], main = "7", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN", "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 5
mylist[[5]] <- df

# Plot 9 ----------------------------------------------

# set parameters for growth equation
aridity<-8018*0.0001

temp<-24.09528542

temp2<-temp^2

CN_SCALE<-NA

# set stand age and density
plot_density<-88
observed.a<-round((10^.40808)*(6.7^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-36.715181

for (o in 1:10){
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[o,1]<-mean(Diameter[19,])
  predict.tasb[o,1]<-sum(TASB[19,])*.5*1000*(1/10000)
}
observed.d<-c(6.185714)
modeled.d<-mean(predict.d)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[19,], main = "9", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 6
mylist[[6]] <- df

# Plot 10 ----------------------------------------------

# set parameters for growth equation
aridity<-6920*0.0001

temp<-22.33102036

temp2<-temp^2

C_AVG<-((517/10)*(1/3))+((531/10)*(2/3))

N_AVG<-((1446/100)*(1/3))+((927/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-839
observed.a<-round((10^.40808)*(4.2^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-102.412670

for (h in 1:10){
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[h,1]<-mean(Diameter[12,])
  predict.tasb[h,1]<-sum(TASB[12,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(3.794279)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[12,], main = "10", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 7
mylist[[7]] <- df 

# Plot 11 ----------------------------------------------

# set parameters for growth equation
aridity<-6946*0.0001

temp<-22.33102036

temp2<-temp^2

C_AVG<-((476/10)*(1/3))+((431/10)*(2/3))

N_AVG<-((1186/100)*(1/3))+((747/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-1333
observed.a<-round((10^.40808)*(2.2^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-24.856105

for (h in 1:10){  
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[h,1]<-mean(Diameter[5,])
  predict.tasb[h,1]<-sum(TASB[5,])*.5*1000*(1/10000)
}  
# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(1.874043)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[5,], main = "11", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 8
mylist[[8]] <- df  

# Plot 12 ----------------------------------------------

# set parameters for growth equation
aridity<-6932*0.0001

temp<-22.33102036

temp2<-temp^2

C_AVG<-((366/10)*(1/3))+((427/10)*(2/3))

N_AVG<-((1137/100)*(1/3))+((628/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-3209
observed.a<-round((10^.40808)*(4.5^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-270.982210

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[h,1]<-mean(Diameter[13,])
  predict.tasb[h,1]<-sum(TASB[13,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(3.189155)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[13,], main = "12", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 9
mylist[[9]] <- df  

# Plot 13 ----------------------------------------------

# set parameters for growth equation
aridity<-6926*0.0001

temp<-22.47732353

temp2<-temp^2

C_AVG<-((680/10)*(1/3))+((563/10)*(2/3))

N_AVG<-((1508/100)*(1/3))+((936/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-296
observed.a<-round((10^.40808)*(2.5^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-9.486802

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[h,1]<-mean(Diameter[7,])
  predict.tasb[h,1]<-sum(TASB[7,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(2.3)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[7,], main = "13", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 10
mylist[[10]] <- df 

# Plot 14 ----------------------------------------------

# set parameters for growth equation
aridity<-7595*0.0001

temp<-20.97239685

temp2<-temp^2

C_AVG<-((475/10)*(1/3))+((227/10)*(2/3))

N_AVG<-((2085/100)*(1/3))+((1689/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-349
observed.a<-round((10^.40808)*(16.4^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-609.471481

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[h,1]<-mean(Diameter[50,])
  predict.tasb[h,1]<-sum(TASB[50,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(9.863562)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[50,], main = "14", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 11
mylist[[11]] <- df 

# Plot 15 ----------------------------------------------

# set parameters for growth equation
aridity<-6941*0.0001

temp<-22.33102036

temp2<-temp^2

C_AVG<-((486/10)*(1/3))+((475/10)*(2/3))

N_AVG<-((1238/100)*(1/3))+((827/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-585
observed.a<-round((10^.40808)*(11.1^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-661.599248

for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[h,1]<-mean(Diameter[33,])
  predict.tasb[h,1]<-sum(TASB[33,])*.5*1000*(1/10000)
}  

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(8.878969)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[33,], main = "15", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 12
mylist[[12]] <- df

# Plot 16 ----------------------------------------------

# set parameters for growth equation
aridity<-8774*0.0001

temp<-19.62741661

temp2<-temp^2

C_AVG<-((972/10)*(1/3))+((295/10)*(2/3))

N_AVG<-((1585/100)*(1/3))+((505/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-793
observed.a<-round((10^.40808)*(7.2^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-126.913553

for (h in 1:10){    
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[h,1]<-mean(Diameter[21,])
  predict.tasb[h,1]<-sum(TASB[21,])*.5*1000*(1/10000)
}  

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(3.616565)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[21,], main = "16", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 13
mylist[[13]] <- df

# Plot 17 ----------------------------------------------

# set parameters for growth equation
aridity<-7687*0.0001

temp<-20.97239685

temp2<-temp^2

C_AVG<-((405/10)*(1/3))+((182/10)*(2/3))

N_AVG<-((2095/100)*(1/3))+((1702/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-253
observed.a<-round((10^.40808)*(17.5^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-357.394052

for (h in 1:10){  
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[h,1]<-mean(Diameter[53,])
  predict.tasb[h,1]<-sum(TASB[53,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(8.594118)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[53,], main = "17", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 14
mylist[[14]] <- df

# Plot 18 ----------------------------------------------

# set parameters for growth equation
aridity<-9677*0.0001

temp<-19.52181244

temp2<-temp^2

C_AVG<-((515/10)*(1/3))+((143/10)*(2/3))

N_AVG<-((1366/100)*(1/3))+((454/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-312
observed.a<-round((10^.40808)*(11.1^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.d<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-227.810385


for (h in 1:10){   
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[h,1]<-mean(Diameter[33,])
  predict.tasb[h,1]<-sum(TASB[33,])*.5*1000*(1/10000)
  
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(7.419048)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[33,], main = "18", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 15
mylist[[15]] <- df

# Plot 19 ----------------------------------------------

# set parameters for growth equation
aridity<-9796*0.0001

temp<-19.75616646

temp2<-temp^2

C_AVG<-((702/10)*(1/3))+((164/10)*(2/3))

N_AVG<-((1359/100)*(1/3))+((580/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777


# set stand age and density
plot_density<-45
observed.a<-round((10^.40808)*(13.4^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-105.126587

for (h in 1:10){    
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[h,1]<-mean(Diameter[40,])
  predict.tasb[h,1]<-sum(TASB[40,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(11.866667)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[40,], main = "19", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 16
mylist[[16]] <- df

# Plot 20 ----------------------------------------------

# set parameters for growth equation
aridity<-9604*0.0001

temp<-19.6721344

temp2<-temp^2

C_AVG<-((592/10)*(1/3))+((194/10)*(2/3))

N_AVG<-((1335/100)*(1/3))+((571/100)*(2/3))

CN<-C_AVG/N_AVG

CN_SCALE<-(CN*4.941)+29.777

# set stand age and density
plot_density<-185
observed.a<-round((10^.40808)*(1.6^1.06166))
predict.d<-matrix(nrow = 10, ncol = 1,0)
predict.tasb<-matrix(nrow = 10, ncol = 1,0)
observed.tasb<-2.204984

for (h in 1:10){  
  
  age<-matrix(0, nrow=observed.a[1], ncol=plot_density[1]) # initialize the age matrix
  Diameter<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the diameter matrix
  TASB<-matrix(0, nrow=observed.a[1], plot_density[1]) # initialize the total above-stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*1^(-0.0523))
  
  for (j in 1:plot_density){ # specify tree per hectare
    
    for (i in 2:observed.a){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.0202 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.9477*age[i,j]^(-0.0523))
      
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
  predict.d[h,1]<-mean(Diameter[4,])
  predict.tasb[h,1]<-sum(TASB[4,])*.5*1000*(1/10000)
}

# save average modeled diameter
modeled.d<-mean(predict.d)
observed.d<-c(1.6)
modeled.tasb<-mean(predict.tasb)
hist(Diameter[4,], main = "20", xlab = "Diameter (in)")

# set up dataframe to store simulated data
df<-cbind(observed.d, modeled.d, observed.a, plot_density, temp, temp2, aridity, CN_SCALE, observed.tasb, modeled.tasb)

df<-data.frame(df)

colnames(df)<-c("Observed_Diameter","Modeled_Diameter","Age", "Tree_Density", "Temperature", "Temperature2", "Aridity", "Soil_CN",
                "Observed_Biomass", "Modeled_Biomass")

# data will be saved as list 12
mylist[[17]] <- df

final_list <- do.call(rbind.data.frame, mylist)
# Compare observed diameter vs modeled diameter ---------------------------
par(mfrow=c(1,1))

model.1<-lm(data = final_list, log10(Observed_Diameter/Modeled_Diameter)~Age+Tree_Density+Temperature+Temperature2+Aridity)
summary(model.1)
# plot(Temperature~Aridity, data= final_list[[1]])
# 
# res.age<-residuals(model.1)
# plot(res.age~df2$Age)
# plot(res.age~df2$Aridity)

model.2<-lm(data = final_list, log10(Observed_Diameter/Modeled_Diameter)~log10(Age))
summary(model.2)

plot(data = final_list, Observed_Diameter~Modeled_Diameter, xlim = c(2,12), xlab="Modeled", ylab="Observed",
     col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")

plot(data = final_list, Observed_Biomass~Modeled_Biomass, xlim = c(0,1200), ylim = c(0,1200), xlab="Modeled", ylab="Observed", main ="Tree Carbon (gC/m^2)",
     col.axis="#027368", col="#75BFBF", pch=16, type="p") + abline(0,1, col="#048ABF")


# Calculating total above stump C biomass for year 2100 for 700 tph by region

Panhandle<-vector()
North<-vector()
Central<-vector()
South<-vector()

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
  
for (b in 1:10){  
  age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
  Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
  TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.02279 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.95169*1^(-0.04831))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:80){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.02279 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.95169*age[i,j]^(-0.04831))
      
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
}
  
  hist(Diameter[80,]*2.54, main = "Panhandle", xlab = "Diameter (cm)")
  
  # Calculating North TASB --------------------------------------------------
  
  temp<-20.30
  
  temp2<-temp^2
  
  aridity<-8212*.0001
  
  # CN scaled with FIA data
  C_AVG<-((317/10)*(1/3))+((110/10)*(2/3))
  
  N_AVG<-((1665/100)*(1/3))+((300/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  for (b in 1:10){  
  age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
  Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
  TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.02279 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.95169*1^(-0.04831))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:80){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.02279 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.95169*age[i,j]^(-0.04831))
      
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
  }
  
  hist(Diameter[80,]*2.54, main = "North", xlab = "Diameter (cm)")
  
  # TASB Central ------------------------------------------------------------
  
  temp<-22.29
  
  temp2<-temp^2
  
  aridity<-7259*.0001
  
  # CN scaled with FIA data
  C_AVG<-((475/10)*(1/3))+((144/10)*(2/3))
  
  N_AVG<-((1729/100)*(1/3))+((1137/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  for (b in 1:10){  
  
  age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
  Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
  TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.02279 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.95169*1^(-0.04831))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:80){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.02279 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.95169*age[i,j]^(-0.04831))
      
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
  }
  
  hist(Diameter[80,]*2.54, main = "Central", xlab = "Diameter (cm)")
  
  # South TASB --------------------------------------------------------------
  
  temp<-23.80
  
  temp2<-temp^2
  
  aridity<-7305*.0001
  
  # CN scaled with FIA data
  C_AVG<-((1052/10)*(1/3))+((1017/10)*(2/3))
  
  N_AVG<-((1721/100)*(1/3))+((1289/100)*(2/3))
  
  CN<-C_AVG/N_AVG
  
  CN_SCALE<-(CN*4.941)+29.777
  
  for (b in 1:10){  
  
  age<-matrix(0, nrow=80, ncol=700) # initialize the age matrix
  Diameter<-matrix(0, nrow=80, ncol=700) # initialize the diameter matrix
  TASB<-matrix(0, nrow=80, ncol=700) # initialize the total above stump biomass matrix
  
  # initialize the diameter for the first year
  Diameter[1,]<-(10^(-2.02279 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.95169*1^(-0.04831))
  
  
  for (j in 1:700){ # specify tree per hectare
    
    
    
    for (i in 2:80){ # specify how long to run the simulation (years)
      
      age[i,j]<-age[i-1,j]+1
      
      growth<-(10^(-2.02279 + 0.1872*temp - 0.0044*temp2 - 0.2243*aridity))*(0.95169*age[i,j]^(-0.04831))
      
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
  }
  
  hist(Diameter[80,]*2.54, main = "South", xlab = "Diameter (cm)")
  


TASB_Panhandle<-c(mean(Panhandle), sd(Panhandle))
TASB_North<-c(mean(North), sd(North))
TASB_Central<-c(mean(Central), sd(Central))
TASB_South<-c(mean(South), sd(South))
table<-rbind(TASB_Panhandle, TASB_North, TASB_Central, TASB_South)


