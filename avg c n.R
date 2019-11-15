#setwd("C:/Users/Alicia/Desktop/FL")

sitetreetemp<-read.csv("temperature_new_FL_SITETREE.csv", header=T, sep=",", stringsAsFactors = F)
soils<-read.csv("FL_SOILS_LAB.csv", header=T, sep=",", stringsAsFactors = F)




plot_id<-unique(sitetreetemp$PLT_CN)


for (i in 1:length(plot_id)){
  test<-subset(soils,PLT_CN==plot_id [i])
  layer1<-subset(test, LAYER_TYPE=="MIN_1")
  layer2<-subset(test, LAYER_TYPE=="MIN_2")
  Carbonavg<-(layer1$C_ORG_PCT+layer2$C_ORG_PCT)/2
  rows<-row.names(sitetreetemp[which(sitetreetemp$PLT_CN==plot_id[i]),])
  if(length(Carbonavg)>0){
    sitetreetemp[rows,"avg_soil_c_pct"]<-Carbonavg
  }
}


for (i in 1:length(plot_id)){
  test<-subset(soils,PLT_CN==plot_id [i])
  layer1<-subset(test, LAYER_TYPE=="MIN_1")
  layer2<-subset(test, LAYER_TYPE=="MIN_2")
  Nitrogenavg<-(layer1$N_TOTAL_PCT+layer2$N_TOTAL_PCT)/2
  rows<-row.names(sitetreetemp[which(sitetreetemp$PLT_CN==plot_id[i]),])
  if(length(Nitrogenavg)>0){
    sitetreetemp[rows,"avg_soil_n_pct"]<-Nitrogenavg
  }
}

df<-read.csv("CompleteSoilSitetree.csv")
