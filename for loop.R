rm(list=ls())

setwd("C:/Users/john/Desktop/FL")
sitetree<-read.csv("FL_SITETREE.csv")
plot<-read.csv("FL_PLOT.csv")


  
 plot_id<-unique(sitetree$PLT_CN)
 print(plot_id)

for (i in 1:length(plot_id))
{test<-subset(plot,CN==plot_id [i])
rows<-row.names(sitetree[which(sitetree$PLT_CN==plot_id[i]),])
sitetree[rows,"LAT"]<-test$LAT
sitetree[rows,"LON"]<-test$LON}
 
 unique(sitetree$PLT_CN)
 
write.csv(sitetree,"new_FL_SITETREE.csv")


