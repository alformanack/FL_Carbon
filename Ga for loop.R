setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
sitetree<-read.csv("GA_SITETREE.csv")
plot<-read.csv("GA_PLOT.csv")

plot_id<-unique(sitetree$PLT_CN)


for (i in 1:length(plot_id))
{test<-subset(plot,CN==plot_id [i])
rows<-row.names(sitetree[which(sitetree$PLT_CN==plot_id[i]),])
sitetree[rows,"LAT"]<-test$LAT
sitetree[rows,"LON"]<-test$LON}

write.csv(sitetree,"new_GA_SITETREE.csv")

GA_slash<-subset(sitetree, sitetree$SPCD=="111")
write.csv(GA_slash,"GA_slash.csv")

GA_SLASH<-read.csv("GA_SLASH.csv")
mean(GA_SLASH$rvalue_1)
