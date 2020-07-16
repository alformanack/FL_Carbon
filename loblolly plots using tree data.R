# Get plot data from FIA TREE file ----------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Desktop/FL")

tree<-read.csv("FL_TREE.csv", header=T, sep=",")

unique.plot<-unique(tree$PLT_CN)

plot_notlob<-unique(tree[tree$SPCD!=131,]$PLT_CN)

unique.131 <- setdiff(unique.plot, plot_notlob)

df <- tree[tree$PLT_CN %in% unique.131,]

plot<-read.csv("FL_PLOT.csv")

plot_id<-unique(df$PLT_CN)

for (i in 1:length(plot_id)){
  test<-subset(plot,CN==plot_id [i])
  rows<-row.names(df[which(df$PLT_CN==plot_id[i]),])
  print(i)
  df[rows,"LAT"]<-test$LAT
  df[rows,"LON"]<-test$LON
}

ggplot(df, aes(x = LAT, y = LON)) +
  geom_point() +geom_text(hjust = 0, nudge_x = 0.05, check_overlap = TRUE)

plots<-list()

for (i in 1:length(unique.131)){
  plots[[i]]<-subset(df, PLT_CN==unique.131[i])
}

# lapply(plots, `[`, c('LAT', 'LON'))
# lapply(plots, `[`, c('DESIGNCD'))
# 
# lapply(plots, function(i) sum(na.omit((i[["TPA_UNADJ"]]))))


j = 0
c<-c(1:228)  
plots2<-matrix(0, ncol = 8, nrow = 206)
colnames(plots2)<-c("Tree_Density", "Maximum_DBH", "Average_DBH", "Observed_Biomass", "LAT", "LON", "PLT_CN",
                    "original_plot")
age<-vector()
TASB<-vector()

for (h in c) {
  df<-plots[[h]]
  df$sl <- h
  df2<-df[df$INVYR==min(df$INVYR) & df$STATUSCD=="1",]
  if (nrow(df2) > 0) {
    
    j = j+1
    
    # for (j in 1:3838) {
    df2$TPA_UNADJ <- ifelse(is.na(df2$TPA_UNADJ), df2$TPAGROW_UNADJ, df2$TPA_UNADJ)
    df2_sum<-sum(df2$TPA_UNADJ)
    plots2[j,3]<-sum(df2$DIA*df2$TPA_UNADJ)/df2_sum
    plots2[j,1]<-df2_sum*2.47 #trees per acre converted to trees per hectare
    plots2[j,2]<-max(df2$DIA) #find max diameter to predict age
    #age<-round((10^.40808)*(df2$DIA^1.06166))
    #TASB<-na.omit((0.0578*(df2$DIA^2.41)*(age^.204))*(round(df2$TPA_UNADJ)*2.47))
    TASB<-(0.037403*((df2$DIA*2.54)^2.676835))*(round(df2$TPA_UNADJ)*2.47)
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
plots3<-plots3[order(plots3$LON),]
ggplot(plots3, aes(x = LON, y = LAT, label=plots3$original_plot)) +
  geom_point() +geom_text(hjust = 0, nudge_x = 0.05, check_overlap = TRUE)


plots2[]

#write.csv(plots2, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/plots2.csv")
setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
plots2<-read.csv("plots2.csv", header=T, sep=",")

#producing histograms of diameter distributions by plot

par(mfrow=c(1,5))


c<-c(18,29,30,36,75,83,91,122,130,133,147,153,156,159,182,185,195,210,226,228)

for (o in c){
  diameter.totals<-vector()
  df<-plots[[o]]
  df2<-df[df$INVYR==min(df$INVYR) & df$STATUSCD=="1",]
  df2$TPA_UNADJ <- ifelse(is.na(df2$TPA_UNADJ), df2$TPAGROW_UNADJ, df2$TPA_UNADJ)
  for (h in 1:length(df2$DIA))
  {diameter.totals<-rep((df2$DIA), round(df2$TPA_UNADJ*2.47))}
  hist(diameter.totals, main = o, xlab = "Diameter (in)")}

df <- plots2[plots2[,8] %in% c,]

write.csv(df, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/loblolly_plots.csv")
