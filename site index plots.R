# write.csv(df, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/slash_tree_lat_lon.csv")
setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
df<-read.csv("slash_tree_lat_lon.csv", header=T, sep=",")

setwd("C:/Users/Alicia/Desktop/FL")
cond<-read.csv("FL_COND.csv")
df1<-subset(cond, cond$SISP==111)
df2<-subset(df1, select=c("PLT_CN", "INVYR", "SICOND", "SISP"))
# joined_df<-dplyr::left_join(df2, df, by = c("PLT_CN" = "PLT_CN"))
joined_df<-merge(df2, df, by = c("PLT_CN" = "PLT_CN"))
plots<-list()
unique.plt<-unique(joined_df$PLT_CN)
for (i in 1:length(unique.plt)){
  plots[[i]]<-subset(joined_df, PLT_CN==unique.plt[i])
}
View(plots)
j= 0
c<-c(1:417)
plots2<-matrix(0, ncol = 8, nrow = 228)
colnames(plots2)<-c("Tree_Density", "Maximum_DBH", "Average_DBH", "Observed_Biomass", "LAT", "LON", "PLT_CN",
                    "original_plot")
TASB<-vector()
for (h in c) {
  df<-plots[[h]]
  df$sl <- h
  df2<-df[df$INVYR.y==max(df$INVYR.y) & df$STATUSCD=="1",]
  if (nrow(df2) > 20) {
    j = j+1
    # for (j in 1:3838) {
    df2$TPA_UNADJ <- ifelse(is.na(df2$TPA_UNADJ), df2$TPAGROW_UNADJ, df2$TPA_UNADJ)
    df2_sum<-sum(df2$TPA_UNADJ)
    plots2[j,3]<-sum(df2$DIA*df2$TPA_UNADJ)/df2_sum
    plots2[j,1]<-df2_sum*2.47 #trees per acre converted to trees per hectare
    plots2[j,2]<-max(df2$DIA) #find max diameter to predict age
    #age<-round((10^.40808)*(df2$DIA^1.06166))
    #TASB<-na.omit((0.0578*(df2$DIA^2.41)*(age^.204))*(round(df2$TPA_UNADJ)*2.47))
    TASB<-(0.041281*((df2$DIA*2.54)^2.722214))*(round(df2$TPA_UNADJ*2.47))
    plots2[j,4]<-sum(TASB)*.5*1000*(1/10000)
    plots2[j,5]<-unique(df2$LAT)
    plots2[j,6]<-unique(df2$LON)
    plots2[j,7]<-unique(df2$PLT_CN)
    plots2[j,8]<-unique(df2$sl)
    # colnames(plots2)<-c("Tree Density", "Maximum DBH", "Average DBH", "Observed Biomass", "LAT", "LON", "PLT_CN")
    # }
  }
}

c<-c(152, 84, 98, 143, 332, 268, 371, 48, 342, 186, 151, 156, 46, 316, 221, 95, 237, 193, 199, 126)
df <- plots2[plots2[,8] %in% c,]
write.csv(df, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/slash_plots_site_index.csv")
setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
envdata<-read.csv("slash_plots_site_index.csv", header=T, sep=",")

par(mfrow=c(1,5))
c<-c(152, 84, 98, 143, 332, 268, 371, 48, 342, 186, 151, 156, 46, 316, 221, 95, 237, 193, 199, 126)
# c<-c(2711,1869,2642,1766,1848,1027,196,2243,771,2328,170,2309,3105,2801,1984,2022,476,3614,1070,669)
c<-sort(c, decreasing = FALSE)
# diameter.totals<-matrix(0, nrow = length(c), ncol = 3000 )
for (o in c){
  diameter.totals<-vector()
  df<-plots[[o]]
  df2<-df[df$INVYR.y==max(df$INVYR.y) & df$STATUSCD=="1",]
  df2$TPA_UNADJ <- ifelse(is.na(df2$TPA_UNADJ), df2$TPAGROW_UNADJ, df2$TPA_UNADJ)
  for (h in 1:length(df2$DIA))
  {diameter.totals<-rep((df2$DIA), round(df2$TPA_UNADJ*2.47))}
  hist(diameter.totals, main = o, xlab = "Diameter (in)")}

#trying to save info from histogram in table
par(mfrow=c(2,5))
c<-c(152, 84, 98, 143, 332, 268, 371, 48, 342, 186, 151, 156, 46, 316, 221, 95, 237, 193, 199, 126)
c<-sort(c, decreasing = FALSE)
observed.DBH<-list()
# for (p in 1:length(c)){
  for (o in c){
  diameter.totals<-vector()
  df<-plots[[o]]
  df2<-df[df$INVYR.y==max(df$INVYR.y) & df$STATUSCD=="1",]
  df2$TPA_UNADJ <- ifelse(is.na(df2$TPA_UNADJ), df2$TPAGROW_UNADJ, df2$TPA_UNADJ)
    for (h in 1:length(df2$DIA))
      {diameter.totals<-rep((df2$DIA), round(df2$TPA_UNADJ*2.47))}
        hist(diameter.totals, main = o, xlab = "Diameter (in)")
  # }
  observed.DBH[[o]]<-diameter.totals
}

observed.DBH<-observed.DBH[lengths(observed.DBH) != 0] 

names(observed.DBH) <- c("plot_46", "plot_48", "plot_84", "plot_95", "plot_98", "plot_126", "plot_143", "plot_151",
                         "plot_152", "plot_156", "plot_186", "plot_193", "plot_199", "plot_221", "plot_237", 
                         "plot_268", "plot_316", "plot_332", "plot_342", "plot_371")

save(observed.DBH, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/observed.DBH_site_tree.RData")
load("C:/Users/Alicia/Documents/GitHub/FL_Carbon/observed.DBH_site_tree.RData")

B<-vector()
B<-sapply(observed.DBH, mean)
envdata$Average_DBH<-B
V<-vector()
V<-sapply(observed.DBH, length)
envdata$Tree_Density<-V

df<-envdata
envdata<-read.csv("slash_env_data_site_index.csv", header=T, sep=",")
envdata[,2:9]<-df[,2:9]
write.csv(envdata, file= "C:/Users/Alicia/Documents/GitHub/FL_Carbon/slash_env_data_site_index.csv")
