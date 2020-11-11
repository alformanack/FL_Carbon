# Get plot data from FIA TREE file ----------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Desktop/FL")

tree<-read.csv("FL_TREE.csv", header=T, sep=",")

unique.plot<-unique(tree$PLT_CN)

plot_notlongleaf<-unique(tree[tree$SPCD!=121,]$PLT_CN)

unique.121 <- setdiff(unique.plot, plot_notlongleaf)

df <- tree[tree$PLT_CN %in% unique.121,]

plot<-read.csv("FL_PLOT.csv")

plot_id<-unique(df$PLT_CN)

for (i in 1:length(plot_id)){
  test<-subset(plot,CN==plot_id [i])
  rows<-row.names(df[which(df$PLT_CN==plot_id[i]),])
  df[rows,"LAT"]<-test$LAT
  df[rows,"LON"]<-test$LON
}


# #add GPS coordinates by linking CN in plot df to PLT_CN in tree df
# df1<-subset(plot, select=c("CN", "LAT", "LON"))
# df3<-merge(x = df1, y = df, by.x = "CN", by.y = "PLT_CN", all = TRUE)

#create list element for separate plots with longleaf pine present
plots<-list()

for (i in 1:length(unique.121)){
  plots[[i]]<-subset(df, df$PLT_CN==unique.121[i])
}

mean(plots[[8]]$DIA*plots[[8]]$TPA_UNADJ)


# #Find plots with >= 80 percent longleaf
# plots2<-list()
# for (h in 1:6041) {
#   df1<-plots[[h]]
#   df2<-df1[df1$INVYR==max(df1$INVYR) & df1$STATUSCD=="1",]
#   longleaf<-subset(df2, df2$SPCD==121)
#   # if there is no elements in my df, skip the dataframe
#   if (nrow(df2) != "0") {
#     if((length(longleaf$TREE)/length(df2$TREE)>=.8)){plots2[[h]]<-df2}
#     print(h)
#     # else if (length(longleaf$TREE)/length(df2$TREE)<.8){M<-0}
#     # if(M==1){plots2[[h]]<-df2}
#   }
#   # final_list <- do.call(rbind.data.frame, plots2)
#   # else {
#   #   plots2[[h]] <- NA
#   # }
# } 
# 
# #reorder plots with majority longleaf
# plots3<-list() 
# c<-c(26,29,31,47,49,106,111,113,119,120,123,127,153,154,172,929,897,867,866, 3002,3997,4996,5987,6028, 6036,
#      6015,6007,5987,5985,5974,5972,5971,4960,4981,4836,4695)
# # c<-c(255,262,263,287,292,408,436,439,456,457,462,470,472,473,477)
# for (i in 1:length(c)){
#   plots3[[i]]<-plots2[[c[i]]]
# }
# 
# lapply(plots3, `[`, c('INVYR'))
# lapply(plots3, `[`, c('LAT'))
# lapply(plots2, max(plots[[2]]$INVYR), c('STATUSCD'))
# plots2[[1]]$LAT.x
# plots2[[2]]$LAT.x
# plots2[[3]]$LAT.x
# plots2[[4]]$LAT.x
# plots2[[5]]$LAT.x #different GPS coordinates
# plots2[[5]]$CN#different GPS coordinates
# plots2[[6]]$LAT.x #different GPS coordinates
# plots2[[7]]$LAT.x 
# plots2[[8]]$LAT.x #different GPS coordinates
# plots2[[9]]$LAT.x 
# plots2[[10]]$LAT.x 
# plots2[[11]]$LAT.x #different GPS coordinates 
# plots2[[12]]$LAT.x 
# plots2[[13]]$LAT.x 
# plots2[[14]]$LAT.x 
# plots2[[15]]$LAT.x #different GPS coordinates
# 

#Calculate plot information
j = 0
plots4<-matrix(0, ncol = 9, nrow = 78)
colnames(plots4)<-c("Tree_Density", "Maximum_DBH", "Average_DBH", "sd", "Observed_Biomass", "LAT", "LON", "PLT_CN", 
                    "original_plot")
TASB<-vector()
c<-c(1:804)
for (h in c) {
  df<-plots[[h]]
  df$sl <- h
  df2<-df[df$INVYR==max(df$INVYR) & df$STATUSCD=="1",]
  
  if (nrow(df2) > 15 & sd(df2$DIA)<2.6) {
    
    j = j+1
    
  df2$TPA_UNADJ <- ifelse(is.na(df2$TPA_UNADJ), df2$TPAGROW_UNADJ, df2$TPA_UNADJ)
  df2_sum<-sum(df2$TPA_UNADJ)
  plots4[j,3]<-sum(df2$DIA*df2$TPA_UNADJ)/df2_sum
  plots4[j,4]<-sd(df2$DIA)
  plots4[j,1]<-df2_sum*2.47 #trees per acre converted to trees per hectare
  plots4[j,2]<-max(df2$DIA)
  TASB<-((0.0725*((df2$DIA*2.54)^2.5074))+(0.0016*((df2$DIA*2.54)^3.0786))+(0.0214*((df2$DIA*2.54)^2.0051)))*(round(df2$TPA_UNADJ)*2.47)
  plots4[j,5]<-sum(TASB)*.5*1000*(1/10000)
  plots4[j,6]<-unique(df2$LAT)
  plots4[j,7]<-unique(df2$LON)
  plots4[j,8]<-unique(df2$PLT_CN)
  plots4[j,9]<-unique(df2$sl)
  } 
}  
plots3<-as.data.frame(plots4)
ggplot(plots3, aes(x = LAT, y = LON, label=plots3$original_plot)) +
  geom_point() +geom_text(hjust = 0, nudge_x = 0.05, check_overlap = TRUE)


df <- plots4[plots4[,9] %in% c,]
  
plots4[]

#produce histograms of diameter distributions    
c<-c(711,554,673,765,45,61,22,70,69,8,136,126,749,747,731,752,637,155,163,462)
c<-sort(c, decreasing = FALSE)
par(mfrow=c(1,5))  
for (o in c){
    diameter.totals<-vector()
    df<-plots[[o]]
    df2<-df[df$INVYR==max(df$INVYR) & df$STATUSCD=="1" & df$SPCD=="121",]
    df2$TPA_UNADJ <- ifelse(is.na(df2$TPA_UNADJ), df2$TPAGROW_UNADJ, df2$TPA_UNADJ)
    if (nrow(df2) != "0") {
    for (h in 1:length(df2$DIA))
    {diameter.totals<-rep((df2$DIA), round(df2$TPA_UNADJ*2.47))}
    hist(diameter.totals, main = o, xlab = "Diameter (in)")}
}





c<-c(2:15)

#Look for cut stumps
  
for (o in c){
    diameter.totals<-vector()
    df<-plots[[o]]
    print(o)
    df2<-df[df$INVYR==max(df$INVYR) & df$STATUSCD=="3" & df$SPCD=="121",]
    if (nrow(df2) != "0") {
      hist(df2$DIA, main = o, xlab = "cut stump Diameter")
    }
}

  


# checking if calculations are right in loop
sum(plots[[14]][1:11,111])*2.47)
hist(plots[[14]][1:11,18])
max(plots[[14]][1:11,18])
DIA<-as.vector(plots[[14]][1:11,18])
Trees<-as.vector(plots[[14]][1:11,111])
Bio<-0.041281*((DIA*2.54)^2.722214)
T_Bio<-(Bio*round(Trees))*2.47
Sum_T_BIO<-sum(T_Bio)*.5*1000*(1/10000)  

plots2[]

#Plot 1 has no values because they are all cut stumps
plots[[255]]$INVYR
plots[[255]]$STATUSCD #status code 3 = cut stump
plots[[255]]$SPCD 
plots[[255]]$DIA 
plots[[255]]$TPAGROW_UNADJ 

#Plot 2 is missing TPA_unadjusted values, need to calculate
plots[[2]]$INVYR
plots[[2]]$STATUSCD
plots[[2]]$TREE
plots[[2]]$DIA
plots[[2]]$TPA_UNADJ
plots[[2]]$TPAGROW_UNADJ


write.csv(df, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/longleafplots.csv")
setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")
plots2<-read.csv("longleafplots.csv", header=T, sep=",")


