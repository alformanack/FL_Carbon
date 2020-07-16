rm(list=ls())
setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")

df<-dplR::read.rwl("ms004.rwl",format="tucson")


# BA<-dplR::bai.in(df)
# BA2<-dplR::bai.out(df)
Age<-matrix(NA,length(df[,1]),length(df[1,]))
R<-matrix(NA,length(df[,1]),length(df[1,]))
D<-matrix(NA,length(df[,1]),length(df[1,]))
BA<-matrix(NA,length(df[,1]),length(df[1,]))
BAI<-matrix(NA,length(df[,1]),length(df[1,]))
DI<-matrix(NA,length(df[,1]),length(df[1,]))


for (j in 1:length(df[1,])){
  for (i in 2:length(df[,1])){
    if(is.na(df[i-1,j])==F & i==2){
      Age[i-1,j]<-1
      R[i-1,j]<-df[i-1,j]
      D[i-1,j]<-2*R[i-1,j]
      BA[i-1,j]<-3.14*(R[i-1,j]^2)
      BAI[i-1,j]<-BA[i-1,j]
      DI[i-1,j]<-D[i-1,j]
    }
    if(is.na(df[i,j])==F){
      if(is.na(df[i-1,j])==T){
        Age[i,j]<-1
        R[i,j]<-df[i,j]
        D[i,j]<-2*R[i,j]
        BA[i,j]<-3.14*(R[i,j]^2)
        BAI[i,j]<-BA[i,j]
        DI[i,j]<-D[i,j]
        
      }
      if(is.na(df[i-1,j])==F){
        Age[i,j]<- Age[i-1,j]+1
        R[i,j]<-R[i-1,j]+df[i,j]
        D[i,j]<-2*R[i,j]
        BA[i,j]<-3.14*(R[i,j]^2)
        BAI[i,j]<-BA[i,j]-3.14*(R[i-1,j]^2)
        DI[i,j]<-D[i,j]-1*(D[i-1,j])
      }
    }
    
  }
}

years<-as.numeric(row.names(df))
indiv<-colnames(df)
#turn everything into vectors
rows<-length(df[,1])
cols<-length(df[1,])
df_new<-matrix(NA,rows*cols,8)
colnames(df_new)<-c("year","age","BA","BAI","R","D","DI","indiv")
for (i in 1:cols){
  df_new[(rows*i-(rows-1)):(rows*i),c("year","age","BA","BAI","R","D","DI")]<-cbind(as.numeric(years),as.numeric(Age[,i]),
                                                                    as.numeric(BA[,i]),as.numeric(BAI[,i]),
                                                                    as.numeric(R[,i]),as.numeric(D[,i]),as.numeric(DI[,i]))
  df_new[(rows*i-(rows-1)):(rows*i),c("indiv")]<-indiv[i]
}
#df_new<-as.numeric(df_new)
df_new2<-data.frame(matrix(as.numeric(df_new),rows*cols,8))
colnames(df_new2)<-c("year","age","BA","BAI","R","D","DI","indiv")
df_new2[,"indiv"]<-df_new[,"indiv"]
df_new<-na.omit(df_new2)


df_new<-df_new[which(df_new$BAI!=0),]
df_new$D<-df_new$D/25.4
#model<-lm(log(BAI)~(age)+log(BA)+year,data=df_new)
model<-lme4::lmer(log10(D)~log10(age)+(1+log10(age)|indiv),data=df_new)
model2<-lm(log10(D)~log10(age)+log10(year),data=df_new)
sjPlot::tab_model(model)
df_new$resid<-model@resp$y-model@resp$mu
#plot(df_new$year,df_new$resid,col=rgb(0,0,0,0.2))
plot(log10(df_new$D),model@resp$mu,col=rgb(0,0,0,0.2),pch=16,cex=1.5)
abline(0,1,col=rgb(1,0,0,0.3),lwd=2)
hist(coef(model)$indiv[,2],col="gray",xlab="value")
