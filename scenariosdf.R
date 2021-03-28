scenarios<-matrix(0, nrow=8, ncol=3)
scenarios<-as.data.frame(scenarios)
row.names(scenarios)<-c("Dry/HighN/Hot", "Dry/LowN/Hot", "Wet/HighN/Hot", "Wet/LowN/Hot",
                        "Dry/HighN/Cool", "Dry/LowN/Cool", "Wet/HighN/Cool", "Wet/LowN/Cool")

colnames(scenarios)<-c("Aridity", "SoilCN", "MAT")
scenarios[1:8,1]<-0.75
scenarios[3:4 ,1]<-1.1
scenarios[7:8 ,1]<-1.1
scenarios[c(1,3,5,7),2]<-40
scenarios[c(2,4,6,8),2]<-120
scenarios[1:4,3]<-24
scenarios[5:8,3]<-20

save(scenarios, file = "C:/Users/Alicia/Documents/GitHub/FL_Carbon/scenarios.rdata")
