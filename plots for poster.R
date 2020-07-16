theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

# scale_fill_Publication <- function(...){
#   library(scales)
#   discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
#   
# }

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#495F8C","#F25C05","#F2B705", "#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

# scale_colour_Publication <- function(...){
#   library(scales)
#   discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
#   
# }
scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#495F8C","#F25C05","#F2B705","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}



# Biomass and DBH plots -------------------------------------------------------------------

ggplot(final_list, aes(Modeled_Biomass,Observed_Biomass )) +
  labs(title="Tree Carbon (gC/m^2)", x="Modeled", y="Observed") +
  xlim(0,20000) +
  ylim(0,20000) +
  geom_errorbarh(aes(xmin=final_list$Modeled_Biomass-(1.96*final_list$sd.tasb), 
                     xmax=final_list$Modeled_Biomass+(1.96*final_list$sd.tasb), 
                     height = .5)) +
  geom_point(aes(size=Tree_Density, color=Species), cex=5, alpha=0.75) +
  scale_color_manual(values=c("#495F8C", "#F25C05","#F2B705" )) +
  geom_abline(linetype = "dashed", size=1) +
  theme_Publication() +
  scale_fill_Publication() 
  # scale_colour_Publication()


ggplot(final_list, aes(Modeled_Diameter,Observed_Diameter )) +
  labs(title="Average DBH (inches)", x="Modeled DBH (in)", y="Observed DBH (in)") +
  xlim(2,16) +
  ylim(2,16) +
  #geom_abline(linetype = "dashed") +
  geom_errorbarh(aes(xmin=final_list$Modeled_Diameter-(1.96*final_list$sd.dbh), 
                     xmax=final_list$Modeled_Diameter+(1.96*final_list$sd.dbh), height = .5)) +
  geom_point(aes(size=Tree_Density, color=Species), cex=5, alpha=0.75) +
  scale_color_manual(values=c("#495F8C", "#F25C05","#F2B705")) +
  geom_abline(linetype = "dashed", size=1) +
  theme_Publication() +
  scale_fill_Publication() 
  # scale_colour_Publication()


# relative effect of coefficient ------------------------------------------

ggplot() +
  labs( x="Aridity index", y="Relative effect on growth") +
  # xlim(2,16) +
  # ylim(2,16) +
  geom_point(data=loblolly, 
             aes(loblolly$aridity_1, 10^(3.2046573*loblolly$aridity_1 - 1.9681509*loblolly$aridity2)/10^(3.2046573*min(loblolly$aridity_1)- 1.9681509*min(loblolly$aridity2))), 
             cex=4, alpha=0.5, color="#495F8C") +
  geom_point(data=slashpine, 
             aes(aridity_1, 10^(- 0.301793*slashpine$aridity_1)/10^(- 0.301793*min(slashpine$aridity_1))), 
             cex=4, alpha=0.5, color="#F2B705") +
  geom_point(data=longleaf, 
             aes(aridity_1, 10^(- 0.977477*longleaf$aridity_1)/10^(- 0.977477*min(longleaf$aridity_1))), 
             cex=4, alpha=0.5, color="#F25C05") +
  #scale_color_manual(values=c("#495F8C", "#F2B705", "#F25C05")) +
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication()

ggplot() +
  labs( x="Age (years)", y="Relative effect on growth") +
  # xlim(2,16) +
  # ylim(2,16) +
  geom_point(data=loblolly, 
             aes(AGEDIA, AGEDIA^(0.7405648)/AGEDIA*(1)^(0.7405648)), 
             cex=4, alpha=0.5, color="#495F8C") +
  geom_point(data=slashpine, 
             aes(AGEDIA, AGEDIA^(0.5784213)/AGEDIA*(1)^(0.5784213)), 
             cex=4, alpha=0.5, color="#F2B705") +
  geom_point(data=longleaf, 
             aes(AGEDIA, AGEDIA^(0.5718692)/AGEDIA*(1)^(0.5718692)), 
             cex=4, alpha=0.5, color="#F25C05") +
  #scale_color_manual(values=c("#495F8C", "#F2B705", "#F25C05")) +
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication()

ggplot() +
  labs( x="Soil CN ratio", y="Relative effect on growth") +
  # xlim(2,16) +
  # ylim(2,16) +
  geom_point(data=na.exclude(loblolly), 
             aes(na.exclude(loblolly$SOILGRIDS_CN_SCALE), 10^(- 0.0011033*na.exclude(loblolly$SOILGRIDS_CN_SCALE))/10^(- 0.0011033*min(na.exclude(loblolly$SOILGRIDS_CN_SCALE)))), 
             cex=4, alpha=0.5, color="#495F8C") +
  geom_point(data=na.exclude(longleaf), 
             aes(na.exclude(longleaf$SOILGRIDS_CN_SCALE), 10^(- 0.009259*na.exclude(longleaf$SOILGRIDS_CN_SCALE))/10^(- 0.009259*min(na.exclude(longleaf$SOILGRIDS_CN_SCALE)))), 
             cex=4, alpha=0.5, color="#F25C05") +
  #scale_color_manual(values=c("#495F8C", "#F2B705", "#F25C05")) +
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication()

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")

biomass<-read.csv("TASB_totals.csv", header=T, sep=",")

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(colour = "white", face = "bold"),
            panel.background = element_rect(fill= "#083A40", colour = "#083A40"),
            plot.background = element_rect(fill = "#083A40",
                                           colour = "#083A40"),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="white"),
            axis.ticks = element_line(colour="white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.background = element_rect(fill = "#083A40",
                                             colour = "#083A40"),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}


ggplot(biomass, aes(x=Location, y=TASB, fill=ï..Species)) + 
  labs(y="grams Carbon/m^2", title="Potential carbon storage for 700tph in year 2100") +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=TASB-SD, ymax=TASB+SD),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9), colour="white") +
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication()



# Extra -------------------------------------------------------------------



ggplot(final_biomass, aes(x=Location, y=TASB, fill=Species)) +
  labs(y="grams Carbon/m^2", title="Potential carbon storage for 700tph in year 2100") +
  # geom_boxplot() +
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  stat_summary(fun.y=mean, geom="point", shape=8, size=4, aes(color=Species), position="identity") +
  geom_errorbar(aes(ymin=TASB-SD, ymax=TASB+SD),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9), color="white") +
  # geom_jitter(shape=16, position=position_jitter(0.2), aes(color=Species), alpha=0.75, size=3) +
  # geom_boxplot(notch=FALSE, colour="white") +
  scale_fill_Publication() +
  scale_colour_Publication()+
  theme_Publication()

ggplot(fb, aes(x=Location, y=TASB, colour=Species)) +
  labs(y="grams Carbon/m^2", title="Potential carbon storage for 700tph in year 2100") +
  geom_point() +
  # geom_boxplot() +
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  geom_errorbar(aes(ymin=TASB-sd, ymax=TASB+sd),
                width=.2) +
  # geom_jitter(shape=16, position=position_jitter(0.2), aes(color=Species), alpha=0.75, size=3) +
  # geom_boxplot(notch=FALSE, colour="white") +
  scale_fill_Publication() +
  scale_colour_Publication()+
  theme_Publication()


par(mfrow=c(1,1))
plot(data=loblolly, loblolly$aridity_1, 10^(3.2046573*loblolly$aridity_1 - 1.9681509*loblolly$aridity2)/10^(3.2046573*min(loblolly$aridity_1)
                      - 1.9681509*min(loblolly$aridity2)), xlab= "Aridity", ylab= "Relative effect",
     xlim=c(.7,1.1), col="#495F8C")
points(slashpine$aridity_1, 10^(- 0.301793*slashpine$aridity_1)/10^(- 0.301793*min(slashpine$aridity_1)), col="#F2B705")
points(longleaf$aridity_1, 10^(- 0.977477*longleaf$aridity_1)/10^(- 0.977477*min(longleaf$aridity_1)), col="#F25C05")


par(mfrow=c(1,1))
plot(data=na.exclude(loblolly), loblolly$SOILGRIDS_CN_SCALE, 10^(- 0.0011033*loblolly$SOILGRIDS_CN_SCALE)/10^(- 0.0011033*min(loblolly$SOILGRIDS_CN_SCALE)), xlab= "Aridity", ylab= "Relative effect",
     col="#495F8C")
points(slashpine$aridity_1, 10^(- 0.301793*slashpine$aridity_1)/10^(- 0.301793*min(slashpine$aridity_1)), col="#F2B705")
points(longleaf$aridity_1, 10^(- 0.977477*longleaf$aridity_1)/10^(- 0.977477*min(longleaf$aridity_1)), col="#F25C05")
