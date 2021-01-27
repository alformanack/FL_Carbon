#Finding loblolly plots that were remeasured

# Get plot data from FIA TREE file ----------------------------------------

rm(list=ls())

setwd("C:/Users/Alicia/Desktop/FL")

tree<-read.csv("FL_TREE.csv", header=T, sep=",") %>%
  mutate(PLT_CN = as.character(PLT_CN))

unique.plot<-unique(tree$PLT_CN)

plot_notLob<-unique(tree[tree$SPCD!=131,]$PLT_CN)

# unique.prev<-unique(plot$PREV_PLT_CN)
# 
# remeasure<-df[df$PLT_CN %in% unique.prev]

unique.131 <- setdiff(unique.plot, plot_notLob)

cut_trees<- unique(tree[tree$STATUSCD==3,]$PLT_CN)

filter_cut_trees<-setdiff(unique.plot, cut_trees)

# intersection of filter_cut_trees and unique.111
# intersect(unique.111, filter_cut_trees) %>% length()

library(tidyverse)

#We're first going to use the SURVEY table to identify plots that were measured under the annual inventory system.

fiaSurvey <- read.csv("FL_SURVEY.csv", header=T, sep=",") %>%
  filter(ANN_INVENTORY == 'Y') %>%
  mutate(CN = as.character(CN))

#Next we'll read in the PLOT file, and filter it. We build in some redundancy in our filtration among multiple 
#tables, to ensure we're getting only the data we want. Filtering the PLOT table will focus on again identifying 
#plots that were measured or re-measured in the annual survey system. We also want to identify plots that were 
#forested.


fiaPlots <- read.csv("FL_PLOT.csv", header=T, sep=",") %>%
  mutate(CN = as.character(CN)) %>%
  filter(DESIGNCD==1) %>%
  filter(KINDCD == 1 | KINDCD == 2) %>% #first or re-measurement in annual survey system
  filter(PLOT_STATUS_CD == 1) %>% #accessible forest measured
  mutate(CN = as.character(CN),
         SRV_CN = as.character(SRV_CN)) %>%
  filter(SRV_CN %in% fiaSurvey$CN) %>% # confirms that these are coded as annual inventory
  filter(INVYR != 9999)



fiaTrees <- tree %>%
  mutate(PLT_CN = as.character(PLT_CN)) %>%
  filter(PLT_CN %in% intersect(unique.131, filter_cut_trees)) %>%
  group_by(PLT_CN) %>%
  # mutate(numSubplots = length(unique(SUBP))) %>%
  mutate(maxSUBP= max(SUBP)) %>% 
  ungroup() %>%
  filter(maxSUBP<=4)
#this ensures we're working with only plots where
#measurements were made on all 4 subplots

###only selecting plots with slash pine tree only

plotFinalFilt <- fiaPlots %>%
  filter(CN %in% fiaTrees$PLT_CN)

#Having pulled in and done preliminary filtering on these data, what we want to do is get the start and 
#end conditions for each revisited plot.

##Joining tree data file with plot info

treeCNStats <- fiaTrees %>%
  filter(STATUSCD != 0) %>% #drop trees that are not part of the sample
  left_join(., plotFinalFilt %>%
              mutate(PREV_PLT_CN = as.character(PREV_PLT_CN),
                     CN=as.character(CN)) %>%
              select(CN, PREV_PLT_CN, MEASYEAR, LAT, LON, ELEV, DESIGNCD,
                     MACRO_BREAKPOINT_DIA),
            by = c('PLT_CN' = 'CN'))

#The PLOTS table provides us with both a CN value - the current number for a plot - and its PREV_PLT_CN. 
#This lets us match 'backwards' in time to identify the measurements made previously on the same plot. 
#We will do this by defining a vector we'll call remeasCNs. remeasCNs is defined as the set of unique PLT_CN values 
#for which the tree data from the PREV_PLT_CN - the previous measurements made on this plot - are also available.

startPlotCNs <- unique(treeCNStats$PLT_CN)
startPlotCNs <- startPlotCNs[startPlotCNs %in% treeCNStats$PREV_PLT_CN]
# 
# remeasCNs <- unique(treeCNStats %>%
#                       filter(PREV_PLT_CN %in% startPlotCNs) %>%
#                       filter(!is.na(PREV_PLT_CN)) %>%
#                       .$PLT_CN)
# 
# startPlotInfo <- treeCNStats %>%
#   filter(PLT_CN %in% startPlotCNs) %>%
#   select(PLT_CN, CN, STATUSCD, SPCD, SPGRPCD, DIA, MEASYEAR, TPA_UNADJ,
#          MACRO_BREAKPOINT_DIA) %>%
#   rename(TREE_CN = CN)
# 
# remeasPlotInfo <- treeCNStats %>%
#   filter(PLT_CN %in% remeasCNs) %>%
#   select(PLT_CN, PREV_PLT_CN, CN, PREV_TRE_CN, STATUSCD, SPCD, SPGRPCD, DIA, MEASYEAR,
#          TPA_UNADJ, MACRO_BREAKPOINT_DIA) %>%
#   rename(secondTreeStatus = STATUSCD,
#          secondDIA = DIA,
#          secondTPA = TPA_UNADJ)

# treeCNStats %>%
#   filter(LAT == "30.17" & LON == "-82.44") %>% 
#   # filter(PLT_CN == startPlotCNs[10] | PREV_PLT_CN == startPlotCNs[10]) %>%
#   # filter(PLT_CN == "156125285010854" | PREV_PLT_CN == "156125285010854") %>%
#   select(CN, PLT_CN, PREV_PLT_CN, LAT, LON) %>%
#   # view()
#   pull(PLT_CN) %>% 
#   unique()

treeCNStats %>% 
  select(CN, PLT_CN, PREV_PLT_CN, LAT, LON, MEASYEAR, PREV_TRE_CN, DESIGNCD, SUBP, TREE, STATUSCD, DIA, TPA_UNADJ, TPAGROW_UNADJ) %>% 
  mutate(LATLON = as.character(paste0(LAT, LON))) -> plots.unlisted

plots.list <- list()

unique.latlon<- plots.unlisted %>% pull(LATLON) %>% unique()


for (i in 1:length(unique.latlon)) {
  plots.list[[i]] <- plots.unlisted %>% 
    filter(LATLON %in% unique.latlon[i])
}

# plots.list[[1]] %>% 
#   mutate(how_long = max(MEASYEAR) - min(MEASYEAR)) %>% 
#   view()

plots.remeasure <- 
  lapply(plots.list, function(x){
    mutate(x, how_long = as.numeric(max(MEASYEAR) - min(MEASYEAR)))
  })

#filtering remeasurments for time intervals greater than 10 years
plots.remeasure <-
  lapply(plots.remeasure, function(x){
    filter(x, how_long > 0)
  })

#removing empty dataframes in list
plots.remeasure<-plots.remeasure[sapply(plots.remeasure, function(x) dim(x)[1]) > 0]

view(plots.remeasure[[1]])
16

#creating single dataframe that has all plots remeasured withtime since remeasurement being greater than 10 years
#saving it as slash_remeas_plots to extract env data using ArcMap

plots2<-as.data.frame(matrix(0, ncol = 4, nrow = 10))
colnames(plots2)<-c("PLT_CN", "LAT", "LON", "TIME_INT")

for (j in 1:10) {
  df<-plots.remeasure[[j]]
  plots2[j,1]<-max(df$PLT_CN)
  plots2[j,2]<-unique(df$LAT)
  plots2[j,3]<-unique(df$LON)
  plots2[j,4]<-unique(df$how_long)
}

#write.csv(plots2, "C:/Users/Alicia/Documents/GitHub/FL_Carbon/slash_remeas_plots.csv")

#looking at spatial variability of remeasurment plots
ggplot(plots2, aes(x = LON, y = LAT, label=row.names(plots2))) +
  geom_point() +geom_text(hjust = 0, nudge_x = 0.05, check_overlap = TRUE)

#randomly sleecting remeasurment plots, subsetting the data, and checking spatial variability again
a<-sample(1:77, 20, replace=F)
a<-sort(a, decreasing = FALSE)
a<-c(2,  9, 13,  17,  32, 36)  # 38(need to delete rows) 44 (good) 54 58 65 67 72 74 75)
# a<- c(376, 386, 479, 273, 227, 309,   7, 134,  81, 380, 482, 292, 534, 388,  25, 244,  45,  19, 187, 205)
#a<-c(451, 177, 587, 546, 124, 547, 235, 131, 589, 141, 303, 305, 188,  10, 391, 318, 348, 464, 402, 67)

plots3<-subset(plots2, row.names(plots2) %in% a)

ggplot(plots3, aes(x = LON, y = LAT, label=row.names(plots3))) +
  geom_point() +geom_text(hjust = 0, nudge_x = 0.05, check_overlap = TRUE)

#subsetting the data into a list of initial measurement plots (plots.start) and final measurment (plots.end)
plots.start<- list()
df<-data.frame()
for (h in a) {
  df<-plots.remeasure[[h]]
  df<-subset(df, df$MEASYEAR== min(df$MEASYEAR))
  plots.start[[h]]<-df
}

final.start<-plots.start[lengths(plots.start) != 0]

#save(final.start, file="slash_remeas_start.Rdata")

plots.end<- list()

for (h in a) {
  df<-plots.remeasure[[h]]
  df<-subset(df, MEASYEAR==max(MEASYEAR))
  plots.end[[h]]<-df
}


final.end<-plots.end[lengths(plots.end) != 0]

#save(final.end, file="slash_remeas_end.Rdata")

view(final.start[[1]])
view(final.end[[1]])
