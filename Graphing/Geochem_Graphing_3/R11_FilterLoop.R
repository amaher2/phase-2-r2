#filter for creating data table sub tables
#Code from MakingGraphs.R that works
#First have these packages
#File names need to be updated

library(smwrGraphs)
library(ggplot2)
library("reshape")
library(dplyr)

#Then bring the data in
QWgraphData <- read.csv("P:/FPJ00/Phase2GeochemistryData/QwGdatWellExport.csv")
names(QWgraphData)
sapply(QWgraphData, class)
QWgraphData %>% mutate_if(is.factor, as.character) -> QWgraphData
sapply(QWgraphData,class)

#Loop to make datatables based on srsname
QWgraphData$srsname <- as.character(QWgraphData$srsname)

col.filters <- unique(QWgraphData$srsname) 

lapply(seq_along(col.filters), function(x) {
  filter(QWgraphData, srsname == col.filters[x])
}
) -> list

names(list) <- col.filters

list2env(list, .GlobalEnv)

#Filtering tritium data
CW13H<-filter(Tritium, Piez_Nest == "CW_1")
CW23H<-filter(Tritium, Piez_Nest == "CW_2")
HFC3H<-filter(Tritium, Piez_Nest == "HFC_1")
LF13H<-filter(Tritium, Piez_Nest == "LF_1")
LF23H<-filter(Tritium, Piez_Nest == "LF_2")
Ol3H<-filter(Tritium, Piez_Nest == "Ol_1")





















#Then bring the data in
names(g.all2)get 
g.all2$srsname[1]
sapply(g.all2,class)
g.all2 %>% mutate_if(is.factor, as.character) -> g.all2

g.all2<-g.all2[with(g.all2,order(Site_Name,MeanSampleDepth_ftBLS)),]
g.all2

#Filtering to get certain analytes from certain sites
Sodium <- filter(g.all2, srsname == "Sodium")
Chloride <- filter(g.all2, srsname == "Chloride")
SodiumOlivia <- filter(Sodium, Site_name == "Olivia")
ChlorideOlivia <- filter(Chloride, Site_name == "Olivia")
ChlorideOliviaWG <- filter(ChlorideOlivia, medium_cd == "WG")
ChlorideOliviaGW2 <- filter(ChlorideOliviaWG, meth_cd == "IC022")

#Cannot make continous x and y unless all the values are numeric (now the ggplot2 code should work)
sapply(ChlorideOliviaSelect, class)
ChlorideOliviaSelect$result_va<-as.numeric(ChlorideOliviaSelect$result_va)

#Loop to make datatables based on srsname
g.all2$srsname <- as.character(g.all2$srsname)

col.filters <- unique(g.all2$srsname) 

lapply(seq_along(col.filters), function(x) {
  filter(g.all2, srsname == col.filters[x])
}
) -> list

names(list) <- col.filters

list2env(list, .GlobalEnv)

#Filtering tritium data
Olivia3H<-filter(Tritium, Site_name == "Olivia")
OliviaWG3H<-filter(Olivia3H, parameter_units == "T.U.")
HFC3H<-filter(Tritium, Site_name == "University of Minnesota Hydrogeology Field Camp")
