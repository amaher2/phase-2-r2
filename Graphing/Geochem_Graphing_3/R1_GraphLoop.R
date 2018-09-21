#Making graphs with the hydrogeologic and geochem data! Need to go through and put 
#Before making graphs again, need to go through and update to the csv you want to use. Also, Site_name must be changed to Site_Name

#Loop
library(smwrGraphs)
library(ggplot2)
library("reshape")
library(dplyr)

QWgraphData2 <- read.csv("P:/FPJ00/analysis/Combined_geochem/Graphing/InputDatasets/QwExport3.csv")
sapply(QWgraphData2, class)
QWgraphData2 %>% mutate_if(is.factor, as.character) -> QWgraphData2
sapply(QWgraphData2, class)
QWgraphData2$result_va<-as.numeric(QWgraphData2$result_va)


#names(g.all2)get 
#g.all2$srsname[1]
#sapply(g.all2,class)
#g.all2 %>% mutate_if(is.factor, as.character) -> g.all2
#sapply(g.all2,class)
#g.all2$result_va<-as.numeric(g.all2$result_va)



QWgraphData2<-QWgraphData2[with(QWgraphData2,order(Site_Name,MeanSampleDepth_ftBLS)),]




#Cations
unique(QWgraphData2$srsname[QWgraphData2$medium_cd=="WG"])
cations<-c("Calcium","Magnesium","Sodium","Manganese","Iron")
parm<-c("Calcium",
        "Silica",
        "Phosphorus",
        "Total dissolved solids",
        "d18O",
        "Bicarbonate" ,
        "Nitrate" ,
        "Tritium" ,
        "Magnesium" ,
        "Carbonate (CO3)" ,
        "Sulfate" ,
        "Alkalinity",
        "Ammonia and ammonium",
        "Temperature_ water",
        "Inorganic nitrogen (nitrate and nitrite)",
        "Oxygen",
        "Total hardness -- SDWA NPDWR",
        "Bromide" ,
        "Potassium" ,
        "Nitrite" ,
        "Carbon dioxide",
        "pH" ,
        "Fluoride",
        "Chloride",
        "Specific conductance",
        "Sodium_ percent total cations" ,
        "Iron",
        "dD",
        "Sodium",
        "Manganese" ,
        "Hardness_ non-carbonate" ,
        "Acetate" ,
        "Phosphate" ,
        "Cl.Br" ,
        "Formate" ,
        "Thiosulfate")
a<-"Calcium"
s<-"Olivia"
QWgraphData2[is.na(QWgraphData2$Site_Name),]
setPDF(layout = "landscape",basename = "qwprofiles",multiplefiles = FALSE)
setLayout(width = c(1.5,1.5,1.5,1.5,1.5),height = 6)
plot(0,0)
for(s in c("Cromwell","Litchfield","Olivia","HFC")) {
  tmp.df<-QWgraphData2[QWgraphData2$Site_Name==s&QWgraphData2$medium_cd=="WG",]
  for(a in cations){
    tmp.df2<-tmp.df[tmp.df$srsname==a,]
    
    with(tmp.df2,xyPlot(x =result_va ,y=MeanSampleDepth_ftBLS,yaxis.rev = TRUE,
                        caption = paste(tmp.df2$Site_Name[1],tmp.df2$srsname[1], tmp.df2$parameter_units[1]),Plot=list(what="points")))
    with(tmp.df2,addXY(x =result_va ,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="lines")))
    
  }
}
dev.off()






#More graphs Anions
unique(QWgraphData2$srsname[QWgraphData2$medium_cd=="WG"])
anions<-c("Chloride","Bromide","Nitrate","Fluoride","Sulfate")
parm<-c("Calcium",
        "Silica",
        "Phosphorus",
        "Total dissolved solids",
        "d18O",
        "Bicarbonate" ,
        "Nitrate" ,
        "Tritium" ,
        "Magnesium" ,
        "Carbonate (CO3)" ,
        "Sulfate" ,
        "Alkalinity",
        "Ammonia and ammonium",
        "Temperature_ water",
        "Inorganic nitrogen (nitrate and nitrite)",
        "Oxygen",
        "Total hardness -- SDWA NPDWR",
        "Bromide" ,
        "Potassium" ,
        "Nitrite" ,
        "Carbon dioxide",
        "pH" ,
        "Fluoride",
        "Chloride",
        "Specific conductance",
        "Sodium_ percent total cations" ,
        "Iron",
        "dD",
        "Sodium",
        "Manganese" ,
        "Hardness_ non-carbonate" ,
        "Acetate" ,
        "Phosphate" ,
        "Cl.Br" ,
        "Formate" ,
        "Thiosulfate")
a<-"Chloride"
s<-"Olivia"
QWgraphData2[is.na(QWgraphData2$Site_name),]
setPDF(layout = "landscape",basename = "qwprofiles2",multiplefiles = FALSE)
setLayout(width = c(1.5,1.5,1.5,1.5,1.5),height = 6)
plot(0,0)

s<-"Litchfield"
a<-"Bromide"
for(s in c("Cromwell","Litchfield","Olivia","HFC")) {
  tmp.df<-QWgraphData2[QWgraphData2$Site_name==s&QWgraphData2$medium_cd=="WG",]
  for(a in anions){
    tmp.df2<-tmp.df[tmp.df$srsname==a,]
    
    with(tmp.df2[tmp.df2$anl_ent_cd=="USGSNWQL",],xyPlot(x =result_va ,y=MeanSampleDepth_ftBLS,yaxis.rev = TRUE,
                                                         caption = paste(tmp.df2$Site_name[1],tmp.df2$srsname[1], tmp.df2$parameter_units[1]),Plot=list(what="points")))
    with(tmp.df2[tmp.df2$anl_ent_cd=="USGSNWQL",],addXY(x =result_va ,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="lines")))
    with(tmp.df2[tmp.df2$anl_ent_cd=="Rick A. Knurr Ion Chrom Analytical",],
         addXY(x=result_va,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="points",color="red")))
    
  }
}
dev.off()



#More graphs WI
unique(QWgraphData2$srsname[QWgraphData2$medium_cd=="WI"])
porewater<-c("Fluoride","Nitrate","Chloride","Phosphate","Sulfate")
parm<-c("Calcium",
        "Silica",
        "Phosphorus",
        "Total dissolved solids",
        "d18O",
        "Bicarbonate" ,
        "Nitrate" ,
        "Tritium" ,
        "Magnesium" ,
        "Carbonate (CO3)" ,
        "Sulfate" ,
        "Alkalinity",
        "Ammonia and ammonium",
        "Temperature_ water",
        "Inorganic nitrogen (nitrate and nitrite)",
        "Oxygen",
        "Total hardness -- SDWA NPDWR",
        "Bromide" ,
        "Potassium" ,
        "Nitrite" ,
        "Carbon dioxide",
        "pH" ,
        "Fluoride",
        "Chloride",
        "Specific conductance",
        "Sodium_ percent total cations" ,
        "Iron",
        "dD",
        "Sodium",
        "Manganese" ,
        "Hardness_ non-carbonate" ,
        "Acetate" ,
        "Phosphate" ,
        "Cl.Br" ,
        "Formate" ,
        "Thiosulfate")
a<-"Fluoride"
s<-"HFC"
QWgraphData2[is.na(QWgraphData2$Site_name),]
setPDF(layout = "landscape",basename = "wiprofiles",multiplefiles = FALSE)
setLayout(width = c(1.5,1.5,1.5,1.5,1.5),height = 6)
plot(0,0)
for(s in c("Cromwell","Litchfield","Olivia","HFC")) {
  tmp.df<-QWgraphData2[QWgraphData2$Site_name==s&QWgraphData2$medium_cd=="WI",]
  for(a in porewater){
    tmp.df2<-tmp.df[tmp.df$srsname==a,]
    if(nrow(tmp.df2)>0){
      with(tmp.df2,xyPlot(x =result_va ,y=MeanSampleDepth_ftBLS,yaxis.rev = TRUE,
                          caption = paste(tmp.df2$Site_name[1],tmp.df2$srsname[1], tmp.df2$parameter_units[1]),Plot=list(what="points")))
      with(tmp.df2,addXY(x =result_va ,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="lines")))  
    } else {
      next
    }
    
    
  }
}
dev.off()
s
a

warnings()


#More graphs WI
unique(QWgraphData2$srsname[QWgraphData2$medium_cd=="WI"])
moreporewater<-c("Acetate","Bromide","Cl.Br","Nitrite","Thiosulfate")
parm<-c("Calcium",
        "Silica",
        "Phosphorus",
        "Total dissolved solids",
        "d18O",
        "Bicarbonate" ,
        "Nitrate" ,
        "Tritium" ,
        "Magnesium" ,
        "Carbonate (CO3)" ,
        "Sulfate" ,
        "Alkalinity",
        "Ammonia and ammonium",
        "Temperature_ water",
        "Inorganic nitrogen (nitrate and nitrite)",
        "Oxygen",
        "Total hardness -- SDWA NPDWR",
        "Bromide" ,
        "Potassium" ,
        "Nitrite" ,
        "Carbon dioxide",
        "pH" ,
        "Fluoride",
        "Chloride",
        "Specific conductance",
        "Sodium_ percent total cations" ,
        "Iron",
        "dD",
        "Sodium",
        "Manganese" ,
        "Hardness_ non-carbonate" ,
        "Acetate" ,
        "Phosphate" ,
        "Cl.Br" ,
        "Formate" ,
        "Thiosulfate")
a<-"Acetate"
s<-"HFC"
QWgraphData2[is.na(QWgraphData2$Site_name),]
setPDF(layout = "landscape",basename = "wi2profiles",multiplefiles = FALSE)
setLayout(width = c(1.5,1.5,1.5,1.5,1.5),height = 6)
plot(0,0)
for(s in c("Cromwell","Litchfield","Olivia","HFC")) {
  tmp.df<-QWgraphData2[QWgraphData2$Site_name==s&QWgraphData2$medium_cd=="WI",]
  for(a in moreporewater){
    tmp.df2<-tmp.df[tmp.df$srsname==a,]
    if(nrow(tmp.df2)>0){
      with(tmp.df2,xyPlot(x =result_va ,y=MeanSampleDepth_ftBLS,yaxis.rev = TRUE,
                          caption = paste(tmp.df2$Site_name[1],tmp.df2$srsname[1], tmp.df2$parameter_units[1]),Plot=list(what="points")))
      with(tmp.df2,addXY(x =result_va ,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="lines")))  
    } else {
      next
    }
    
    
  }
}
dev.off()


#More graphs WI
unique(QWgraphData2$srsname[QWgraphData2$medium_cd=="WI"])
moreporewater<-c("Acetate","Bromide","Cl.Br","Nitrite","Thiosulfate")
parm<-c("Calcium",
        "Silica",
        "Phosphorus",
        "Total dissolved solids",
        "d18O",
        "Bicarbonate" ,
        "Nitrate" ,
        "Tritium" ,
        "Magnesium" ,
        "Carbonate (CO3)" ,
        "Sulfate" ,
        "Alkalinity",
        "Ammonia and ammonium",
        "Temperature_ water",
        "Inorganic nitrogen (nitrate and nitrite)",
        "Oxygen",
        "Total hardness -- SDWA NPDWR",
        "Bromide" ,
        "Potassium" ,
        "Nitrite" ,
        "Carbon dioxide",
        "pH" ,
        "Fluoride",
        "Chloride",
        "Specific conductance",
        "Sodium_ percent total cations" ,
        "Iron",
        "dD",
        "Sodium",
        "Manganese" ,
        "Hardness_ non-carbonate" ,
        "Acetate" ,
        "Phosphate" ,
        "Cl.Br" ,
        "Formate" ,
        "Thiosulfate")
a<-"Acetate"
s<-"HFC"
QWgraphData2[is.na(QWgraphData2$Site_name),]
setPDF(layout = "landscape",basename = "wi2profiles",multiplefiles = FALSE)
setLayout(width = c(1.5,1.5,1.5,1.5,1.5),height = 6)
plot(0,0)
for(s in c("Cromwell","Litchfield","Olivia","HFC")) {
  tmp.df<-QWgraphData2[QWgraphData2$Site_name==s&QWgraphData2$medium_cd=="WI",]
  for(a in moreporewater){
    tmp.df2<-tmp.df[tmp.df$srsname==a,]
    if(nrow(tmp.df2)>0){
      with(tmp.df2,xyPlot(x =result_va ,y=MeanSampleDepth_ftBLS,yaxis.rev = TRUE,
                          caption = paste(tmp.df2$Site_name[1],tmp.df2$srsname[1], tmp.df2$parameter_units[1]),Plot=list(what="points")))
      with(tmp.df2,addXY(x =result_va ,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="lines")))  
    } else {
      next
    }
    
    
  }
}
dev.off()



#More graphs GW
unique(QWgraphData2$srsname[QWgraphData2$medium_cd=="WG"])
moreWGgraphs<-c("Alkalinity","Bicarbonate","Carbon dioxide","Carbonate (CO3)","Ammonia and ammonium")
parm<-c("Calcium",
        "Silica",
        "Phosphorus",
        "Total dissolved solids",
        "d18O",
        "Bicarbonate" ,
        "Nitrate" ,
        "Tritium" ,
        "Magnesium" ,
        "Carbonate (CO3)" ,
        "Sulfate" ,
        "Alkalinity",
        "Ammonia and ammonium",
        "Temperature_ water",
        "Inorganic nitrogen (nitrate and nitrite)",
        "Oxygen",
        "Total hardness -- SDWA NPDWR",
        "Bromide" ,
        "Potassium" ,
        "Nitrite" ,
        "Carbon dioxide",
        "pH" ,
        "Fluoride",
        "Chloride",
        "Specific conductance",
        "Sodium_ percent total cations" ,
        "Iron",
        "dD",
        "Sodium",
        "Manganese" ,
        "Hardness_ non-carbonate" ,
        "Acetate" ,
        "Phosphate" ,
        "Cl.Br" ,
        "Formate" ,
        "Thiosulfate")
a<-"Alkalinity"
s<-"Olivia"

QWgraphData2[is.na(QWgraphData2$Site_name),]
setPDF(layout = "landscape",basename = "qwprofiles3",multiplefiles = FALSE)
setLayout(width = c(1.5,1.5,1.5,1.5,1.5),height = 6)
plot(0,0)
for(s in c("Cromwell","Litchfield","Olivia","HFC")) {
  tmp.df<-QWgraphData2[QWgraphData2$Site_name==s&QWgraphData2$medium_cd=="WG",]
  for(a in moreWGgraphs){
    tmp.df2<-tmp.df[tmp.df$srsname==a,]
    
    with(tmp.df2,xyPlot(x =result_va ,y=MeanSampleDepth_ftBLS,yaxis.rev = TRUE,
                        caption = paste(tmp.df2$Site_name[1],tmp.df2$srsname[1], tmp.df2$parameter_units[1]),Plot=list(what="points")))
    with(tmp.df2,addXY(x =result_va ,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="lines")))
    
  }
}
dev.off()






#More graphs GW
unique(QWgraphData2$srsname[QWgraphData2$medium_cd=="WG"])
moreWGgraphs2<-c("Formate","Nitrite","Cl.Br","Oxygen","pH")
parm<-c("Calcium",
        "Silica",
        "Phosphorus",
        "Total dissolved solids",
        "d18O",
        "Bicarbonate" ,
        "Nitrate" ,
        "Tritium" ,
        "Magnesium" ,
        "Carbonate (CO3)" ,
        "Sulfate" ,
        "Alkalinity",
        "Ammonia and ammonium",
        "Temperature_ water",
        "Inorganic nitrogen (nitrate and nitrite)",
        "Oxygen",
        "Total hardness -- SDWA NPDWR",
        "Bromide" ,
        "Potassium" ,
        "Nitrite" ,
        "Carbon dioxide",
        "pH" ,
        "Fluoride",
        "Chloride",
        "Specific conductance",
        "Sodium_ percent total cations" ,
        "Iron",
        "dD",
        "Sodium",
        "Manganese" ,
        "Hardness_ non-carbonate" ,
        "Acetate" ,
        "Phosphate" ,
        "Cl.Br" ,
        "Formate" ,
        "Thiosulfate")
a<-"Formate"
s<-"Olivia"

QWgraphData2[is.na(QWgraphData2$Site_name),]
setPDF(layout = "landscape",basename = "qwprofiles4",multiplefiles = FALSE)
setLayout(width = c(1.5,1.5,1.5,1.5,1.5),height = 6)
plot(0,0)
for(s in c("Cromwell","Litchfield","Olivia","HFC")) {
  tmp.df<-QWgraphData2[QWgraphData2$Site_name==s&QWgraphData2$medium_cd=="WG",]
  for(a in moreWGgraphs2){
    tmp.df2<-tmp.df[tmp.df$srsname==a,]
    if(nrow(tmp.df2)>0){
      with(tmp.df2,xyPlot(x =result_va ,y=MeanSampleDepth_ftBLS,yaxis.rev = TRUE,
                          caption = paste(tmp.df2$Site_name[1],tmp.df2$srsname[1], tmp.df2$parameter_units[1]),Plot=list(what="points")))
      with(tmp.df2[grep(pattern = "USGS",tmp.df2$anl_ent_cd),],addXY(x =result_va ,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="lines")))
      with(tmp.df2[tmp.df2$anl_ent_cd=="Rick A. Knurr Ion Chrom Analytical",],
           addXY(x=result_va,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="points",color="red"),
                 with(tmp.df2[tmp.df2$anl_ent_cd=="University of Waterloo Ontario, Canada Environmental Isotope Laboratory",],
                      addXY(x=result_va,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="points",color="orange")))))
    } else {
      next
    }
    
    
  }
}
dev.off()





#More graphs GW
unique(QWgraphData2$srsname[QWgraphData2$medium_cd=="WG"])
moreWGgraphs3<-c("Phosphate","Phosphorus","Thiosulfate","Silica","Potassium")
parm<-c("Calcium",
        "Silica",
        "Phosphorus",
        "Total dissolved solids",
        "d18O",
        "Bicarbonate" ,
        "Nitrate" ,
        "Tritium" ,
        "Magnesium" ,
        "Carbonate (CO3)" ,
        "Sulfate" ,
        "Alkalinity",
        "Ammonia and ammonium",
        "Temperature_ water",
        "Inorganic nitrogen (nitrate and nitrite)",
        "Oxygen",
        "Total hardness -- SDWA NPDWR",
        "Bromide" ,
        "Potassium" ,
        "Nitrite" ,
        "Carbon dioxide",
        "pH" ,
        "Fluoride",
        "Chloride",
        "Specific conductance",
        "Sodium_ percent total cations" ,
        "Iron",
        "dD",
        "Sodium",
        "Manganese" ,
        "Hardness_ non-carbonate" ,
        "Acetate" ,
        "Phosphate" ,
        "Cl.Br" ,
        "Formate" ,
        "Thiosulfate")
a<-"Phosphate"
s<-"Olivia"

QWgraphData2[is.na(QWgraphData2$Site_name),]
setPDF(layout = "landscape",basename = "qwprofiles5",multiplefiles = FALSE)
setLayout(width = c(1.5,1.5,1.5,1.5,1.5),height = 6)
plot(0,0)
for(s in c("Cromwell","Litchfield","Olivia","HFC")) {
  tmp.df<-QWgraphData2[QWgraphData2$Site_name==s&QWgraphData2$medium_cd=="WG",]
  for(a in moreWGgraphs3){
    tmp.df2<-tmp.df[tmp.df$srsname==a,]
    if(nrow(tmp.df2)>0){
      with(tmp.df2,xyPlot(x =result_va ,y=MeanSampleDepth_ftBLS,yaxis.rev = TRUE,
                          caption = paste(tmp.df2$Site_name[1],tmp.df2$srsname[1], tmp.df2$parameter_units[1]),Plot=list(what="points")))
      with(tmp.df2[grep(pattern = "USGS",tmp.df2$anl_ent_cd),],addXY(x =result_va ,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="lines")))
      with(tmp.df2[tmp.df2$anl_ent_cd=="Rick A. Knurr Ion Chrom Analytical",],
           addXY(x=result_va,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="points",color="red")))
    } else {
      next
    }
    
    
  }
}
dev.off()



#More graphs GW
unique(QWgraphData2$srsname)
moreWGgraphs4<-c("Tritium","dD","d18O")
parm<-c("Calcium",
        "Silica",
        "Phosphorus",
        "Total dissolved solids",
        "d18O",
        "Bicarbonate" ,
        "Nitrate" ,
        "Tritium" ,
        "Magnesium" ,
        "Carbonate (CO3)" ,
        "Sulfate" ,
        "Alkalinity",
        "Ammonia and ammonium",
        "Temperature_ water",
        "Inorganic nitrogen (nitrate and nitrite)",
        "Oxygen",
        "Total hardness -- SDWA NPDWR",
        "Bromide" ,
        "Potassium" ,
        "Nitrite" ,
        "Carbon dioxide",
        "pH" ,
        "Fluoride",
        "Chloride",
        "Specific conductance",
        "Sodium_ percent total cations" ,
        "Iron",
        "dD",
        "Sodium",
        "Manganese" ,
        "Hardness_ non-carbonate" ,
        "Acetate" ,
        "Phosphate" ,
        "Cl.Br" ,
        "Formate" ,
        "Thiosulfate")
a<-"Tritium"
s<-"Olivia"

QWgraphData2[is.na(QWgraphData2$Site_name),]
setPDF(layout = "landscape",basename = "qwprofiles6",multiplefiles = FALSE)
setLayout(width = c(1.5,1.5,1.5,1.5,1.5),height = 6)
plot(0,0)
for(s in c("Cromwell","Litchfield","Olivia","HFC")) {
  tmp.df<-QWgraphData2[QWgraphData2$Site_name==s,]
  for(a in moreWGgraphs4){
    tmp.df2<-tmp.df[tmp.df$srsname==a,]
    if(nrow(tmp.df2)>0){
      with(tmp.df2[tmp.df2$medium_cd=="WG",],xyPlot(x =result_va ,y=MeanSampleDepth_ftBLS,yaxis.rev = TRUE,
                                                    caption = paste(tmp.df2$Site_name[1],tmp.df2$srsname[1], tmp.df2$parameter_units[1]),Plot=list(what="points")))
      with(tmp.df2[tmp.df2$medium_cd=="WG",],addXY(x =result_va ,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="lines")))
      with(tmp.df2[tmp.df2$medium_cd=="WI",],
           addXY(x=result_va,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="points",color="red")))
    } else {
      next
    }
    
    
  }
}
dev.off()







s
a

warnings()











#########################################
#Trying to make a simple graph
library(ggplot2)
g.all2

1

#Function found onlinge at stackoverflow
plotAllXYbyZ <- function(g.all2, x, y, z) {
  # to make sure all columns to be melted for ploting are numerical 
  g.all2[, (y):= lapply(.SD, function(x) {as.numeric(as.character(x))}), .SDcols = y]
  g.all2graph <- melt(g.all2, id = c(x,z), measure = y)
  ggplot(g.all2graph, aes_string(x = colnames(dt)[x], y = "value", colours = colnames(dt)[z])) +
    geom_line() + facet_wrap(~ variable)
}
plotAllXYbyZ(g.all2)



#These don't work either
na.omit(g.all2)
with(g.all2, plot("result_va", "site_no.x"))
with(g.all2,plot("Calcium","Magnesium"))

sapply(g.all2,class)
plotAllXYbyZ <- function(g.all2, MeanSampleDepth_ftBLS, result_va)
  g.all2[, (result_va):= lapply(.SD, function(MeanSampleDepth_ftBLS) {as.numeric(as.character(MeanSampleDepth_ftBLS))}), .SDcols = result_va]
g.all2graph <- melt(g.all2, id = c(MeanSampleDepth_ftBLS), measure = result_va)
ggplot(g.all2graph, aes_string(x = colnames(dt)[x], y = "value", colours = colnames(dt)[z])) +
  geom_line() + facet_wrap(~ variable)
}
plotAllXYbyZ(g.all2) 


plot(MeanSampleDepth_ftBLS, result_va)

install.packages("reshape")
library("reshape")

#Trying USGS R training graph (works)
#Filtering to get certain analytes from certain sites
Sodium <- filter(g.all2, srsname == "Sodium")
Chloride <- filter(g.all2, srsname == "Chloride")
SodiumOlivia <- filter(Sodium, Site_name == "Olivia")
ChlorideOlivia <- filter(Chloride, Site_name == "Olivia")
ChlorideOliviaWG <- filter(ChlorideOlivia, medium_cd == "WG")
ChlorideOliviaGW2 <- filter(ChlorideOliviaWG, meth_cd == "IC022")

#selecting what to graph 
SodiumOliviaSelect <- select(SodiumOlivia, MeanSampleDepth_ftBLS, result_va)
ChlorideOliviaSelect <- select(ChlorideOliviaGW2, MeanSampleDepth_ftBLS, result_va)

#Plotting
plot(SodiumOliviaSelect$result_va, SodiumOliviaSelect$MeanSampleDepth_ftBLS, pch=16, col="#FF5034",ylim=c(0,250),xlim = c(0,140))
points(ChlorideOliviaSelect$result_va, ChlorideOliviaSelect$MeanSampleDepth_ftBLS, pch=16, col="skyblue")



#ggplot2 plotting Sodium and Chloride with depth
library(ggplot2)
library(dplyr)

# aes() are the "aesthetics" info.  When you simply add the x and y
# that can seem a bit of a confusing term.  You also use aes() to 
# change color, shape, size etc. of some items
OChlorideDepth <- ggplot(data=ChlorideOliviaSelect, aes(x=result_va, y=MeanSampleDepth_ftBLS))
OChlorideDepth 

#Different syntax than you are used to
OChlorideDepth  + geom_point()

OChlorideDepth_scatter <- OChlorideDepth +
  labs(title="Olivia Chloride (mg/L) vs. Depth",
       x="Chloride (mg/L)", y="Mean Sample Depth (ft)")
OChlorideDepth_scatter

#Different syntax than you are used to
OChlorideDepth_scatter  + geom_point()

#changing xlim and ylim
OChlorideDepth_scatter + xlim(0, 50)


#trying this again (this is a simpler version of the ggplot2 code)
Ocl <- ggplot(ChlorideOliviaSelect, aes(x=result_va, y=MeanSampleDepth_ftBLS))+ geom_point()
Ocl
Ocl + xlim(0,50)+ylim(0,250)
#reversing y scale
Ocl + scale_y_reverse()

#This order allows titles and to reverse the y scale
Ocl_scatter <- Ocl + labs(title="Olivia Chloride (mg/L) vs. Mean Sample Depth (ft)", x="Chloride (mg/L)", y="Mean Sample Depth (ft)")
Ocl_scatter
Ocl_scatter + scale_y_reverse()


#Cannot make continous x and y unless all the values are numeric (now the above ggplot2 code should work)
sapply(ChlorideOliviaSelect, class)
ChlorideOliviaSelect$result_va<-as.numeric(ChlorideOliviaSelect$result_va)




