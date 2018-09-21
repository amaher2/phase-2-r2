#Code from MakingGraphs.R that works
#First have these packages
#Need to update file names

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

#Filtering nitrate data
CW1N<-filter(Nitrate, Piez_Nest == "CW_1")
CW2N<-filter(Nitrate, Piez_Nest == "CW_2")
HFCN<-filter(Nitrate, Piez_Nest == "HFC_1")
LF1N<-filter(Nitrate, Piez_Nest == "LF_1")
LF2N<-filter(Nitrate, Piez_Nest == "LF_2")
OlN<-filter(Nitrate, Piez_Nest == "Ol_1")

#Filtering for Knurr lab
CW1NitrateKnurr<-filter(CW1N, anl_ent_cd == "Rick A. Knurr Ion Chrom Analytical")
CW2NitrateKnurr<-filter(CW2N, anl_ent_cd == "Rick A. Knurr Ion Chrom Analytical")
HFCNitrateKnurr<-filter(HFCN, anl_ent_cd == "Rick A. Knurr Ion Chrom Analytical")
LF1NitrateKnurr<-filter(LF1N, anl_ent_cd == "Rick A. Knurr Ion Chrom Analytical")
LF2NitrateKnurr<-filter(LF2N, anl_ent_cd == "Rick A. Knurr Ion Chrom Analytical")
OlNitrateKnurr<-filter(OlN, anl_ent_cd == "Rick A. Knurr Ion Chrom Analytical")

#Filtering for USGS water quality 
CW1NitrateUSGS<-filter(CW1N, Source == "USGS water quality data")
CW2NitrateUSGS<-filter(CW2N, Source == "USGS water quality data")
HFCNitrateUSGS<-filter(HFCN, Source == "USGS water quality data")
LF1NitrateUSGS<-filter(LF1N, Source == "USGS water quality data")
LF2NitrateUSGS<-filter(LF2N, Source == "USGS water quality data")
OlNitrateUSGS<-filter(OlN, Source == "USGS water quality data")

#There is Nitrate-N and Nitrate_Nitrate in USGS data. Knurr's data only has Nitrate_N.
#Need to filter for Nitrate-N for USGS data
CW1Nitrate_NUSGS<-filter(CW1NitrateUSGS, parameter_units == "mg/l as N")
CW2Nitrate_NUSGS<-filter(CW2NitrateUSGS, parameter_units == "mg/l as N")
HFCNitrateUSGS_N<-filter(HFCNitrateUSGS, parameter_units == "mg/l as N")
LF1NitrateUSGS_N<-filter(LF1NitrateUSGS, parameter_units == "mg/l as N")
LF2NitrateUSGS_N<-filter(LF2NitrateUSGS, parameter_units == "mg/l as N")
OlNitrateUSGS_N<-filter(OlNitrateUSGS, parameter_units == "mg/l as N")

LF1NitrateKnurrGW<-filter(LF1NitrateKnurr, medium_cd == "WG")
LF1NitrateKnurrGI<-filter(LF1NitrateKnurr, medium_cd == "WI")



#Cannot make continous x and y unless all the values are numeric (now the ggplot2 code should work)
sapply(CW1Nitrate_NUSGS, class)
CW1Nitrate_NUSGS$result_va<-as.numeric(CW1Nitrate_NUSGS$result_va)






#CW1 Nitrate_N USGS data 
#How to select x and y data for smwrgraphs xyPlot
with(CW1Nitrate_NUSGS, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(CW1Nitrate_NUSGS,class)

setPDF(layout = "landscape",basename = "CW1Nitrate_NUSGS",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-10
ymin<-0
ymax<-350

#xmax<-4
#xmax<-if(xmax<1){
#  round(xmax,1)
#}else if (xmax>1&xmax<10){
#round(xmax,0)
#} else {
#  round(xmax,-2)
#}



NitrateUSGSCW1<-with(CW1Nitrate_NUSGS[CW1Nitrate_NUSGS$result_va>0.01,], 
                     xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=3, plot=list(what="lines"), 
                            ytitle = "Depth below land surface, in feet",
                            xtitle=paste(CW1Nitrate_NUSGS$srsname[1],", in ", CW1Nitrate_NUSGS$parameter_units[1],sep=""),
                            yaxis.range = c(ymin,ymax),
                            xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=CW1Nitrate_NUSGS$result_va[CW1Nitrate_NUSGS$remark_cd=="<"],y=-1*CW1Nitrate_NUSGS$MeanSampleDepth_ftBLS[CW1Nitrate_NUSGS$result_va==0.01],"ND")
mtext(text = paste("GW"," ",  CW1Nitrate_NUSGS$srsname[1]," at ",CW1Nitrate_NUSGS$Site_name[1],sep="", " ", CW1Nitrate_NUSGS$Piez_Nest[1]),side=3,line = 1)
dev.off()






#Nitrate_N Knurr's Lab
#How to select x and y data for smwrgraphs xyPlot
with(CW1NitrateKnurr, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(CW1NitrateKnurr,class)

setPDF(layout = "landscape",basename = "CW1NitrateKnurr",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-10
ymin<-0
ymax<-350

#xmax<-4
#xmax<-if(xmax<1){
#  round(xmax,1)
#}else if (xmax>1&xmax<10){
#round(xmax,0)
#} else {
#  round(xmax,-2)
#}
NitrateKnurrCW1<-with(CW1NitrateKnurr[CW1NitrateKnurr$result_va>0,], 
                      xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=3, plot=list(what="lines"), 
                             ytitle = "Depth below land surface, in feet",
                             xtitle=paste(CW1NitrateKnurr$srsname[1],", in ", CW1NitrateKnurr$parameter_units[1],sep=""),
                             yaxis.range = c(ymin,ymax),
                             xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=CCW1NitrateKnurr$result_va[CW1NitrateKnurr$remark_cd=="<"],y=-1*CW1NitrateKnurr$MeanSampleDepth_ftBLS[CW1NitrateKnurr$result_va],"ND")
mtext(text = paste("GW"," ",CW1NitrateKnurr$srsname[1]," at ",CW1NitrateKnurr$Site_name[1],sep="", " ", CW1NitrateKnurr$Piez_Nest[1]),side=3,line = 1)
dev.off()















#CW2 Nitrate_N USGS data 
#How to select x and y data for smwrgraphs xyPlot
with(CW2Nitrate_NUSGS, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(CW2Nitrate_NUSGS,class)

setPDF(layout = "landscape",basename = "CW2Nitrate_NUSGS",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-10
ymin<-0
ymax<-150

#xmax<-4
#xmax<-if(xmax<1){
#  round(xmax,1)
#}else if (xmax>1&xmax<10){
#round(xmax,0)
#} else {
#  round(xmax,-2)
#}
NitrateUSGSCW2<-with(CW2Nitrate_NUSGS[CW2Nitrate_NUSGS$result_va>0.01,], 
                     xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=3, plot=list(what="lines"), 
                            ytitle = "Depth below land surface, in feet",
                            xtitle=paste(CW2Nitrate_NUSGS$srsname[1],", in ", CW2Nitrate_NUSGS$parameter_units[1],sep=""),
                            yaxis.range = c(ymin,ymax),
                            xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=CW2Nitrate_NUSGS$result_va[CW2Nitrate_NUSGS$remark_cd=="<"],y=-1*CW2Nitrate_NUSGS$MeanSampleDepth_ftBLS[CW2Nitrate_NUSGS$result_va==0.01],"ND")
mtext(text = paste("GW"," ",CW2Nitrate_NUSGS$srsname[1]," at ",CW2Nitrate_NUSGS$Site_name[1],sep="", " ", CW2Nitrate_NUSGS$Piez_Nest[1]),side=3,line = 1)
dev.off()












#CW2 Nitrate_N Knurr data 
#How to select x and y data for smwrgraphs xyPlot
with(CW2NitrateKnurr, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(CW2NitrateKnurr,class)

setPDF(layout = "landscape",basename = "CW2NitrateKnurr",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-10
ymin<-0
ymax<-150

#xmax<-4
#xmax<-if(xmax<1){
#  round(xmax,1)
#}else if (xmax>1&xmax<10){
#round(xmax,0)
#} else {
#  round(xmax,-2)
#}
NitrateCW2Knurr<-with(CW2NitrateKnurr[CW2NitrateKnurr$result_va>0.004,], 
                      xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=3, plot=list(what="lines"), 
                             ytitle = "Depth below land surface, in feet",
                             xtitle=paste(CW2NitrateKnurr$srsname[1],", in ", CW2NitrateKnurr$parameter_units[1],sep=""),
                             yaxis.range = c(ymin,ymax),
                             xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=CW2NitrateKnurr$result_va[CW2NitrateKnurr$remark_cd=="<"],y=-1*CW2NitrateKnurr$MeanSampleDepth_ftBLS[CW2NitrateKnurr$result_va==0.004],"ND")
mtext(text = paste("GW"," ",CW2NitrateKnurr$srsname[1]," at ",CW2NitrateKnurr$Site_name[1],sep="", " ", CW2NitrateKnurr$Piez_Nest[1]),side=3,line = 1)
dev.off()



















#HFC Nitrate_N USGS data 
#How to select x and y data for smwrgraphs xyPlot
with(HFCNitrateUSGS_N, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(HFCNitrateUSGS_N,class)

setPDF(layout = "landscape",basename = "HFCNitrateUSGS_N",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-10
ymin<-0
ymax<-250

#xmax<-4
#xmax<-if(xmax<1){
#  round(xmax,1)
#}else if (xmax>1&xmax<10){
#round(xmax,0)
#} else {
#  round(xmax,-2)
#}



NitrateHFCUSGS_N<-with(HFCNitrateUSGS_N[HFCNitrateUSGS_N$result_va>0.01,], 
                       xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=3, plot=list(what="lines"), 
                              ytitle = "Depth below land surface, in feet",
                              xtitle=paste(HFCNitrateUSGS_N$srsname[1],", in ", HFCNitrateUSGS_N$parameter_units[1],sep=""),
                              yaxis.range = c(ymin,ymax),
                              xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=HFCNitrateUSGS_N$result_va[HFCNitrateUSGS_N$remark_cd=="<"],y=-1*HFCNitrateUSGS_N$MeanSampleDepth_ftBLS[HFCNitrateUSGS_N$result_va==0.01],"ND")
mtext(text = paste("GW"," ",HFCNitrateUSGS_N$srsname[1]," at ",HFCNitrateUSGS_N$Site_name[1],sep=""),side=3,line = 1)
dev.off()








#HFC Nitrate_N Knurr data 
#How to select x and y data for smwrgraphs xyPlot
with(HFCNitrateKnurr, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(HFCNitrateKnurr,class)

setPDF(layout = "landscape",basename = "HFCNitrateKnurr",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-10
ymin<-0
ymax<-200

#xmax<-4
#xmax<-if(xmax<1){
#  round(xmax,1)
#}else if (xmax>1&xmax<10){
#round(xmax,0)
#} else {
#  round(xmax,-2)
#}
NitrateHFCKnurr<-with(HFCNitrateKnurr[HFCNitrateKnurr$result_va>0.004,], 
                      xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=3, plot=list(what="lines"), 
                             ytitle = "Depth below land surface, in feet",
                             xtitle=paste(HFCNitrateKnurr$srsname[1],", in ", HFCNitrateKnurr$parameter_units[1],sep=""),
                             yaxis.range = c(ymin,ymax),
                             xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=HFCNitrateKnurr$result_va[HFCNitrateKnurr$remark_cd=="<"],y=-1*HFCNitrateKnurr$MeanSampleDepth_ftBLS[HFCNitrateKnurr$result_va==0.004],"ND")
mtext(text = paste("PW", " ", HFCNitrateKnurr$srsname[1]," at ",HFCNitrateKnurr$Site_name[1],sep=""),side=3,line = 1)
dev.off()













#LF1 GW Nitrate_N Knurr data 
#How to select x and y data for smwrgraphs xyPlot
with(LF1NitrateKnurrGW, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(LF1NitrateKnurrGW,class)

setPDF(layout = "landscape",basename = "LF1NitrateKnurrGW",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-10
ymin<-0
ymax<-150

#xmax<-4
#xmax<-if(xmax<1){
#  round(xmax,1)
#}else if (xmax>1&xmax<10){
#round(xmax,0)
#} else {
#  round(xmax,-2)
#}



NitrateLF1KnurrGW<-with(LF1NitrateKnurrGW[LF1NitrateKnurrGW$result_va>0.004,], 
                        xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=3, plot=list(what="lines"), 
                               ytitle = "Depth below land surface, in feet",
                               xtitle=paste(LF1NitrateKnurrGW$srsname[1],", in ", LF1NitrateKnurrGW$parameter_units[1],sep=""),
                               yaxis.range = c(ymin,ymax),
                               xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=LF1NitrateKnurrGW$result_va[LF1NitrateKnurrGW$remark_cd=="<"],y=-1*LF1NitrateKnurrGW$MeanSampleDepth_ftBLS[LF1NitrateKnurrGW$result_va==0.004],"ND")
mtext(text = paste("GW"," ",  LF1NitrateKnurrGW$srsname[1]," at ",LF1NitrateKnurrGW$Site_name[1],sep="", " ", LF1NitrateKnurrGW$Piez_Nest[1]),side=3,line = 1)
dev.off()














#LF2 GW Nitrate_N USGS data 
#How to select x and y data for smwrgraphs xyPlot
with(LF1NitrateKnurrGI, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(LF1NitrateKnurrGI,class)

setPDF(layout = "landscape",basename = "LF1NitrateKnurrGI",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-10
ymin<-0
ymax<-150

#xmax<-4
#xmax<-if(xmax<1){
#  round(xmax,1)
#}else if (xmax>1&xmax<10){
#round(xmax,0)
#} else {
#  round(xmax,-2)
#}



NitrateLF1KnurrGI<-with(LF1NitrateKnurrGI[LF1NitrateKnurrGI$result_va>0.004,], 
                        xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=3, plot=list(what="lines"), 
                               ytitle = "Depth below land surface, in feet",
                               xtitle=paste(LF1NitrateKnurrGI$srsname[1],", in ", LF1NitrateKnurrGI$parameter_units[1],sep=""),
                               yaxis.range = c(ymin,ymax),
                               xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=LF1NitrateKnurrGI$result_va[LF1NitrateKnurrGI$remark_cd=="<"],y=-1*LF1NitrateKnurrGI$MeanSampleDepth_ftBLS[LF1NitrateKnurrGI$result_va==0.004],"ND")
mtext(text = paste("GI"," ",  LF1NitrateKnurrGI$srsname[1]," at ",LF1NitrateKnurrGI$Site_name[1],sep="", " ", LF1NitrateKnurrGI$Piez_Nest[1]),side=3,line = 1)
dev.off()

































#LF2 Nitrate_N USGS data 
#How to select x and y data for smwrgraphs xyPlot
with(LF2NitrateUSGS_N, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(LF2NitrateUSGS_N,class)

setPDF(layout = "landscape",basename = "LF2NitrateUSGS_N",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-10
ymin<-0
ymax<-200

#xmax<-4
#xmax<-if(xmax<1){
#  round(xmax,1)
#}else if (xmax>1&xmax<10){
#round(xmax,0)
#} else {
#  round(xmax,-2)
#}



NitrateLF2USGS_N<-with(LF2NitrateUSGS_N[LF2NitrateUSGS_N$result_va>0.01,], 
                       xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=3, plot=list(what="lines"), 
                              ytitle = "Depth below land surface, in feet",
                              xtitle=paste(LF2NitrateUSGS_N$srsname[1],", in ", LF2NitrateUSGS_N$parameter_units[1],sep=""),
                              yaxis.range = c(ymin,ymax),
                              xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=LF2NitrateUSGS_N$result_va[LF2NitrateUSGS_N$remark_cd=="<"],y=-1*LF2NitrateUSGS_N$MeanSampleDepth_ftBLS[LF2NitrateUSGS_N$result_va==0.01],"ND")
mtext(text = paste("GW"," ",LF2NitrateUSGS_N$srsname[1]," at ",LF2NitrateUSGS_N$Site_name[1],sep="", " ", LF2NitrateUSGS_N$Piez_Nest[1]),side=3,line = 1)
dev.off()
