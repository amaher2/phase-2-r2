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

#Cannot make continous x and y unless all the values are numeric (now the ggplot2 code should work)
sapply(HFC3H, class)
HFC3H$result_va<-as.numeric(HFC3H$result_va)

#Example
vignette(topic="LineScatter", package="smwrGraphs")


#How to select x and y data for smwrgraphs xyPlot
with(CW13H, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(CW13H,class)

setPDF(layout = "landscape",basename = "CW13H",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-8
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



TritiumCW1<-with(CW13H[CW13H$result_va>0.8,], 
                 xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=3, plot=list(what="lines"), 
                        ytitle = "Depth below land surface, in feet",
                        xtitle=paste(CW13H$srsname[1],", in ", CW13H$parameter_units[1],sep=""),
                        yaxis.range = c(ymin,ymax),
                        xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=CW13H$result_va[CW13H$remark_cd=="<"],y=-1*CW13H$MeanSampleDepth_ftBLS[CW13H$result_va==0.8],"ND")
mtext(text = paste(CW13H$srsname[1]," at ",CW13H$Site_name[1],sep=""),side=3,line = 1)
dev.off()





#How to select x and y data for smwrgraphs xyPlot
with(CW23H, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(CW23H,class)

setPDF(layout = "landscape",basename = "CW23H",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-8
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



TritiumCW2<-with(CW23H[CW23H$result_va>0.8,], 
                 xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=3, plot=list(what="lines"), 
                        ytitle = "Depth below land surface, in feet",
                        xtitle=paste(CW23H$srsname[1],", in ", CW23H$parameter_units[1],sep=""),
                        yaxis.range = c(ymin,ymax),
                        xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=CW23H$result_va[CW23H$remark_cd=="<"],y=-1*CW23H$MeanSampleDepth_ftBLS[CW23H$result_va==0.8],"ND")
mtext(text = paste(CW13H$srsname[1]," at ",CW23H$Site_name[1],sep="", " ", CW23H$Piez_Nest[1]),side=3,line = 1)
dev.off()







#How to select x and y data for smwrgraphs xyPlot
with(HFC3H, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(HFC3H,class)

setPDF(layout = "landscape",basename = "HFC3H",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-25
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



TritiumHFC<-with(HFC3H[HFC3H$result_va>0.8,], 
                 xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=3, plot=list(what="lines"), 
                        ytitle = "Depth below land surface, in feet",
                        xtitle=paste(HFC3H$srsname[1],", in ", HFC3H$parameter_units[1],sep=""),
                        yaxis.range = c(ymin,ymax),
                        xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=HFC3H$result_va[HFC3H$remark_cd=="<"],y=-1*HFC3H$MeanSampleDepth_ftBLS[HFC3H$result_va==0.8],"ND")
mtext(text = paste(HFC3H$srsname[1]," at ",HFC3H$Site_name[1],sep=""),side=3,line = 1)
dev.off()







#How to select x and y data for smwrgraphs xyPlot
with(LF13H, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(LF13H,class)

setPDF(layout = "landscape",basename = "LF13H",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-20
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



TritiumLF1<-with(LF13H[LF13H$result_va>0.8,], 
                 xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=5, plot=list(what="lines"), 
                        ytitle = "Depth below land surface, in feet",
                        xtitle=paste(LF13H$srsname[1],", in ", LF13H$parameter_units[1],sep=""),
                        yaxis.range = c(ymin,ymax),
                        xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=LF13H$result_va[LF13H$remark_cd=="<"],y=-1*LF13H$MeanSampleDepth_ftBLS[LF13H$result_va==0.8],"ND")
mtext(text = paste(LF13H$srsname[1]," at ",LF13H$Site_name[1],sep="", " ", LF13H$Piez_Nest[1]),side=3,line = 1)
dev.off()





#How to select x and y data for smwrgraphs xyPlot
with(LF23H, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(LF23H,class)

setPDF(layout = "landscape",basename = "LF23H",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-20
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



TritiumLF2<-with(LF23H[LF23H$result_va>0.8,], 
                 xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=5, plot=list(what="lines"), 
                        ytitle = "Depth below land surface, in feet",
                        xtitle=paste(LF23H$srsname[1],", in ", LF23H$parameter_units[1],sep=""),
                        yaxis.range = c(ymin,ymax),
                        xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=LF23H$result_va[LF23H$remark_cd=="<"],y=-1*LF23H$MeanSampleDepth_ftBLS[LF23H$result_va==0.8],"ND")
mtext(text = paste(LF23H$srsname[1]," at ",LF23H$Site_name[1],sep="", " ", LF23H$Piez_Nest[1]),side=3,line = 1)
dev.off()










#How to select x and y data for smwrgraphs xyPlot
with(Ol3H, xyPlot(result_va, MeanSampleDepth_ftBLS))
sapply(Ol3H,class)

setPDF(layout = "landscape",basename = "Ol3H",multiplefiles = FALSE)
setLayout(width = 2,height = 6)

xmin<-0
xmax<-20
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



TritiumOl<-with(Ol3H[Ol3H$result_va>0.8,], 
                xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,xlabels.rotate=TRUE,xlabels=5, plot=list(what="lines"), 
                       ytitle = "Depth below land surface, in feet",
                       xtitle=paste(Ol3H$srsname[1],", in ", Ol3H$parameter_units[1],sep=""),
                       yaxis.range = c(ymin,ymax),
                       xaxis.range=c(xmin,xmax)))
#TritiumOlivia<-with(OliviaWG3H[OliviaWG3H$result_va==0.8,],addXY(x = as.numeric(result_va), y=MeanSampleDepth_ftBLS*-1,
#                                                  Plot=list(what="points",symbol="circle",filled=FALSE)))


#TritiumOlivia<-with(OliviaWG3H, xyPlot(as.numeric(result_va), MeanSampleDepth_ftBLS,yaxis.rev = TRUE,ytitle = "Depth below land surace, in feet",
#                                       xtitle="Tritium, in tritium units"))

#abline(v=0.8,col="gray",lty=2)
legend("bottomright",legend = c("ND = non-detect"," "),cex=.75,bty = "n")
text(x=Ol3H$result_va[Ol3H$remark_cd=="<"],y=-1*Ol3H$MeanSampleDepth_ftBLS[Ol3H$result_va==0.8],"ND")
mtext(text = paste(Ol3H$srsname[1]," at ",Ol3H$Site_name[1],sep=""),side=3,line = 1)
dev.off()

