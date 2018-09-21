#Figuring out how to do the Chloride Bromide mass ratio vs. Chloride concentration
#Needs file names updated

#Cl/Br plots

library(smwrGraphs)
library(plyr)
library(reshape2)
library(dplyr)
library(ggplot2)

#This run is for all results, even with Br udner the detection limit
ClBrplot<-read.csv("P:/FPJ00/Phase2GeochemistryData/SiteCl_Na.csv")

names(ClBrplot)

ClBrplot<-dcast(ClBrplot,formula = Site_name+Piez_Nest+Order+Site_ID+medium_cd~srsname,value.var = "result_va")
sapply(ClBrplot,class)
ClBrplot %>% mutate_if(is.factor, as.character) -> ClBrplot


#How to select x and y data for smwrgraphs xyPlot
windows()
with(ClBrplot, xyPlot(Chloride, Cl.Br)) 
setPDF(layout = "portrait",basename = "Cl_Br")
setLayout(width = 6,height=6)
with(ClBrplot,plot(Chloride, Cl.Br,ylim=c(10,100000),xlim=c(0.1,1000000)))


#This works!
setPDF(layout = "portrait",basename = "ClBrplot")
setLayout(width = 6,height=4)
#with(ClBrplot,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(ClBrplot,colorPlot(Chloride, Cl.Br,color = Site_name,yaxis.range=c(10,100000),xaxis.range=c(0.1,1000000), yaxis.log = TRUE, xaxis.log = TRUE, ytitle = "Cl/Br Mass Ratio", xtitle ="Chloride concentration, mg/L"))
addExplanation(AA,where = "ul")


dev.off()




#This run is for results where Br is detected
ClBrplot<-read.csv("P:/FPJ00/Phase2GeochemistryData/SiteCl_Na.csv")

names(ClBrplot)
ClBrplot2<-filter(ClBrplot, srsname=="Chloride" | srsname=="Cl.Br" )

ClBrplot2<-filter(ClBrplot2, remark_cd=="0")


ClBrplot2<-dcast(ClBrplot2,formula = Site_name+Piez_Nest+Order+Site_ID+medium_cd~srsname,value.var = "result_va")
sapply(ClBrplot2,class)
ClBrplot2 %>% mutate_if(is.factor, as.character) -> ClBrplot2

ClBrplot2<-filter(ClBrplot2, Cl.Br>0)


#How to select x and y data for smwrgraphs xyPlot
windows()
with(ClBrplot2, xyPlot(Chloride, Cl.Br)) 
setPDF(layout = "portrait",basename = "Cl_Br")
setLayout(width = 6,height=6)
with(ClBrplot2,plot(Chloride, Cl.Br,ylim=c(10,100000),xlim=c(0.1,1000000)))


#This works!
setPDF(layout = "portrait",basename = "ClBrplot2")
setLayout(width = 6,height=4)
#with(ClBrplot2,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(ClBrplot2,colorPlot(Chloride, Cl.Br,color = Site_name,yaxis.range=c(10,100000),xaxis.range=c(0.1,1000000), yaxis.log = TRUE, xaxis.log = TRUE, ytitle = "Cl/Br Mass Ratio", xtitle ="Chloride concentration, mg/L"))
addExplanation(AA,where = "ul")


dev.off()



#This run is for Olivia
ClBrplot<-read.csv("P:/FPJ00/Phase2GeochemistryData/SiteCl_Na.csv")
ClBrplotOl<-filter(ClBrplot, Site_name=="Olivia")
names(ClBrplotOl)

ClBrplotOl<-dcast(ClBrplotOl,formula = Site_name+Piez_Nest+Order+Site_ID+medium_cd~srsname,value.var = "result_va")
sapply(ClBrplotOl,class)
ClBrplotOl %>% mutate_if(is.factor, as.character) -> ClBrplotOl


#How to select x and y data for smwrgraphs xyPlot
windows()
with(ClBrplotOl, xyPlot(Chloride, Cl.Br)) 
setPDF(layout = "portrait",basename = "Cl_Br")
setLayout(width = 6,height=6)
with(ClBrplotOl,plot(Chloride, Cl.Br,ylim=c(10,100000),xlim=c(0.1,1000000)))


#This works!
setPDF(layout = "portrait",basename = "ClBrplotOl")
setLayout(width = 6,height=4)
#with(ClBrplotOl,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(ClBrplotOl,colorPlot(Chloride, Cl.Br,color = Order,yaxis.range=c(10,100000),xaxis.range=c(0.1,1000000), yaxis.log = TRUE, xaxis.log = TRUE, ytitle = "Cl/Br Mass Ratio", xtitle ="Chloride concentration, mg/L"))
addExplanation(AA,where = "ul")


dev.off()




#This run is for HFC
ClBrplot<-read.csv("P:/FPJ00/Phase2GeochemistryData/SiteCl_Na.csv")
ClBrplotHFC<-filter(ClBrplot, Site_name=="HFC")
names(ClBrplotHFC)

ClBrplotHFC<-dcast(ClBrplotHFC,formula = Site_name+Piez_Nest+Order+Site_ID+medium_cd~srsname,value.var = "result_va")
sapply(ClBrplotHFC,class)
ClBrplotHFC %>% mutate_if(is.factor, as.character) -> ClBrplotHFC


#How to select x and y data for smwrgraphs xyPlot
windows()
with(ClBrplotHFC, xyPlot(Chloride, Cl.Br)) 
setPDF(layout = "portrait",basename = "Cl_Br")
setLayout(width = 6,height=6)
with(ClBrplotHFC,plot(Chloride, Cl.Br,ylim=c(10,100000),xlim=c(0.1,1000000)))


#This works!
setPDF(layout = "portrait",basename = "ClBrplotHFC")
setLayout(width = 6,height=4)
#with(ClBrplotHFC,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(ClBrplotHFC,colorPlot(Chloride, Cl.Br,color = Order,yaxis.range=c(10,100000),xaxis.range=c(0.1,1000000), yaxis.log = TRUE, xaxis.log = TRUE, ytitle = "Cl/Br Mass Ratio", xtitle ="Chloride concentration, mg/L"))
addExplanation(AA,where = "ul")


dev.off()



#This run is for Cromwell
ClBrplot<-read.csv("P:/FPJ00/Phase2GeochemistryData/SiteCl_Na.csv")
ClBrplotCW<-filter(ClBrplot, Site_name=="Cromwell")
names(ClBrplotCW)

ClBrplotCW<-dcast(ClBrplotCW,formula = Site_name+Piez_Nest+Order+Site_ID+medium_cd~srsname,value.var = "result_va")
sapply(ClBrplotCW,class)
ClBrplotCW %>% mutate_if(is.factor, as.character) -> ClBrplotCW


#How to select x and y data for smwrgraphs xyPlot
windows()
with(ClBrplotCW, xyPlot(Chloride, Cl.Br)) 
setPDF(layout = "portrait",basename = "Cl_Br")
setLayout(width = 6,height=6)
with(ClBrplotCW,plot(Chloride, Cl.Br,ylim=c(10,100000),xlim=c(0.1,1000000)))


#This works!
setPDF(layout = "portrait",basename = "ClBrplotCW")
setLayout(width = 6,height=4)
#with(ClBrplotCW,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(ClBrplotCW,colorPlot(Chloride, Cl.Br,color = Order,yaxis.range=c(10,100000),xaxis.range=c(0.1,1000000), yaxis.log = TRUE, xaxis.log = TRUE, ytitle = "Cl/Br Mass Ratio", xtitle ="Chloride concentration, mg/L"))
addExplanation(AA,where = "ul")


dev.off()



#This run is for Litchfield
ClBrplot<-read.csv("P:/FPJ00/Phase2GeochemistryData/SiteCl_Na.csv")
ClBrplotLF<-filter(ClBrplot, Site_name=="Litchfield")
names(ClBrplotLF)

ClBrplotLF<-dcast(ClBrplotLF,formula = Site_name+Piez_Nest+Order+Site_ID+medium_cd~srsname,value.var = "result_va")
sapply(ClBrplotLF,class)
ClBrplotLF %>% mutate_if(is.factor, as.character) -> ClBrplotLF


#How to select x and y data for smwrgraphs xyPlot
windows()
with(ClBrplotLF, xyPlot(Chloride, Cl.Br)) 
setPDF(layout = "portrait",basename = "Cl_Br")
setLayout(width = 6,height=6)
with(ClBrplotLF,plot(Chloride, Cl.Br,ylim=c(10,100000),xlim=c(0.1,1000000)))


#This works!
setPDF(layout = "portrait",basename = "ClBrplotLF")
setLayout(width = 6,height=4)
#with(ClBrplotLF,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(ClBrplotLF,colorPlot(Chloride, Cl.Br,color = Order,yaxis.range=c(10,100000),xaxis.range=c(0.1,1000000), yaxis.log = TRUE, xaxis.log = TRUE, ytitle = "Cl/Br Mass Ratio", xtitle ="Chloride concentration, mg/L"))
addExplanation(AA,where = "ul")


dev.off()


#This run is for Olivia and HFC run
ClBrplot<-read.csv("P:/FPJ00/Phase2GeochemistryData/SiteCl_Na.csv")
ClBrplotOlHFC<-filter(ClBrplot, Site_name=="Olivia" | Site_name=="HFC")
names(ClBrplotOlHFC)

ClBrplotOlHFC<-dcast(ClBrplotOlHFC,formula = Site_name+Piez_Nest+Order+Site_ID+medium_cd~srsname,value.var = "result_va")
sapply(ClBrplotOlHFC,class)
ClBrplotOlHFC %>% mutate_if(is.factor, as.character) -> ClBrplotOlHFC


#How to select x and y data for smwrgraphs xyPlot
windows()
with(ClBrplotOlHFC, xyPlot(Chloride, Cl.Br)) 
setPDF(layout = "portrait",basename = "Cl_Br")
setLayout(width = 6,height=6)
with(ClBrplotOlHFC,plot(Chloride, Cl.Br,ylim=c(10,100000),xlim=c(0.1,1000000)))


#This works!
setPDF(layout = "portrait",basename = "ClBrplotOlHFC")
setLayout(width = 6,height=4)
#with(ClBrplotOlHFC,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(ClBrplotOlHFC,colorPlot(Chloride, Cl.Br,color = Site_name,yaxis.range=c(10,100000),xaxis.range=c(0.1,1000000), yaxis.log = TRUE, xaxis.log = TRUE, ytitle = "Cl/Br Mass Ratio", xtitle ="Chloride concentration, mg/L"))
addExplanation(AA,where = "ul")


dev.off()




#This run is for Br vs. Cl/Br ratio
ClBrplot<-read.csv("P:/FPJ00/Phase2GeochemistryData/SiteCl_Na.csv")

names(ClBrplot)

ClBrplot<-dcast(ClBrplot,formula = Site_name+Piez_Nest+Order+Site_ID+medium_cd~srsname,value.var = "result_va")
sapply(ClBrplot,class)
ClBrplot %>% mutate_if(is.factor, as.character) -> ClBrplot


#How to select x and y data for smwrgraphs xyPlot
windows()
with(ClBrplot, xyPlot(Bromide, Cl.Br)) 
setPDF(layout = "portrait",basename = "Br_ClBr")
setLayout(width = 6,height=6)
with(ClBrplot,plot(Bromide, Cl.Br,ylim=c(10,100000),xlim=c(0.1,1000000)))


#This works!
setPDF(layout = "portrait",basename = "Br_ClBrplot")
setLayout(width = 6,height=5)
#with(ClBrplot,xyPlot(Sodium, Bromide,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(ClBrplot,colorPlot(Bromide, Cl.Br,color = Site_name,yaxis.range=c(10,100000),xaxis.range=c(0.001,1000000), yaxis.log = TRUE, xaxis.log = TRUE, ytitle = "Cl/Br Mass Ratio", xtitle ="Bromide concentration, mg/L"))
addExplanation(AA,where = "ul")


dev.off()


#This works!
setPDF(layout = "portrait",basename = "Br_ClBrplot2")
setLayout(width = 6,height=5)
#with(ClBrplot,xyPlot(Sodium, Bromide,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(ClBrplot,colorPlot(Bromide, Cl.Br,color = Site_name,yaxis.range=c(10,100000),xaxis.range=c(0.001,.5), yaxis.log = TRUE, ytitle = "Cl/Br Mass Ratio", xtitle ="Bromide concentration, mg/L"))
addExplanation(AA,where = "ul")


dev.off()



#This run is for results where Br is detected
ClBrplot<-read.csv("P:/FPJ00/Phase2GeochemistryData/SiteCl_Na.csv")

names(ClBrplot)
ClBrplot2<-filter(ClBrplot, srsname=="Bromide" | srsname=="Cl.Br" )

ClBrplot2<-filter(ClBrplot2, remark_cd=="0")


ClBrplot2<-dcast(ClBrplot2,formula = Site_name+Piez_Nest+Order+Site_ID+medium_cd~srsname,value.var = "result_va")
sapply(ClBrplot2,class)
ClBrplot2 %>% mutate_if(is.factor, as.character) -> ClBrplot2

ClBrplot2<-filter(ClBrplot2, Cl.Br>0)


#How to select x and y data for smwrgraphs xyPlot
windows()
with(ClBrplot2, xyPlot(Bromide, Cl.Br)) 
setPDF(layout = "portrait",basename = "Cl_Br")
setLayout(width = 6,height=6)
with(ClBrplot2,plot(Bromide, Cl.Br,ylim=c(10,100000),xlim=c(0.1,1000000)))


#This works!
setPDF(layout = "portrait",basename = "ClBrplot3")
setLayout(width = 6,height=4)
#with(ClBrplot2,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(ClBrplot2,colorPlot(Bromide, Cl.Br,color = Site_name,yaxis.range=c(10,100000),xaxis.range=c(0.001,0.5), yaxis.log = TRUE, ytitle = "Cl/Br Mass Ratio", xtitle ="Bromide concentration, mg/L"))
addExplanation(AA,where = "ul")


dev.off()

