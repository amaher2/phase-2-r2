#Na and Cl plots
#Nees file names updated

library(smwrGraphs)
library(plyr)
library(reshape2)
library(dplyr)
library(ggplot2)

NaCl<-read.csv("P:/FPJ00/Phase2GeochemistryData/SiteCl_Na.csv")

names(NaCl)
NaCl<-dcast(NaCl,formula = Site_name+Piez_Nest+Order+Site_ID+medium_cd~srsname,value.var = "result_va_mol")
sapply(NaCl,class)
NaCl %>% mutate_if(is.factor, as.character) -> NaCl


#How to select x and y data for smwrgraphs xyPlot
windows()
with(NaCl, xyPlot(Sodium, Chloride))
setPDF(layout = "portrait",basename = "NaCl")
setLayout(width = 4,height=4)
with(NaCl,plot(Sodium, Chloride,asp=1,ylim=c(0,0.006),xlim=c(0,0.006)))
abline(a = c(0,1))

#This works!
setPDF(layout = "portrait",basename = "NaCl")
setLayout(width = 4,height=4)
#with(NaCl,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(NaCl,colorPlot(Sodium, Chloride,color = Site_name,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006), ytitle = "Chloride (Molarity)", xtitle ="Sodium (Molarity)"))
abline(a = c(0,1))
addExplanation(AA,where = "ul")
text(x=0.004,y=0.004,labels = "1:1 line",pos = 3,offset = .2,srt=45)

dev.off()


#Need just one site!
NaClOlivia<-filter(NaCl, Site_name=="Olivia")
setPDF(layout = "portrait",basename = "NaClOlivia")
setLayout(width = 4,height=4)

AA2<-with(NaClOlivia,colorPlot(Sodium, Chloride,color = Order,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006), ytitle = "Chloride (Molarity)", xtitle ="Sodium (Molarity)"))
abline(a = c(0,1))
addExplanation(AA2,where = "ul")
text(x=0.004,y=0.004,labels = "1:1 line",pos = 3,offset = .2,srt=45)

dev.off()



#Need just one site!
NaClHFC<-filter(NaCl, Site_name=="HFC")
setPDF(layout = "portrait",basename = "NaClHFC")
setLayout(width = 4,height=4)

AA2<-with(NaClHFC,colorPlot(Sodium, Chloride,color = Order,yaxis.range=c(0,0.0045),xaxis.range=c(0,0.0045), ytitle = "Chloride (Molarity)", xtitle ="Sodium (Molarity)"))
abline(a = c(0,1))
addExplanation(AA2,where = "ul")
text(x=0.004,y=0.004,labels = "1:1 line",pos = 3,offset = .2,srt=45)

dev.off()


#Need just one site!
NaClCW<-filter(NaCl, Site_name=="Cromwell")
setPDF(layout = "portrait",basename = "NaClCW")
setLayout(width = 4,height=4)

AA3<-with(NaClCW,colorPlot(Sodium, Chloride,color = Order,yaxis.range=c(0,0.003),xaxis.range=c(0,0.003), ytitle = "Chloride (Molarity)", xtitle ="Sodium (Molarity)"))
abline(a = c(0,1))
addExplanation(AA3,where = "ul")
text(x=0.004,y=0.004,labels = "1:1 line",pos = 3,offset = .2,srt=45)

dev.off()



#Need just one site!
NaClLF<-filter(NaCl, Site_name=="Litchfield")
setPDF(layout = "portrait",basename = "NaClLF")
setLayout(width = 4,height=4)

AA3<-with(NaClLF,colorPlot(Sodium, Chloride,color = Order,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006), ytitle = "Chloride (Molarity)", xtitle ="Sodium (Molarity)"))
abline(a = c(0,1))
addExplanation(AA3,where = "ul")
text(x=0.004,y=0.004,labels = "1:1 line",pos = 3,offset = .2,srt=45)

dev.off()


# Graphing chloride bromide relationship
NaCl<-read.csv("P:/FPJ00/Phase2GeochemistryData/SiteCl_Na.csv")

names(NaCl)
NaCl<-dcast(NaCl,formula = Site_name+Piez_Nest+Order+Site_ID+medium_cd~srsname,value.var = "result_va_mol")
sapply(NaCl,class)
NaCl %>% mutate_if(is.factor, as.character) -> NaCl


#How to select x and y data for smwrgraphs xyPlot
windows()
with(NaCl, xyPlot(Sodium, Chloride))
setPDF(layout = "portrait",basename = "NaCl")
setLayout(width = 4,height=4)
with(NaCl,plot(Sodium, Chloride,asp=1,ylim=c(0,0.006),xlim=c(0,0.006)))
abline(a = c(0,1))

#This works!
setPDF(layout = "portrait",basename = "Cl_vs_Br_mol")
setLayout(width = 4,height=4)
#with(NaCl,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(NaCl,colorPlot(Bromide, Chloride,color = Site_name,yaxis.range=c(0,0.002),xaxis.range=c(0,0.0002), ytitle = "Chloride (Molarity)", xtitle ="bromide (Molarity)"))
addExplanation(AA,where = "ur")

dev.off()




#Olivia
Br_Cl_Olivia<-filter(NaCl, Site_name=="Olivia")
setPDF(layout = "portrait",basename = "Br_vs_Cl_Olivia")
setLayout(width = 4,height=4)

#This works!
setPDF(layout = "portrait",basename = "Olivia_Cl_vs_Br_mol")
setLayout(width = 4,height=4)
#with(NaCl,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(Br_Cl_Olivia,colorPlot(Bromide, Chloride,color = Order,yaxis.range=c(0,0.0015),xaxis.range=c(0,0.000004), ytitle = "Chloride (Molarity)", xtitle ="bromide (Molarity)"))
addExplanation(AA,where = "ur")

dev.off()

#HFC
Br_Cl_HFC<-filter(NaCl, Site_name=="HFC")
setPDF(layout = "portrait",basename = "Br_vs_HFC")
setLayout(width = 4,height=4)

#This works!
setPDF(layout = "portrait",basename = "HFC_Cl_vs_Br_mol")
setLayout(width = 4,height=4)
#with(NaCl,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(Br_Cl_HFC,colorPlot(Bromide, Chloride,color = Order,yaxis.range=c(0,0.00005),xaxis.range=c(0,0.0000003), ytitle = "Chloride (Molarity)", xtitle ="bromide (Molarity)"))
addExplanation(AA,where = "ur")

dev.off()


#Litchfield
Br_Cl_LF<-filter(NaCl, Site_name=="Litchfield")
setPDF(layout = "portrait",basename = "Br_vs_Cl_LF")
setLayout(width = 4,height=4)

#This works!
setPDF(layout = "portrait",basename = "LF_Cl_vs_Br_mol")
setLayout(width = 4,height=4)
#with(NaCl,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(Br_Cl_LF,colorPlot(Bromide, Chloride,color = Order,yaxis.range=c(0,0.002),xaxis.range=c(0,0.0002), ytitle = "Chloride (Molarity)", xtitle ="bromide (Molarity)"))
addExplanation(AA,where = "ur")

dev.off()


#Cromwell
Br_Cl_CW<-filter(NaCl, Site_name=="Cromwell")
setPDF(layout = "portrait",basename = "CW_vs_Cl_LF")
setLayout(width = 4,height=4)

#This works!
setPDF(layout = "portrait",basename = "CW_Cl_vs_Br_mol")
setLayout(width = 4,height=4)
#with(NaCl,xyPlot(Sodium, Chloride,yaxis.range=c(0,0.006),xaxis.range=c(0,0.006)))
AA<-with(Br_Cl_CW,colorPlot(Bromide, Chloride,color = Order,yaxis.range=c(0,0.002),xaxis.range=c(0,0.0000007), ytitle = "Chloride (Molarity)", xtitle ="bromide (Molarity)"))
addExplanation(AA,where = "ur")

dev.off()


#Doesn't work!
sapply(NaCl,class)
NaCl
setPDF(layout = "landscape",basename = "NaCl",multiplefiles = FALSE)
setLayout(width = 6,height = 6)

xmin<-0
xmax<-0.06
ymin<-0
ymax<-0.06


unique(NaCl$Sodium)
setPDF(layout = "portrait",basename = "NaCl")
setLayout(width = 6,height = 6)
NN<-with(NaCl, 
         xyPlot(Sodium, Chloride), 
         ytitle = "Chloride (Molarity)",
         xtitle= "Sodium (Molarity)",
         yaxis.range = c(ymin,ymax),
         xaxis.range=c(xmin,xmax),
         Plot=list(name=Site_name,color=setColor(Site_name)))
addExplanation(NN,where="ul",title="Explanation")
dev.off()

?unique
